## Read read data using the new Epidata XML format into R

## Copyright (C) 2011 David Whiting

## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of the
## License, or (at your option) any later version.

## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA


unlink("STATUS.LOG")



status.log <- function(x) {
  ## Purpose: Simple logging mechanism. Can be useful for detecting bottlenecks etc.
  ## ----------------------------------------------------------------------
  ## Arguments: a message to be recorded in the log
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:27
  right.now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S") 
  cat(paste(right.now, x, "\n"), file = "STATUS.LOG", append = TRUE)
} 



extract.epidata.records <- function(rec, fields) {
  ## Purpose: Extract records from xml structure, allowing for missing fields.
  ## ----------------------------------------------------------------------
  ## Arguments: rec: a single record
  ## fields: a vector of field names (probably from the info table)
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 19:59
  dd <- xmlAttrs(rec)
  names.of.missing.fields <- fields[!fields %in% names(dd)]
  if (length(names.of.missing.fields)) {
    num.missing.flds <- length(names.of.missing.fields)
    missing.flds <- rep(NA, num.missing.flds)
    names(missing.flds) <- names.of.missing.fields
    dd <- c(dd, missing.flds) 
  }
  ## Sort the fields so that they are all in the same order.
  dd <- dd[order(names(dd))]
  dd
}



### Get the records
epidata.records <- function(datfile, flds) {
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:27
  epi.records <- xmlChildren(datfile)[["Records"]]
  num.recs <- xmlSize(epi.records)
  status.log(paste("Found", num.recs, "records"))
  status.log("get the xmlAttrs")
  ## recs <- xmlApply(epi.records, xmlAttrs)
  recs <- lapply(recs, extract.epidata.records, flds)
  check.record.structure(recs)
  save(recs, file = "temp-recs.Rda") # For debugging
  status.log("rbind the records")
  recs <- as.data.frame(do.call(rbind, recs))
  rownames(recs) <- NULL
  recs
}


check.record.structure <- function(recs) {
  ## Purpose: See if there are unequal numbers of fields in the rows of data.
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:27
  num.recs <- length(recs)
  num.flds <- NULL
  for (i in 1:num.recs) {
    num.flds <- c(num.flds, length(recs[[i]]))
  }
  if (length(unique(num.flds)) > 1) status.log("Warning: Varying number of columns per record")
}


convert.type <- function(x, fld.type, dec.sep) {
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:27
  ## Type conversion
  if (fld.type %in% c(1, 2)) {
    x <- as.numeric(as.character(x))
  } else if (fld.type %in% c(12, 13)){
    ## Characters, do nothing
  } else if (fld.type == 3){
    ## Work around comma decimal point issue.
    if (!is.null(dec.sep)) {
      levels(x) <- gsub("[,.]", dec.sep, levels(x))
    }
    x <- as.numeric(as.character(x))
  } else if (fld.type == 4){
    ## 16/05/1968 (DD/MM/YYYY, i.e. 16th of May, 1968)
    x <- as.Date(x, "%d/%m/%Y")
  } else {
    status.log(paste("Field type not handled:", fld.type))
  }
  x
}


### Field/structure info
epidata.apply.field.structure <- function(sections, dat, dec.sep) {
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:27
  num.sections <- xmlSize(xmlChildren(sections))
  dat.orig <- dat
  
  for (si in 1:num.sections) {
    fields <- xmlChildren(xmlChildren(sections)[[si]])[["Fields"]]
    save(dat, dat.orig, fields, sections, file = "temp.Rda") # For debugging
    num.flds <- xmlSize(fields)
    
    for (i in 1:num.flds) {
      field <- xmlChildren(fields)[[i]]
      fld.id <- xmlAttrs(field)
      fld.name <- xmlValue(xmlChildren(field)[["Name"]])
      fld.type <- xmlValue(xmlChildren(field)[["Type"]])
      
      fld <- which(names(dat) == fld.id)
      names(dat)[fld] <- fld.name
      dat[, fld] <- convert.type(dat[, fld], fld.type, dec.sep)
    }
  }
  dat
}





read.epidata.xml <- function(x, dec.sep = NULL) {
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:27
  require(XML)
  status.log(paste("Parsing", x))
  x <- xmlTreeParse(x)
  epidata <- xmlRoot(x)
  x.fld.info <- fld.info(epidata)
  
  y <- list()
  ## Get the data files
  num.datafiles <- xmlSize(xmlChildren(epidata)["DataFiles"])
  for (i in 1:num.datafiles) {
    datfile <- xmlChildren(xmlChildren(epidata)[["DataFiles"]])[[i]]
    datfile.name <- xmlAttrs(datfile)[["id"]]
    sections <- xmlChildren(datfile)[["Sections"]]
    
    status.log("Get the records")
    dat1 <- epidata.records(datfile, x.fld.info$id)
    status.log("Apply field structure")
    dat1 <- epidata.apply.field.structure(sections, dat1, dec.sep)
    y[i] <- list(dat1)
    names(y)[i] <- datfile.name
  }
  status.log("Finished.")
  y
}


fld.info <- function(x) {
  ## Purpose: Create a table of info about the fields
  ## ----------------------------------------------------------------------
  ## Arguments: an xmlRoot() 
  ## ----------------------------------------------------------------------
  ## Author: David Whiting, Date: 12 Jun 2011, 18:26
  dd <- xmlElementsByTagName(x, "Field", rec= TRUE)
  fld.id <- NULL
  fld.name <- NULL
  fld.type <- NULL
  fld.length <- NULL
  fld.decimals <- NULL
  fld.question <- NULL
  for (i in 1:xmlSize(dd)) {
    fld.id <- c(fld.id, xmlAttrs(dd[[i]])[["id"]])
    fld.name <- c(fld.name, xmlValue(xmlChildren(dd[[i]])[["Name"]]))
    fld.type <- c(fld.type, xmlValue(xmlChildren(dd[[i]])[["Type"]]))
    fld.length <- c(fld.length, xmlValue(xmlChildren(dd[[i]])[["Length"]]))
    fld.decimals <- c(fld.decimals, xmlValue(xmlChildren(dd[[i]])[["Decimals"]]))
    fld.question <- c(fld.question, xmlValue(xmlChildren(dd[[i]])[["Question"]]))
  }
  data.frame(id = fld.id,
             name = fld.name,
             type = fld.type,
             length = fld.length,
             decimals = fld.decimals,
             question = fld.question)
}







### todo

## It is possible that the records can contain data for different
## numbers of fields, e.g. if a field is added after some records have
## been entered. At the moment this code does not handle that situation properly.

### Now use it.
## x <- read.epidata.xml("test-small.epx", dec.sep = ".")
## x <- read.epidata.xml("test-small-simple.epx", dec.sep = ".")[[1]]

## x <- read.epidata.xml("test.epx", dec.sep = ".")[[1]]


## dd <- xmlElementsByTagName(sections, "Field", rec= TRUE)
## dd <- xmlElementsByTagName(xx, "Field", rec= TRUE)


x <- read.epidata.xml("sample.epx", dec.sep = ".")


## Some new ideas about getting records. Focus is on replacing the
## missing fields.
## recs <- xmlElementsByTagName(xx, "Record", rec= TRUE)





