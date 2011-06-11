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
  right.now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  cat(paste(right.now, x, "\n"), file = "STATUS.LOG", append = TRUE)
}


### Get the records
epidata.records <- function(datfile) {
  epi.records <- xmlChildren(datfile)[["Records"]]
  num.recs <- xmlSize(epi.records)
  status.log(paste("Found", num.recs, "records"))
  status.log("get the xmlAttrs")
  recs <- xmlApply(epi.records, xmlAttrs)
  status.log("rbind the records")
  save(recs, file = "temp-recs.Rda")
  recs <- as.data.frame(do.call(rbind, recs))
  rownames(recs) <- NULL
  recs
}


convert.type <- function(x, fld.type, dec.sep) {
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
  num.sections <- xmlSize(xmlChildren(sections))
  dat.orig <- dat
  
  for (si in 1:num.sections) {
    fields <- xmlChildren(xmlChildren(sections)[[si]])[["Fields"]]
    save(dat, dat.orig, fields, sections, file = "temp.Rda")
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
  require(XML)
  status.log(paste("Parsing", x))
  x <- xmlTreeParse(x)
  epidata <- xmlRoot(x)

  y <- list()
  ## Get the data files
  num.datafiles <- xmlSize(xmlChildren(epidata)["DataFiles"])
  for (i in 1:num.datafiles) {
    datfile <- xmlChildren(xmlChildren(epidata)[["DataFiles"]])[[i]]
    datfile.name <- xmlAttrs(datfile)[["id"]]
    sections <- xmlChildren(datfile)[["Sections"]]
    
    status.log("Get the records")
    dat1 <- epidata.records(datfile)
    status.log("Apply field structure")
    dat1 <- epidata.apply.field.structure(sections, dat1, dec.sep)
    y[i] <- list(dat1)
    names(y)[i] <- datfile.name
  }
  status.log("Finished.")
  y
}


## fld.info <- function(sections) {
##   dd <- xmlElementsByTagName(sections, "Field", rec= TRUE)
 
## }




### TODO

## It is possible that the records can contain data for different
## numbers of fields, e.g. if a field is added after some records have
## been entered. At the moment this code does not handle that situation properly.

### Now use it.
x <- read.epidata.xml("test-small.epx", dec.sep = ".")

##x <- read.epidata.xml("test.epx", dec.sep = ".")[[1]]


