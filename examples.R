### EXAMPLES

### NOTE: read.epidata.xml() reads in the epidata XML file and creates
### a list that has both the data and metadata. Use names(x) to look
### at what x contains. The data are in the first element of the list,
### i.e. x$data. At the moment I think the epidata format only has one
### data file per XML file. When imported this takes the name of the
### ID in epidata, and currently this is x$data$datafile_id_0 and can
### also be accessed as x$data[[1]]

source("read-epidata.R")

## Example 1: Read in an epidata file, using the epidata labels and
## missing values (the default).
x <- read.epidata.xml("sample.epx")


## Example 2: Read in an epidata file, but do not use the epidata
## labels.
x <- read.epidata.xml("sample.epx", use.epidata.labels = FALSE)


## Example 3: Read in an epidata file, use the epidata labels, but do
## not convert the missing values to R NA values.
x <- read.epidata.xml("sample.epx", set.missing.na = FALSE)


## Example 4: Read in an epidata file, but do not use the epidata
## labels.
x <- read.epidata.xml("sample.epx", use.epidata.labels = FALSE)
## Now use the epidata labels for just one field
x$data$datafile_id_0$VL1 <- epidata.value.label(x$data$datafile_id_0$VL1, x$labels, "valuelabelset_id_0")


## Example 5: Read in an epidata file, taking a 30% random sample of
## the records.
x <- read.epidata.xml("sample.epx", random.pc = 30)


## Example 6: Read in the metadata from an epidata file, but do not
## read in any records (take 0% of the records).
x <- read.epidata.xml("sample.epx", random.pc = 0)


## Example 7: Read in an epidata file, taking the first 4 records.
x <- read.epidata.xml("sample.epx", num.recs = 4)
