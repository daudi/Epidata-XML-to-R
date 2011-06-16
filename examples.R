### EXAMPLES

### NOTE: read.epidata.xml() reads in the epidata XML file and creates
### a list that has both the data and metadata. Use names(x) to look
### at what x contains. The data are in the first
### element of the list, i.e. x$data. file_id_0 or x[[1]]

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
x$datafile_id_0$VL1 <- epidata.value.label(x$datafile_id_0$VL1, x$labels, "valuelabelset_id_0")
