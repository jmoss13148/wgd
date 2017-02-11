#' @title Eph Data
#' @description Reads information about Williams College Graduating classes between 2004 and 2016
#' and constructs a dataset containing the information the college provides
#' @param dataset The dataset that the function will read and parse. The options are
#' "names20xx" where xx can be any number from 04 to 16.
#' @return An 8 column dataframe containing information about Williams College graduates. In order
#' by column, the dataframe contains year, what year the student graduted, latin honors, which latin
#' honors, if any, did the student earn, first name, first name length, last name, last name length,
#' subject honors, highest for highest honors, with for honors, and blank for no honors, and subect
#' those honors were earned, blank if no honors were earned.
#' @usage
#' EphData(dataset)
#' @import stringr
#' @import tidyr
#' @import reshape2
#' @export

EphData <- function(dataset){
 
library(tidyr)
library(reshape2)
  
datasets <- c("names2004.txt", "names2005.txt", "names2006.txt", "names2007.txt", "names2008.txt", "names2009.txt", "names2010.txt", "names2011.txt", "names2012.txt", "names2013.txt", "names2014.txt", "names2015.txt", "names2016.txt")
if(dataset %in% datasets){

df <- readLines(system.file("extdata", dataset, package = "wgd"), warn = FALSE)

y <- colsplit(df," ",c("First Name","rest")) # separate first name
x <- colsplit(y[, 2], ",", c("lastname", "rest")) # separate last name block

split.x <- colsplit(x[, 1], "with", c("name", "honors")) # split up based on honors
testing <- as.data.frame(cbind(split.x[, 2], x[, 2]), stringsAsFactors = FALSE)
test <- unite(testing, V2, V1:V2, sep="") # merge these two dataframes
full <- as.data.frame(cbind(y[, 1], split.x[, 1], test), stringsAsFactors = FALSE)

char <- lapply(full, as.character)
complete2 <- as.data.frame(char, stringsAsFactors = FALSE) # first name, last name, and rest of information
complete2[, 1] <- gsub("[[:digit:]]", "", complete2[, 1]) # remove page numbers
# for possible last name endings we don't want
complete2[, 2] <- gsub("Jr.", "", complete2[, 2])
complete2[, 2] <- gsub("I", "", complete2[, 2])
complete2[, 2] <- gsub("II", "", complete2[, 2])
complete2[, 2] <- gsub("III", "", complete2[, 2])
complete2[, 2] <- gsub("V", "", complete2[, 2])
complete3 <- complete2[nchar(complete2[, 1]) > 0, ] ## remove possible instances of extra lines from above
split.complete3 <- colsplit(complete3[, 3], "honors", c("highest", "regular"))
split.complete3[, 2] <- gsub(".* ", "", split.complete3[, 2])

# take out special characters for first names
complete3[, 1] <- gsub("[^[:alnum:] ]", "", complete3[, 1])
# take out numbers
complete3[, 1] = gsub("[[:digit:]]", "", complete3[, 1])
xy <- as.data.frame(cbind(complete3[, 1:2], split.complete3))
## add column for latin honors
find <- grep("Bachelor", xy[, 1])
  summa <- rep("summa", find[2] - 1)
  magna <- rep("magna", find[3] - find[2] - 1)
  cum <- rep("cum", find[4] - find[3] - 1)
  none <- rep("", nrow(xy) - find[4] - 1)
  bind <- c(summa, magna, cum, none)
# remove those rows from xy
rm <- grep("Bachelor", xy[, 1])
xy.filter <- xy[-rm, ]
full.t <- cbind(bind, xy.filter)
## remove trailing spaces and
full.t[, 3] <- gsub("^\\s+|\\s+$", "", full.t[, 3])
full.t[, 3] <- gsub(".* ", "", full.t[, 3])
full.t[, 4] <- gsub("^\\s+|\\s+$", "", full.t[, 4])
full.t[, 4] <- gsub(".* ", "", full.t[, 4])

## add a column for what year
year <- rep(substr(dataset, 6, 9), nrow(full.t))
full.t2 <- as.data.frame(cbind(year, full.t), stringsAsFactors = FALSE)
## replace NA values with blank
full.t2[, 5][is.na(full.t2[, 5])] <- ""

# first name lengths
len <- numeric(nrow(full.t2))
for(i in 1:nrow(full.t2)) {
  len[i] <- nchar(full.t2[i, 3])
}
# last name lengths
len.2 <- numeric(nrow(full.t2))
for(i in 1:nrow(full.t2)) {
  len.2[i] <- nchar(full.t2[i, 4])
}
# bind relavent columns
full.t2 <- as.data.frame(cbind(full.t2[, 1:3], len, full.t2[, 4], len.2, full.t2[, 5:6]), stringsAsFactors = FALSE)
names(full.t2) <- c("year", "latin honors", "first name", "length", "last name", "length", "subject honors", "subject")
full.t2[, 2] <- as.character(full.t2[, 2])
full.t2[, 3] <- as.character(full.t2[, 3])
full.t2[, 5] <- as.character(full.t2[, 5])
full.t2[, 7] <- as.character(full.t2[, 7])
return(full.t2)
}
else { stop("Enter a correct dataset")}
}



