#' @title Read Name Data
#' @description Reads the first names of Williams College graduating classes from 2004 to 2016
#' @param dataset The dataset that the function will read and parse. The options are "names20xx" where xx can be any number from 04 to 16.
#' @return A dataframe containing the first names and length of first names for all Williams College Graduates of a certain year.
#' @usage
#' readNameData(dataset)
#' @export

readNameData <- function(dataset){

datasets <- c("names2004", "names2005", "names2006", "names2007", "names2008", "names2009", "names2010", "names2011", "names2012", "names2013", "names2014", "names2015", "names2016")
if(dataset %in% datasets){
data <- readLines(system.file("extdata", dataset, package = "wgd"), warn = FALSE)

data <- gsub( " .*$", "", data) ## subset for characters before the first space
data <- gsub("[^[:alnum:] ]", "", data)
## remove instances where subject gets carried over to the next line
rm <- grep("Science|Bachelor|Major|Spanish|Degrees|in|Mathematics|Biology|Astrophysics|Physics|Economics|and|Studies|Psychology|Philosophy|Religion|honors|Computer|Geosciences|Chinese|History|Comparative|Neuroscience|Chemistry|English|Economy|Art|AfricanAmerican|American|Asian|Biochemistry|Cognitive|Critical|Environmental|German|Jewish|LatinAmerican|Leadership|Legal|Linguistics|Materials|Music|Performance|Romance|Russian|Women's|Womens", data)
data <- data[-rm]
data1 = gsub("[[:digit:]]", "", data)
data1 <- data[nchar(data1) > 0] ## remove possible instances of extra lines

# create vector with first name length
len <- numeric(length(data1))
for(i in 1:length(data1)) {
  len[i] <- nchar(data1[i])
}

total <- as.data.frame(cbind(data1, len), stringsAsFactors = FALSE)
total[, 2] <- as.numeric(total[, 2])
names(total) <- c("First Name", "Length")
return(total)
}
else { stop("Enter a correct dataset")}
}


