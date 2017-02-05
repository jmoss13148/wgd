#' @title Read Name Data
#' @description Reads the first names of Williams College graduating classes from 2004 to 2016
#' @param dataset The dataset that the function will read and parse. The options are "names20xx" where xx can be any number from 04 to 16.
#' @return A dataframe containing the first names and length of first names for all Williams College Graduates of a certain year.
#' @usage
#' readNameData(dataset)
#' @export

readNameData <- function(dataset){

datasets <- c("names2004.txt", "names2005.txt", "names2006.txt", "names2007.txt", "names2008.txt", "names2009.txt", "names2010.txt", "names2011.txt", "names2012.txt", "names2013.txt", "names2014.txt", "names2015.txt", "names2016.txt")
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


