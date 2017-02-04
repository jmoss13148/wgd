#' @title Summary
#' @description Uses the data from the readNameData() function to compute relavent summary statistics
#' @param type The type of statistics to be computed. The options are "popular", "most popular", "name length", and
#' "name length plot".
#' @return If the user codes popular, the function returns a dataframe with the six most popular names
#' over the past 13 years. If the user codes most popular, the function returns a dataframe reporting all
#' names that have been in the top six over the past 13 years, and how many times each name has been in the top six.
#' If the user codes name length, the function returns a dataframe reporting the average first name length of the
#' graduating class for each year. If the user codes name length plot, the function reports name length data in
#' the form of a scatterplot.
#' @usage
#' summary(type)
#' @export

summary <- function(type){

datasets <- c("names2004", "names2005", "names2006", "names2007", "names2008", "names2009", "names2010", "names2011", "names2012", "names2013", "names2014", "names2015", "names2016")

## for computing the most popular names
dat <- data.frame(x <- 1:6)
for(i in 1:length(datasets)){
  popular <- as.data.frame(tail(sort(table(readNameData(datasets[i])[, 1])))) ## subset for most popular names of certain year
  popular <- popular[order(popular$Var1, decreasing = TRUE), ]
  dat <- cbind(dat, popular)
}
total <- dat[, seq(2, ncol(dat), 2)] ## dataframe with 6 most popular names for each year
names(total) <- c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

## for computing the average first name length
num <- numeric(length(datasets))
for(i in 1:length(datasets)){
  num[i] <- mean(readNameData(datasets[i])[, 2]) ## average name length for each graduating class year
}
names(num) <- c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

if(type == "popular"){
return(total)
}

if(type == "most popular"){
num <- character()
for(i in 1:ncol(total)){
  names <- as.character(total[, i])
  num <- c(num, names) ## bind together most popular names from each year
}
sorted <- sort(table(num)) ## gather the information in a table
ordered <- as.data.frame(sorted)
ordered <- ordered[order(ordered[, 2], decreasing = TRUE), ]
names(ordered) <- c("Name", "# times in top 6")
return(ordered) ## dataframe containing names that have been in the top six, and how many times each name is been in the top six
}

if(type == "name length"){
return(num)
}

if(type == "name length plot"){
plot(names(num), num, main = "First Name Length", ylab = "Number of Characters", xlab = "Year", col = "purple", pch = 16)
abline(lm(num ~ as.numeric(names(num))))
}
}






