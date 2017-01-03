#' @title Total Dist
#' @description Uses the geographic distribution data from the readEphData function to generate graphics and summary statistic for user coded options.
#' @param type The type of output generated. The possible options are Summary, US, and SD. Summary generates
#' a dataframe containing the geographic distribution for the years 2001 to 2016. US generates a barplot displaying
#' how total United States enrollment has changed at Williams from 2001 to 2016. SD generates a data frame containing
#' the standard deviation in total United States enrollment for the year blocks 2001-2010 and 2011-2016.
#' For example, /code{"Summary"}
#' @return Dataframe or barplot depending on type.
#' @usage
#' total_dist(type)
#' @export


total_dist <- function(type) {

williams_data <- as.data.frame(cbind(readEphData("williams2001.txt"), readEphData("williams2002.txt"), readEphData("williams2003.txt"), readEphData("williams2004.txt"), readEphData("williams2005.txt"), readEphData("williams2006.txt"), readEphData("williams2007.txt"), readEphData("williams2008.txt"), readEphData("williams2009.txt"), readEphData("williams2010.txt"), readEphData("williams2011.txt"), readEphData("williams2012.txt"), readEphData("williams2013.txt"), readEphData("williams2014.txt"), readEphData("williams2015.txt"), readEphData("williams2016.txt")), stringsAsFactors = FALSE)
frame <- williams_data[, c(31, seq(2, 32, by = 2))]
dat <- as.data.frame(lapply(frame[, 2:ncol(frame)], as.numeric)) # make number columns numeric
dat <- cbind(williams_data[, 31], dat) # combine with list of states
names(dat) <- c("State", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                "2012", "2013", "2014", "2015", "2016")
if(type == "Summary") {
  return(dat)
}
else if(type == "US") {
  USenrollment <- colSums(dat[, 2:ncol(dat)])
  df.bar <- barplot(USenrollment, main = "Total United States Enrollment", col = "purple", ylab = "Number of Students") # includes Guam, Puerto Rico
  lines(x = df.bar, y = USenrollment/2, col = "white")
  return(points(x = df.bar, y = USenrollment/2, col = "white"))
}
else if(type == "SD") {
  enrollment_var <- as.data.frame(cbind(sd(USenrollment[1:10]), sd(USenrollment[11:16])))
  names(enrollment_var) <- c("2001-2010", "2011-2016")
  row.names(enrollment_var) <- "SD"
  return(enrollment_var)
} else { stop("Input Summary, US, or SD") }
}


