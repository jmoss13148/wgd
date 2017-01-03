williams_data <- cbind(readEphData("williams2001.txt"), readEphData("williams2002.txt"), readEphData("williams2003.txt"), readEphData("williams2004.txt"), readEphData("williams2005.txt"), readEphData("williams2006.txt"), readEphData("williams2007.txt"), readEphData("williams2008.txt"), readEphData("williams2009.txt"), readEphData("williams2010.txt"), readEphData("williams2011.txt"), readEphData("williams2012.txt"), readEphData("williams2013.txt"), readEphData("williams2014.txt"), readEphData("williams2015.txt"), readEphData("williams2016.txt"))
  williams_data <- as.data.frame(williams_data[, c(31, seq(2, 32, by = 2))]) # columns of interest
  x <- as.data.frame(lapply(williams_data[, 2:ncol(williams_data)], as.numeric), stringsAsFactors = FALSE) # make columns of interest numeric
  dat <- as.data.frame(cbind(williams_data[, 1], x)) # combine these with list of states
  names(dat) <- c("State", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014", "2015", "2016")

USenrollment <- colSums(dat[, 2:ncol(dat)])
df.bar <- barplot(USenrollment, main = "Total United States Enrollment", col = "purple", ylab = "Number of Students") # includes Guam, Puerto Rico
lines(x = df.bar, y = USenrollment/2, col = "white")
points(x = df.bar, y = USenrollment/2, col = "white")
enrollment_var <- as.data.frame(cbind(sd(USenrollment[1:10]), sd(USenrollment[11:16])))
names(enrollment_var) <- c("2001-2010", "2011-2016")
row.names(enrollment_var) <- "SD"
enrollment_var


