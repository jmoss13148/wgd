#' @title readEphData
#' @description Scan data on the United States geographic distribution of Williams Students from 2001 to 2016. It includes
#' all 50 states, District of Columbia, Guam, and Puerto Rico. It excludes students from the Armed Forces Europe or
#' Pacific and the Virgin Islands.
#' @param dataset File that will be read and parsed. The options are williams20xx.txt where xx can be from 01 to 16.
#' @return Matrix containing the number of students from each state or province from the user coded year. All years between
#' #' 2001 and 2016 will be combined in later functions.
#' @usage
#' readEphData(dataset)
#' @import stringr
#' @export

readEphData <- function(dataset) {

  if(dataset %in% c("williams2001.txt", "williams2002.txt", "williams2003.txt", "williams2004.txt", "williams2005.txt", "williams2006.txt", "williams2007.txt", "williams2008.txt", "williams2009.txt", "williams2010.txt")) {
      frame <- readLines(system.file("extdata", dataset, package = "wgd"), warn = FALSE)
          rm_spaces <- gsub(" ", "", frame) # remove spaces
          rm_stuff <- gsub("[[:punct:]]", "", rm_spaces) # remove non alphanumeric characters
          rm_one <- rm_stuff[nchar(rm_stuff) > 0] # remove possible empty value
      states <- rm_one[nchar(rm_one) > 3] # subset for state names
      numbers <- rm_one[nchar(rm_one) <= 3] # subset for number of students
    total <- cbind(states, numbers)
}
    # now have 2 x 2 matrix/frame with variable number of rows

  else if(dataset %in% c("williams2011.txt", "williams2012.txt", "williams2013.txt", "williams2014.txt", "williams2015.txt", "williams2016.txt")) {
        frame <- readLines(system.file("extdata", dataset, package = "wgd"), warn = FALSE)
            frame <- frame[nchar(frame) > 1] # remove possible empty value
            rm_spaces <- gsub(" ", "", frame) # remove spaces
          states <- gsub("[0-9]", "", rm_spaces) # subset only states
          numbers <- gsub("[a-zA-Z]", "", rm_spaces) # subset only numbers
      total <- cbind(states, numbers)
}
  # now have 2 x 2 matrix/frame with variable number of rows

  possible_sub <- grep("Armed|Islands", total) # exclude cases with Armed Forces or Virgin Islands
  if(length(possible_sub) > 0) { total <- total[-possible_sub, ] } # for cases these categories aren't present in first place

  # add in columns for states that may have 0 students and are thus not displayed

  if(identical(total[total[, 1] %in% c("Nebraska", "Nebraska\t"), 1], character(0))) {
    a1 <- matrix(c("Nebraska", "0"), 1, 2)
  }  else { a1 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("PuertoRico", "PuertoRico\t"), 1], character(0))) {
    a2 <- matrix(c("PuertoRico", "0"), 1, 2)
  }  else { a2 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Wyoming", "Wyoming\t"), 1], character(0))) {
    a3 <- matrix(c("Wyoming", "0"), 1, 2)
  }  else { a3 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("WestVirginia", "WestVirginia\t"), 1], character(0))) {
    a4 <- matrix(c("WestVirginia", "0"), 1, 2)
  }  else { a4 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Guam", "Guam\t"), 1], character(0))) {
    a5 <- matrix(c("Guam", "0"), 1, 2)
  }  else { a5 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Kansas", "Kansas\t"), 1], character(0))) {
    a6 <- matrix(c("Kansas", "0"), 1, 2)
  }  else { a6 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Mississippi", "Mississippi\t"), 1], character(0))) {
    a7 <- matrix(c("Mississippi", "0"), 1, 2)
  }  else { a7 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("NorthDakota", "NorthDakota\t"), 1], character(0))) {
    a8 <- matrix(c("NorthDakota", "0"), 1, 2)
  }  else { a8 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Nevada", "Nevada\t"), 1], character(0))) {
    a9 <- matrix(c("Nevada", "0"), 1, 2)
  }  else { a9 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Montana", "Montana\t"), 1], character(0))) {
    a10 <- matrix(c("Montana", "0"), 1, 2)
  }  else { a10 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("Oklahoma", "Oklahoma\t"), 1], character(0))) {
    a11 <- matrix(c("Olkahoma", "0"), 1, 2)
  }  else { a11 <- matrix(1:2, 1, 2)}
  if(identical(total[total[, 1] %in% c("SouthDakota", "SouthDakota\t"), 1], character(0))) {
    a12 <- matrix(c("SouthDakota", "0"), 1, 2)
  }  else { a12 <- matrix(1:2, 1, 2)}

    full_data <- rbind(total, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) # combine all possible rows
    full_data <- full_data[nchar(full_data[, 1]) > 1, ] # subset for rows with a valid state
    full_data <- full_data[order(full_data[, 1]), ] # sort by ascending state
    full_data
}

