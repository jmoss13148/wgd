#' @title Name Summary
#' @description Produces summary statistics and graphics for Williams College graduates between
#' 2004 and 2016. The analysis is focused on the differenced between students that are guessed
#' to be Asian versus students who are guessed not to be Asian. Surname analysis is employed to make
#' this decision. The surnames are compared with 378 common Asian surnames. The positive predictive
#' rate is estimated to be 21%.
#' @param type The type of summary statistic or graphic the function will produce. The options
#' are honors, length, and popular.
#' @return If the user codes honors, the function will return a bar graph showing the proportion of
#' Asian and non-Asian students that earn Latin and Subject honors. If the user codes length, the
#' function will return a bar graph comparing the lengths of the first and last names of Asian
#' and non-Asian students. If the user codes popular, the function will return a bar graph showing
#' the correlation betweeen the lengths of first and last names, and the proportion of the single most
#' popular name between 2004 and 2016.
#' @usage
#' nameSummary(type)
#' @import stringr
#' @import tidyr
#' @import reshape2
#' @export


nameSummary <- function(type){

asian.names <- readLines("asiannames.txt")

data <- rbind(EphData("names2004.txt"), EphData("names2005.txt"), EphData("names2006.txt"), EphData("names2007.txt"), EphData("names2008.txt"), EphData("names2009.txt"), EphData("names2010.txt"), EphData("names2011.txt"), EphData("names2012.txt"), EphData("names2013.txt"), EphData("names2014.txt"), EphData("names2015.txt"), EphData("names2016.txt"))
# create a column to show if student is Asian or not
asian <- numeric(nrow(data))
for(i in 1:nrow(data)){
  if(data[i, 5] %in% asian.names){
    asian[i] <- 1
  }
  else { asian[i] <- 0 }
}
# bind this with data
full.data <- cbind(data, asian)
asian.data <- full.data[full.data[, 9] == 1, ]
other.data <- full.data[full.data[, 9] == 0, ]

if(type == "honors"){

# calculate proportions with latin honors
latin.asian <- grep("cum|magna|summa", asian.data[, 2])
prop.asian.latin <- round(length(latin.asian) / nrow(asian.data), 3)

latin.other <- grep("cum|magna|summa", other.data[, 2])
prop.other.latin <- round(length(latin.other) / nrow(other.data), 3)

# subject honors
honors.asian <- grep("with|highest", asian.data[, 7])
prop.asian.honors <- round(length(honors.asian) / nrow(asian.data), 3)

honors.other <- grep("with|highest", other.data[, 7])
prop.other.honors <- round(length(honors.other) / nrow(other.data), 3)

honors.data <- data.frame("Latin Honors" = c(prop.asian.latin, prop.other.latin), "Subject Honors" = c(prop.asian.honors, prop.other.honors))
row.names(honors.data) <- c("Asian", "Not Asian")
names(honors.data) <- c("Latin Honors", "Subject Honors")

barplot(as.matrix(honors.data), col = c("red", "blue"), main = "Latin and Subject Honors Proportions", ylab = "Proportion", ylim = c(0.0, 0.5),  beside = TRUE)
return(legend("topright", c("Asian","Not Asian"), cex=1, bty="n", fill = c("red", "blue")))
}

else if(type == "length"){

# length of first as last names - talk about drawbacks
asian.length <- data.frame("first name length" = mean(asian.data[, 4]), "last name length" = mean(asian.data[, 6]))
other.length <- data.frame("first name length" = mean(other.data[, 4]), "last name length" = mean(other.data[, 6]))
length.data <- rbind(asian.length, other.length)
row.names(length.data) <- c("Asians", "Non-Asians")
names(length.data) <- c("First Name", "Last Name")

barplot(as.matrix(length.data), col = c("red", "blue"), main = " Average Name Length", ylab = "# of characters", ylim = c(0, 8), beside = TRUE)
return(legend(x = max(other.length) + 2, c("Asian","Non-Asian"), cex=1, bty="n", fill = c("red", "blue")))
}

else if(type == "popular") {
fn.asian.cor <- cor(asian.data[, 4], asian.data[, 6])
fn.other.cor <- cor(other.data[, 4], other.data[, 6])

x <- tail(sort(table(asian.data[, 3])))
prop.asian.pop <- x[6] / nrow(asian.data)
y <- tail(sort(table(other.data[, 3])))
prop.other.pop <- y[6] / nrow(other.data)

x <- data.frame(a = c(round(fn.asian.cor, 3), round(fn.other.cor, 3)), b = c(round(prop.asian.pop, 3), round(prop.other.pop, 3)))
names(x) <- c("Name Correlation", "Prop. of Most Popular Name")
row.names(x) <-  c("Asian", "Non-Asian")
barplot(as.matrix(x), col = c("red", "blue"), main = "Name Statistics", ylab = "correlation or proportion", beside = TRUE)
box()
return(legend("bottomright", c("Asian","Non-Asian"), cex=1.2, bty="n", fill = c("red", "blue")))
}
else { stop("Please enter a correct summary type. The options are honors, length, or popular") }
}


number.asian <- nrow(asian.data)
prop.asian.est <- number.asian / (nrow(data) * 0.17)



