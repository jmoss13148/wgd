#' @title Plot State
#' @description Uses the geographic distribution data from the readEphData function to generate graphics for user coded states.
#' @param state The state that will be plotted. The possible options are all 50 states, Guam, Puerto Rico,
#' District of Columbia, and Notable. They should be coded as strings without spaces and with capital letters
#' where normal. For example, /code{"NorthCarolina"}
#' @return A barplot for the specified state. If the user codes /code{"Notable"}, the function will return notable states.
#' @usage
#' plot_state(state)
#' @export

# enter state or province without spaces and with capital letters where normal
plot_state <- function(state) {

  williams_data <- as.data.frame(cbind(readEphData("williams2001.txt"), readEphData("williams2002.txt"), readEphData("williams2003.txt"), readEphData("williams2004.txt"), readEphData("williams2005.txt"), readEphData("williams2006.txt"), readEphData("williams2007.txt"), readEphData("williams2008.txt"), readEphData("williams2009.txt"), readEphData("williams2010.txt"), readEphData("williams2011.txt"), readEphData("williams2012.txt"), readEphData("williams2013.txt"), readEphData("williams2014.txt"), readEphData("williams2015.txt"), readEphData("williams2016.txt")), stringsAsFactors = FALSE)
  frame <- williams_data[, c(31, seq(2, 32, by = 2))]
  dat <- as.data.frame(lapply(frame[, 2:ncol(frame)], as.numeric)) # make number columns numeric
  dat <- cbind(williams_data[, 31], dat) # combine with list of states
  names(dat) <- c("State", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014", "2015", "2016")

  if(state %in% dat[, 1]) {
  data <- dat[dat$State == state, ] #subset for state of interest
  data <- as.numeric(data[, 2:ncol(data)])
      names(data) <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                       "2012", "2013", "2014", "2015", "2016")
  df.bar <- barplot(data, main = state, bg = "grey", col = "purple", ylab = "Number of Students")
  lines(x = df.bar, y = data/2, col = "white")
  return(points(x = df.bar, y = data/2, col = "white"))
  }
  else if(state == "Notable") {
    data <- as.matrix(dat[dat$State %in% c("California", "Florida", "Hawaii", "Massachusetts", "Maine"), 2:17])
        df.bar <- plot(data[1, ], ylim = c(0, 450), main = "Notable States", xlab = "Year", ylab = "Number of Students", axes = FALSE, bty = "L")
        v1 <- c(1, 6, 11, 16) # -> defines position of tick marks.
        v2 <- c("2001", "2006", "2011", "2016") # defines labels of tick marks.
    axis(side = 1, at = v1, labels = v2, tck = -.05)
    axis(side = 2)
        lines(data[1, ], type = "o", pch = 15, lty=2, col = "red")
        lines(data[2, ], type = "o", pch = 16, lty=2, col = "black")
        lines(data[3, ], type = "o", pch = 17, lty=2, col = "purple")
        lines(data[4, ], type = "o", pch = 18, lty=2, col = "green")
        lines(data[5, ], type = "o", pch = 16, lty=2, col = "blue")
    colors = c("red", "black", "purple", "green", "blue")
    par(xpd=TRUE)
    legend(x = 10 , y = 525 , c("California", "Florida", "Hawaii", "Maine", "Massachusetts"), bty ="n", fill = colors, cex = 0.6)
  } else { stop("Input any valid state or Notable. Example: plot_state('DistrictofColumbia')") }
}

# suggested states
# plot_state("California")
# plot_state("DistrictofColumbia")
# plot_state("Florida")
# plot_state("Hawaii")
# plot_state("Guam")
# plot_state("Maine")
# plot_state("Massachusetts")
# plot_state("Mississippi")
# plot_state("Montana")
# plot_state("NewHampshire")
# plot_state("PuertoRico")
# plot_state("Texas")
# plot_state("Vermont")
# plot_state("Virginia")

