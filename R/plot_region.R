#' @title Plot Region
#' @description Uses the geographic distribution data from the read function to generate graphics for user coded
#' regions
#' @param region The region that will be plotted. The possible options are West, Mountain West, South, Midwest, Northeast,
#'  Mid Atlantic, and All.
#' They should be coded as strings without spaces and with capital letter where normal. For example, /code{"MidAtlantic"}
#' @return A barplot for the specified region. If the user codes /code{"All"}, the function will return all regions.
#' @usage
#' plot_region(region)
#' @export

# enter "West", "MountainWest", "South", "Midwest", "Northeast", "MidAtlantic", or "All"
plot_region <- function(region) {

  williams_data <- as.data.frame(cbind(readEphData("williams2001.txt"), readEphData("williams2002.txt"), readEphData("williams2003.txt"), readEphData("williams2004.txt"), readEphData("williams2005.txt"), readEphData("williams2006.txt"), readEphData("williams2007.txt"), readEphData("williams2008.txt"), readEphData("williams2009.txt"), readEphData("williams2010.txt"), readEphData("williams2011.txt"), readEphData("williams2012.txt"), readEphData("williams2013.txt"), readEphData("williams2014.txt"), readEphData("williams2015.txt"), readEphData("williams2016.txt")), stringsAsFactors = FALSE)
  frame <- williams_data[, c(31, seq(2, 32, by = 2))]
  dat <- as.data.frame(lapply(frame[, 2:ncol(frame)], as.numeric)) # make number columns numeric
  dat <- cbind(williams_data[, 31], dat) # combine with list of states
  names(dat) <- c("State", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014", "2015", "2016")

  subwest <- dat[dat$State %in% c("California", "Oregon", "Washington", "Idaho", "Nevada", "Arizona"), ]
  West <- colSums(subwest[2:ncol(subwest)])
  submountainwest <- dat[dat$State %in% c("NewMexico", "Colorado", "Wyoming", "Montana", "NorthDakota", "SouthDakota", "Utah"), ]
  MountainWest <- colSums(submountainwest[2:ncol(subwest)])
  subsouth <- dat[dat$State %in% c("Oklahoma", "Texas", "Arkansas", "Mississippi", "Alabama", "Louisiana", "Florida", "Georgia", "SouthCarolina", "NorthCarolina", "Tennesse"), ]
  South <- colSums(subsouth[2:ncol(subwest)])
  submidwest <- dat[dat$State %in% c("Nebraska", "Kansas", "Iowa", "Missouri", "Ohio", "Kentucky", "Indiana", "Illinois", "Wisconsin", "Michigan", "Minnesota"), ]
  Midwest <- colSums(submidwest[2:ncol(subwest)])
  subnortheast <- dat[dat$State %in% c("New York", "Massachusetts", "Vermont", "New Hampshire", "Maine", "Rhode Island", "Connecticut"), ]
  Northeast <- colSums(subnortheast[2:ncol(subwest)])
  submidatlantic <- dat[dat$State %in% c("Virginia", "Deleware", "DistrictofColumbia", "NewJersey", "WestVirginia", "Pennsylvania", "Maryland"), ]
  MidAtlantic <- colSums(submidatlantic[2:ncol(subwest)])

  if(region == "West") {
    df.bar <- barplot(West, main = region, ylab = "Number of Students", col = "purple")
    lines(x = df.bar, y = West/2, col = "white")
    return(points(x = df.bar, y = West/2, col = "white")) }
  else if(region == "MountainWest") {
    df.bar <- barplot(MountainWest, main = region, ylab = "Number of Students", col = "purple")
    lines(x = df.bar, y = MountainWest/2, col = "white")
    return(points(x = df.bar, y = MountainWest/2, col = "white")) }
  else if(region == "South") {
    df.bar <- barplot(South, main = region, ylab = "Number of Students", col = "purple")
    lines(x = df.bar, y = South/2, col = "white")
    return(points(x = df.bar, y = South/2, col = "white")) }
  else if(region == "Midwest") {
    df.bar <- barplot(Midwest, main = region, ylab = "Number of Students", col = "purple")
    lines(x = df.bar, y = Midwest/2, col = "white")
    return(points(x = df.bar, y = Midwest/2, col = "white")) }
  else if(region == "Northeast") {
    df.bar <- barplot(Northeast, main = region, ylab = "Number of Students", col = "purple")
    lines(x = df.bar, y = Northeast/2, col = "white")
    return(points(x = df.bar, y = Northeast/2, col = "white")) }
  else if(region == "MidAtlantic") {
    df.bar <- barplot(MidAtlantic, main = region, ylab = "Number of Students", col = "purple")
    lines(x = df.bar, y = MidAtlantic/2, col = "white")
    return(points(x = df.bar, y = MidAtlantic/2, col = "white")) }
  else if(region == "All") {
    data<- rbind(West, MountainWest, South, Midwest, Northeast, MidAtlantic)

    df.bar <- plot(data[1, ], ylim = c(0, 700), main = "All Regions", xlab = "Year", ylab = "Number of Students", axes = FALSE, bty = "L")
    v1 <- c(1, 6, 11, 16) # -> defines position of tick marks.
    v2 <- c("2001", "2006", "2011", "2016") # defines labels of tick marks.
    axis(side = 1, at = v1, labels = v2, tck = -.05)
    axis(side = 2)
    lines(data[1, ], type = "o", pch = 15, lty=2, col = "red")
    lines(data[2, ], type = "o", pch = 16, lty=2, col = "black")
    lines(data[3, ], type = "o", pch = 17, lty=2, col = "purple")
    lines(data[4, ], type = "o", pch = 18, lty=2, col = "green")
    lines(data[5, ], type = "o", pch = 16, lty=2, col = "blue")
    lines(data[6, ], type = "o", pch = 16, lty=2, col = "orange")
    colors = c("red", "black", "purple", "green", "blue", "orange")
    par(xpd=TRUE)
    legend(x = 13.5 , y = 850 , c("West","MountainWest","South","Midwest","Northeast", "MidAtlantic"), bty ="n", fill = colors, cex = 0.5)
  } else { stop("Input any valid region or All. Example: plot_region('MountainWest')") }
}

# possible options
# plot_region("West")
# plot_region("MountainWest")
# plot_region("South")
# plot_region("Midwest")
# plot_region("MidAtlantic")
# plot_region("Northeast")
# plot_region("All")
