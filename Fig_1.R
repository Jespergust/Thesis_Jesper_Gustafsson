install.packages("readxl")
install.packages("zoo")
library(readxl)
library(zoo)

nber_recessions <- data.frame(
  start = as.yearmon(c("1990-07", "2001-05", "2007-12", "2020-02")),
  end   = as.yearmon(c("1990-10", "2001-11", "2009-07", "2020-04"))
)


#Set current directory to folder of this R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load dataframe
VRP_dataset <- read_excel("VRP_data_1990_2023.xlsx")

#Select relevant data for plotting
IV <- VRP_dataset$IV
RV <- VRP_dataset$RV
VRP <- VRP_dataset$VRP
dates_VRP <- as.yearmon(paste(VRP_dataset$Year, VRP_dataset$Month), "%Y %m")
IV_RV <- c("IV", "RV")

#Function for plotting data
plot_data <- function(data, dates) {
  data_name <- deparse(substitute(data))
  if(data_name %in% IV_RV) {
    y_ticks <- c(0, 100, 200, 300, 400,500,600)
    
    x_labels <- seq(as.yearmon("1990-01"), max(dates), by = 2)
    x_labels_fmt <- tools::toTitleCase(format(x_labels, "%b %y"))
    
    plot(dates, data, type = "l",lwd = 2, ylim = c(0, 650), xlim = range(dates),
         xaxs = "i", axes = FALSE, xlab = "",ylab = "")
    
    for(i in 1:nrow(nber_recessions)) {
      rect(xleft = nber_recessions$start[i],
           xright = nber_recessions$end[i],
           ybottom = par("usr")[3],  # bottom of plot
           ytop = par("usr")[4],     # top of plot
           col = rgb(0.8, 0.8, 0.8, 0.5),  # semi-transparent gray
           border = "black")
    }
    
    abline(v = as.yearmon("2008-01"), lty = 2)
    
    axis(2, at = y_ticks, las = 1, tck = 0.02)
    axis(4, at = y_ticks, las = 1, tck = 0.02, labels = FALSE)
    axis(1, at = x_labels, labels = x_labels_fmt, tck = 0.02)
    axis(3, at = x_labels, tck = 0.02, labels = FALSE)
    box()
    if(data_name == "IV") {
      title(main = "S&P 500 implied variance", line = 1, cex.main = 1, font.main = 1)
    } else {
      title(main = "S&P 500 realized variance", line = 1, cex.main = 1, font.main = 1)
    }
    
  } else {
    y_ticks <- c(-400,-300,-200,-100,0, 100)
    
    x_labels <- seq(as.yearmon("1990-01"), max(dates), by = 2)
    x_labels_fmt <- tools::toTitleCase(format(x_labels, "%b %y"))
    
    plot(dates, data, type = "l", lwd = 2,ylim = c(-400,150), xlim = range(dates),
         xaxs = "i", axes = FALSE, xlab = "",ylab = "")
    
    for(i in 1:nrow(nber_recessions)) {
      rect(xleft = nber_recessions$start[i],
           xright = nber_recessions$end[i],
           ybottom = par("usr")[3],  # bottom of plot
           ytop = par("usr")[4],     # top of plot
           col = rgb(0.8, 0.8, 0.8, 0.5),  # semi-transparent gray
           border = "black")
    }
    
    abline(v = as.yearmon("2008-01"), lty = 2)
    
    axis(2, at = y_ticks, las = 1, tck = 0.02)
    axis(4, at = y_ticks, las = 1, tck = 0.02, labels = FALSE)
    axis(1, at = x_labels, labels = x_labels_fmt, tck = 0.02)
    axis(3, at = x_labels, tck = 0.02, labels = FALSE)
    box()
    title(main = "Difference between implied and realized variances", line = 1, cex.main = 1, font.main = 1)
  }
  
}

par(mfrow = c(3, 1), mar = c(4, 4, 2, 4))

plot_data(IV,dates_VRP)
plot_data(RV,dates_VRP)
plot_data(VRP,dates_VRP)

par(mfrow = c(1, 1))

