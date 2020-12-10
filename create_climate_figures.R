###############################################################################
# FDSI timeseries figure
# 
###############################################################################
clim_ann <- read.csv("./clean data/climate_data_monthly.csv")

library(Hmisc)
opar <- par()

mean_fdsi <- vector(mode = "numeric", length = nyear)
for (i in 1:nyear){
  mean_fdsi[i] <- mean(clim_ann[clim_ann$year == (i + 1895), "fdsi"])
}

quant25 <- vector(mode = "numeric", length = nyear)
quant975 <- vector(mode = "numeric", length = nyear)
for (i in 1:nyear){
  quant25[i] <- quantile(clim_ann[clim_ann$year == (i + 1895), "fdsi"], 0)
  quant975[i] <- quantile(clim_ann[clim_ann$year == (i + 1895), "fdsi"], 1)
}


startyear_plot <- 1980
startindex_plot <- 1980-1895
endyear_plot <- 2019
endindex_plot <- nyear
lag_length <- 1

tiff(filename=paste0("./outputs/climate_data_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_fdsi[c(startindex_plot:endindex_plot)] ~ c(startyear_plot:endyear_plot),
     ylim = c(-2.5, 2),
     xlab = "Year",
     ylab = "FDSI",
     # main = "FDSI Timeseries",
     cex = 1,
     cex.lab = 1.6,
     cex.main = 2,
     cex.axis = 1.3,
     bg='grey60',
     col = 'grey30',
     pch = 21)
minor.tick(nx = 10)
f3 <- rep(1/lag_length, lag_length)
#start the moving average early to account for lag length
y_lag <- stats::filter(mean_fdsi[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

abline(h = 0)
abline(h= -1.41, lty = 2)
abline(v = 2005, lty = 3)
arrows(x0 = 2005, x1 = 2005, y0 = 1.5, y1 = 1, lwd = 2, length = 0.1)
abline(v = 2015, lty = 3)
arrows(x0 = 2015, x1 = 2015, y0 = 1.5, y1 = 1, lwd = 2, length = 0.1)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()



###############################################################################
# ppt timeseries figure
# 
###############################################################################
clim_ann <- read.csv("./clean data/climate_data_monthly.csv")

library(Hmisc)
opar <- par()

mean_fdsi <- vector(mode = "numeric", length = nyear)
for (i in 1:nyear){
  mean_fdsi[i] <- mean(clim_ann[clim_ann$year == (i + 1895), "MAP_anom"])
}

quant25 <- vector(mode = "numeric", length = nyear)
quant975 <- vector(mode = "numeric", length = nyear)
for (i in 1:nyear){
  quant25[i] <- quantile(clim_ann[clim_ann$year == (i + 1895), "MAP_anom"], 0)
  quant975[i] <- quantile(clim_ann[clim_ann$year == (i + 1895), "MAP_anom"], 1)
}


startyear_plot <- 1980
startindex_plot <- 1980-1895
endyear_plot <- 2019
endindex_plot <- nyear
lag_length <- 1

tiff(filename=paste0("./outputs/precip_data_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_fdsi[c(startindex_plot:endindex_plot)] ~ c(startyear_plot:endyear_plot),
     ylim = c(-200, 200),
     xlab = "Year",
     ylab = "Annual PPT anomaly",
     # main = "FDSI Timeseries",
     cex = 1,
     cex.lab = 1.6,
     cex.main = 2,
     cex.axis = 1.3,
     bg='grey60',
     col = 'grey30',
     pch = 21)
minor.tick(nx = 10)
f3 <- rep(1/lag_length, lag_length)
#start the moving average early to account for lag length
y_lag <- stats::filter(mean_fdsi[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

abline(h = 0)
abline(h= -1.41, lty = 2)
abline(v = 2005, lty = 3)
arrows(x0 = 2005, x1 = 2005, y0 = 1.5, y1 = 1, lwd = 2, length = 0.1)
abline(v = 2015, lty = 3)
arrows(x0 = 2015, x1 = 2015, y0 = 1.5, y1 = 1, lwd = 2, length = 0.1)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()


###############################################################################
# temp timeseries figure
# 
###############################################################################
clim_ann <- read.csv("./clean data/climate_data_monthly.csv")

library(Hmisc)
opar <- par()

mean_fdsi <- vector(mode = "numeric", length = nyear)
for (i in 1:nyear){
  mean_fdsi[i] <- mean(clim_ann[clim_ann$year == (i + 1895), "MAT_anom"])
}

quant25 <- vector(mode = "numeric", length = nyear)
quant975 <- vector(mode = "numeric", length = nyear)
for (i in 1:nyear){
  quant25[i] <- quantile(clim_ann[clim_ann$year == (i + 1895), "MAT_anom"], 0)
  quant975[i] <- quantile(clim_ann[clim_ann$year == (i + 1895), "MAT_anom"], 1)
}


startyear_plot <- 1980
startindex_plot <- 1980-1895
endyear_plot <- 2019
endindex_plot <- nyear
lag_length <- 1

tiff(filename=paste0("./outputs/temp_data_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_fdsi[c(startindex_plot:endindex_plot)] ~ c(startyear_plot:endyear_plot),
     ylim = c(-2, 2.5),
     xlab = "Year",
     ylab = "Annual mean temperature anomaly",
     # main = "FDSI Timeseries",
     cex = 1,
     cex.lab = 1.6,
     cex.main = 2,
     cex.axis = 1.3,
     bg='grey60',
     col = 'grey30',
     pch = 21)
minor.tick(nx = 10)
f3 <- rep(1/lag_length, lag_length)
#start the moving average early to account for lag length
y_lag <- stats::filter(mean_fdsi[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

abline(h = 0)
abline(h= -1.41, lty = 2)
abline(v = 2005, lty = 3)
arrows(x0 = 2005, x1 = 2005, y0 = 1.5, y1 = 1, lwd = 2, length = 0.1)
abline(v = 2015, lty = 3)
arrows(x0 = 2015, x1 = 2015, y0 = 1.5, y1 = 1, lwd = 2, length = 0.1)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()