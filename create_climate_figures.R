clim_ann <- read.csv("./clean data/climate_data_yearly.csv")

library(Hmisc)
source("addTrans.R")

opar <- par()

startyear_plot <- 1950
startindex_plot <- startyear_plot-1895
endyear_plot <- 2019
endindex_plot <- length(unique(clim_ann$year))-(2019 - endyear_plot)
nyear <- endindex_plot - startindex_plot
lag_length <- 1


###############################################################################
# PDSI timeseries figure
# 
###############################################################################

#mean of all plots for a given year
mean_pdsi <- aggregate(clim_ann$pdsi, by = list(clim_ann$year), FUN = mean)$x

quant25 <- aggregate(clim_ann$pdsi, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$pdsi, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 1)})$x                   




tiff(filename=paste0("./outputs/pdsi_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_pdsi[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(-5, 5),
     xlab = "Year",
     ylab = "PDSI",
     # main = "PDSI Timeseries",
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
y_lag <- stats::filter(mean_pdsi[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
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
abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()

###############################################################################
# FDSI timeseries figure
# 
###############################################################################

mean_fdsi <- aggregate(clim_ann$fdsi, by = list(clim_ann$year), FUN = mean)$x


quant25 <- aggregate(clim_ann$fdsi, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$fdsi, by = list(clim_ann$year), 
                      FUN = function(x){quantile(x, 1)})$x                   




tiff(filename=paste0("./outputs/fdsi_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_fdsi[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(-2,2),
     xlab = "Year",
     ylab = "FDSI",
     # main = "PDSI Timeseries",
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
abline(h = -1.41)
abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()

###############################################################################
# ppt timeseries figure
# 
###############################################################################

mean_ppt <- aggregate(clim_ann$ppt_tot, by = list(clim_ann$year), FUN = mean)$x


quant25 <- aggregate(clim_ann$ppt_tot, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$ppt_tot, by = list(clim_ann$year), 
                      FUN = function(x){quantile(x, 1)})$x                   




tiff(filename=paste0("./outputs/ppt_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_ppt[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(0, 700),
     xlab = "Year",
     ylab = "Precipitation (mm)",
     # main = "PDSI Timeseries",
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
y_lag <- stats::filter(mean_ppt[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

abline(h = mean(mean_ppt))
abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()


###############################################################################
# temp timeseries figure
# 
###############################################################################

mean_temp <- aggregate(clim_ann$tmean, by = list(clim_ann$year), FUN = mean)$x


quant25 <- aggregate(clim_ann$tmean, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$tmean, by = list(clim_ann$year), 
                      FUN = function(x){quantile(x, 1)})$x                   


tiff(filename=paste0("./outputs/temp_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)


par(mar = c(5.1, 4.4, 2, 2.1),
    oma = c(0, .5, 0, 0))

plot(mean_temp[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(5, 11),
     xlab = "Year",
     ylab = "Temperature (C)",
     # main = "PDSI Timeseries",
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
y_lag <- stats::filter(mean_temp[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

abline(h = mean(mean_temp))
abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

dev.off()


#------------------------------------------------------------------------------
# Figure for paper
#------------------------------------------------------------------------------

clim_ann <- read.csv("./clean data/climate_data_yearly.csv")

library(Hmisc)
source("addTrans.R")

opar <- par()

startyear_plot <- 1950
startindex_plot <- startyear_plot-1895
endyear_plot <- 2019
endindex_plot <- length(unique(clim_ann$year))-(2019 - endyear_plot)
nyear <- endindex_plot - startindex_plot
lag_length <- 1


tiff(filename=paste0("./outputs/climate_timeseries_", startyear_plot, "-", endyear_plot, ".tiff"), 
     type="cairo",
     units="in", 
     width = 6, 
     height=4, 
     pointsize=12,
     compression = "lzw", 
     res=600)

par(mfrow = c(3,1),
    mar = c(.5, 4, .3, 1),
    oma = c(4, 2, 0, 0))


#mean of all plots for a given year
mean_pdsi <- aggregate(clim_ann$pdsi, by = list(clim_ann$year), FUN = mean)$x

quant25 <- aggregate(clim_ann$pdsi, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$pdsi, by = list(clim_ann$year), 
                      FUN = function(x){quantile(x, 1)})$x                   

plot(mean_pdsi[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(-5, 6),
     xlim = c(1949, 2020),
     xlab = "",
     ylab = "",
     # main = "PDSI Timeseries",
     cex = .5,
     cex.lab = 1,
     cex.main = .7,
     cex.axis = .7,
     xaxt = "n",
     xaxs="i",
     bg='grey60',
     col = 'grey30',
     pch = 21)

axis(side = 4)

axis(1, at = seq(1950, 2020, by = 10), labels = FALSE)
minor.tick(nx = 10, ny = 2)

f3 <- rep(1/lag_length, lag_length)
#start the moving average early to account for lag length
y_lag <- stats::filter(mean_pdsi[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
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
abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)
polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

title(ylab = "PDSI")


## ppt

mean_ppt <- aggregate(clim_ann$MAP_anom_prop, by = list(clim_ann$year), FUN = mean)$x
quant25 <- aggregate(clim_ann$MAP_anom_prop, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$MAP_anom_prop, by = list(clim_ann$year), 
                      FUN = function(x){quantile(x, 1)})$x    

plot(mean_ppt[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(.5, 2),
     xlim = c(1949, 2020),
     xlab = "",
     ylab = "",
     cex = .5,
     cex.lab = 1,
     cex.main = .7,
     cex.axis = .7,
     xaxt = "n",
     xaxs="i",
     bg='grey60',
     col = 'grey30',
     pch = 21)

axis(side = 4)

axis(1, at = seq(1950, 2020, by = 10), labels = FALSE)
minor.tick(nx = 10, ny = 2)

f3 <- rep(1/lag_length, lag_length)
#start the moving average early to account for lag length
y_lag <- stats::filter(mean_ppt[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

abline(h = mean(mean_ppt))

abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)

title(ylab = "Precip. anomaly")

#temp
mean_temp <- aggregate(clim_ann$MAT_anom_prop, by = list(clim_ann$year), FUN = mean)$x
quant25 <- aggregate(clim_ann$MAT_anom_prop, by = list(clim_ann$year), 
                     FUN = function(x){quantile(x, 0)})$x
quant975 <- aggregate(clim_ann$MAT_anom_prop, by = list(clim_ann$year), 
                      FUN = function(x){quantile(x, 1)})$x                   

plot(mean_temp[startindex_plot:endindex_plot] ~ c(startyear_plot:endyear_plot),
     ylim = c(.7, 1.3),
     xlim = c(1949, 2020),
     xlab = NA,
     ylab = NA,
     cex = .5,
     cex.lab = 1,
     cex.main = .7,
     cex.axis = .9,
     xaxs="i",
     bg='grey60',
     col = 'grey30',
     pch = 21)

axis(side = 4)

minor.tick(nx = 10, ny = 2)

f3 <- rep(1/lag_length, lag_length)
#start the moving average early to account for lag length
y_lag <- stats::filter(mean_temp[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
#plot the moving average line, but offset by two years to account for the NAs produced
# by filter() for the first few years
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag, col="darkgray",
      lty = 1, 
      lwd = 2.5)
y_lag2 <- stats::filter(quant25[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag2, col="black")
y_lag3 <- stats::filter(quant975[c((startindex_plot-(lag_length-1)):endindex_plot)], f3, sides=1)
lines(c((startyear_plot-(lag_length-1)):endyear_plot), y_lag3, col="black")

polygon(c(c((startyear_plot-(lag_length-1)):endyear_plot), rev(c((startyear_plot-(lag_length-1)):endyear_plot))),
        c(y_lag2, rev(y_lag3)),
        col = addTrans("grey20",40), border = NA)

abline(h = mean(mean_temp))

abline(v = 2005, lty = 3)
abline(v = 2015, lty = 3)

title(ylab = "Temp. anomaly")

mtext(side = 1, text = "Year", line = 2)


dev.off()


