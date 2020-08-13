# Plot-level understory analysis
# author: Sam Flake
# email: sflake@gmail.com
# Description: this script generates Figures 2, 3, and 4. It relies on the effects
# package or on code borrowed from that package. You'll have to run plot_level_understory.R
# first, because this script needs a lot of global variables generated during that
# analysis. Outputs some .tiff files. 

library(visreg)
library(car)
library(effects)
library(lme4)

pardefault <- par(no.readonly = T)
source('addTrans.R', echo=FALSE)

# set some parameters to be use throughout
# These models were generated in plot_level_understory.R, and that
# script needs to be run first and these models saved in the global environment.
# Some other global variables are also needed, like the raw plot_data data frame.
# Bad practices but I didn't think it was worth refactoring this thing. 

all <- all_plot
cg <- cheatgrass_plot
pg <- pgrass_plot
pf <- pforb_plot
sh <- shrub_plot

# function to calculate standard errors for a given vcov matrix and row of x values, to get
# point estimates of prediction error for a given prediction. See http://www.ats.ucla.edu/stat/r/faq/deltamethod.htm
# This gets used for the confidence interval of the effects plot

calc.se <- function(x){
  sqrt(as.matrix(x) %*% vcov(model) %*% t(x))
} 

inv.as <- function(x){
  (sin(x))^2
}

# function to find the nearest y-value for an x data point, to add to partial residuals
# for plotting. Stolen from effects package
closest <- function(x, x0){
  apply(outer(x, x0, FUN = function(x, x0) abs(x - x0)), 1, which.min)
}

#####################################################################
# All understory vegetation, just two panels
# Figure 3
#####################################################################
vars <- c("Tree_cover", "Delta_pdc")
labels <- c("Tree cover", "Change in canopy (%)")

tiff(filename="./outputs/Figure_3_all_understory_effects.tiff", 
     type="cairo",
     units="in", 
     width = 3, 
     height=5, 
     pointsize=12,
     compression = "lzw", 
     res=600)

par(mfrow = c(2,1), oma = c(2,3,0,0), mar = c(3,1,1,1), bty = 'n')

for(i in c(1:2)){
  var <- vars[i]
  
  eff <- Effect(all_plot, partial.residuals = TRUE, focal.predictors = var)
  
  y <- eff$fit
  x <- eff$x[[var]]
  x.fit <- eff$x.all[[var]]
  
  fitted <- y[closest(x.fit, x)]
  resids <- inv.as(eff$residuals + fitted)
  
  plot(NA,
       xlim = c(min(x), max(x)),
       ylim = c(0, 50),
       xlab = "",
       ylab = "")
  
  points(resids*100 ~ x.fit, 
         pch = 21, 
         bg='grey60',
         col = 'grey30')
  
  lines(inv.as(y)*100 ~ x, lwd = 2)
  lines(inv.as(eff$lower)*100 ~ x)
  lines(inv.as(eff$upper)*100 ~ x)
  polygon(c(x, rev(x)), c(inv.as(eff$upper)*100, rev(inv.as(eff$lower))*100),
          col = addTrans("#68EBC4",30), border = NA)

  mtext(side = 2, text = "Understory cover (%)", outer = TRUE, line = 1.7)
  mtext(side = 1, text = labels[i], line = 2)
}

dev.off()


####################################################################
## Multipanel figure to compare between functional types
# Figure 4
####################################################################
# for unscaling variables -- these things come from a full model in the model set, you'll have to change
# the number or the path depending upon what kind of model object and the order of your models, etc.
# You can also just get them from the raw data.
tc_mean <- mean(plot_data$Tree_cover)
tc_sd <- sd(plot_data$Tree_cover)

cwd_mean <-  mean(plot_data$cwd_normal_cum)
cwd_sd <- sd(plot_data$cwd_normal_cum)

Delta_pdc_mean <-  mean(plot_data$Delta_tc)
Delta_pdc_sd <- sd(plot_data$Delta_tc)

#--------------------------------------------------------------------------
## Calcuate partial residuals, effects
#--------------------------------------------------------------------------

C_tc <- data.frame(
  Tree_cover = seq(min(plot_data$Tree_cover), max(plot_data$Tree_cover), length.out = 200),
  cwd_normal_cum = mean(plot_data$cwd_normal_cum),
  Delta_pdc = mean(plot_data$Delta_pdc),
  AWC = mean(plot_data$AWC)
)

C_awc <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = mean(plot_data$cwd_normal_cum),
  Delta_pdc = mean(plot_data$Delta_pdc),
  AWC = seq(min(plot_data$AWC), max(plot_data$AWC), length.out = 200)
)

C_tc_10 <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = unname(quantile(plot_data$cwd_normal_cum, .1)),
  Delta_pdc = seq(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc), length.out = 200),
  AWC = mean(plot_data$AWC)
)

C_tc_50 <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = unname(quantile(plot_data$cwd_normal_cum, .5)),
  Delta_pdc = seq(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc), length.out = 200),
  AWC = mean(plot_data$AWC)
)

C_tc_90 <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = unname(quantile(plot_data$cwd_normal_cum, .9)),
  Delta_pdc = seq(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc), length.out = 200),
  AWC = mean(plot_data$AWC)
)


# model <- cg
# C <- C_tc
# predictor <- "Tree_cover"
calculate_preds <- function(model, C, predictor){
  preds <- data.frame(
    predictions = predict(model, re.form = NA, newdata = C, type = "response"),
    predictor = C[, predictor]
  )
  
  return(preds)
  
}

# Use the function to get all the stuff we need 
# (predictions, partial residuals, and SEs)
# this is the dumbest way to do this but I don't want to refactor it
preds_tc_cg <- calculate_preds(cg, C_tc, "Tree_cover")
preds_tc_pg <- calculate_preds(pg, C_tc, "Tree_cover")
preds_tc_pf <- calculate_preds(pf, C_tc, "Tree_cover")
preds_tc_sh <- calculate_preds(sh, C_tc, "Tree_cover")

preds_awc_cg <- calculate_preds(cg, C_awc, "AWC")
preds_awc_pg <- calculate_preds(pg, C_awc, "AWC")
preds_awc_pf <- calculate_preds(pf, C_awc, "AWC")
preds_awc_sh <- calculate_preds(sh, C_awc, "AWC")

preds_tc_10_cg <- calculate_preds(cg, C_tc_10, "Delta_pdc")
preds_tc_10_pg <- calculate_preds(pg, C_tc_10, "Delta_pdc")
preds_tc_10_pf <- calculate_preds(pf, C_tc_10, "Delta_pdc")
preds_tc_10_sh <- calculate_preds(sh, C_tc_10, "Delta_pdc")

preds_tc_50_cg <- calculate_preds(cg, C_tc_50, "Delta_pdc")
preds_tc_50_pg <- calculate_preds(pg, C_tc_50, "Delta_pdc")
preds_tc_50_pf <- calculate_preds(pf, C_tc_50, "Delta_pdc")
preds_tc_50_sh <- calculate_preds(sh, C_tc_50, "Delta_pdc")

preds_tc_90_cg <- calculate_preds(cg, C_tc_90, "Delta_pdc")
preds_tc_90_pg <- calculate_preds(pg, C_tc_90, "Delta_pdc")
preds_tc_90_pf <- calculate_preds(pf, C_tc_90, "Delta_pdc")
preds_tc_90_sh <- calculate_preds(sh, C_tc_90, "Delta_pdc")

opar <- par(no.readonly = TRUE)
par(opar)

tiff(filename="./outputs/Figure_4_understory_effects.tiff", 
    type="cairo",
    units="in", 
    width = 7, 
    height=5, 
    pointsize=15,
    compression = "lzw",
    res=600)

layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))

par(oma = c(2,4,0,0), mar = c(3,1,1,1), bty = 'n')


plot(NA,
     ylim = c(0,.12),
     xlim = c(I(min(plot_data$Tree_cover)*100), I(max(plot_data$Tree_cover)*100)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')

lines(inv.as(preds_tc_cg$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 2, col = "#1b9e77")
points(I(preds_tc_cg$predictor[c(1,200)]*100), inv.as(preds_tc_cg$predictions[c(1,200)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_tc_pg$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 2, col = "#000000")
points(I(preds_tc_cg$predictor[c(1,200)]*100), inv.as(preds_tc_pg$predictions[c(1,200)]), pch = 22, col = "#000000", bg = "#000000")
lines(inv.as(preds_tc_pf$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 1, col = "#E69F00")
points(I(preds_tc_cg$predictor[c(1,200)]*100), inv.as(preds_tc_pf$predictions[c(1,200)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_tc_sh$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 1, col = "#56B4E9")
points(I(preds_tc_cg$predictor[c(1,200)]*100), inv.as(preds_tc_sh$predictions[c(1,200)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
mtext(text = "Tree cover (%)", side = 1, line = 2.2)
mtext(text = "Understory cover (%)", side = 2, outer = TRUE, line = 2, cex = 1)
mtext(text = "(a)", side = 1, line = -10, adj = 0.05)

plot(NA,
     ylim = c(0,.1),
     xlim = c(min(plot_data$AWC), max(plot_data$AWC)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(inv.as(preds_awc_cg$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#1b9e77")
points(I(preds_awc_cg$predictor[c(1,200)]), inv.as(preds_awc_cg$predictions[c(1,200)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_awc_pg$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#000000")
points(I(preds_awc_cg$predictor[c(1,200)]), inv.as(preds_awc_pg$predictions[c(1,200)]), pch = 22, col = "#000000", bg = "#000000")
lines(inv.as(preds_awc_pf$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_awc_cg$predictor[c(1,200)]), inv.as(preds_awc_pf$predictions[c(1,200)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_awc_sh$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_awc_cg$predictor[c(1,200)]), inv.as(preds_awc_sh$predictions[c(1,200)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
mtext(text = "Soil AWC (%)", side = 1, line = 2.2)
mtext(text = "(b)", side = 1, line = -10, adj = 0.05)


plot.new()
legend("topright", legend = c("Cheatgrass", "Per. Grass", "Per. Forb", "Shrub"), 
       lty = 1, pch = c(21,22,23,24), lwd = 2, cex = 1.3, col = c("#1b9e77", "#000000", "#E69F00", "#56B4E9"),
       pt.bg = c("#1b9e77", "#000000", "#E69F00", "#56B4E9"))

plot(NA,
     ylim = c(0,.25),
     xlim = c(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(inv.as(preds_tc_10_cg$predictions) ~ I(preds_tc_10_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
points(I(preds_tc_10_cg$predictor[c(1,200)]), inv.as(preds_tc_10_cg$predictions[c(1,200)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_tc_10_pg$predictions) ~ I(preds_tc_10_cg$predictor), lwd = 2, lty = 1, col = "#000000")
points(I(preds_tc_10_cg$predictor[c(1,200)]), inv.as(preds_tc_10_pg$predictions[c(1,200)]), pch = 22, col = "#000000", bg = "#000000")
lines(inv.as(preds_tc_10_pf$predictions) ~ I(preds_tc_10_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_tc_10_cg$predictor[c(1,200)]), inv.as(preds_tc_10_pf$predictions[c(1,200)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_tc_10_sh$predictions) ~ I(preds_tc_10_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_tc_10_cg$predictor[c(1,200)]), inv.as(preds_tc_10_sh$predictions[c(1,200)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
text(x = -30, y = .18, labels = "10% CWD", cex = 1.3)
mtext(text = "(c)", side = 1, line = -10, adj = 0.05)

plot(NA,
     ylim = c(0,.25),
     xlim = c(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(inv.as(preds_tc_50_cg$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
points(I(preds_tc_90_cg$predictor[c(1,200)]), inv.as(preds_tc_50_cg$predictions[c(1,200)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_tc_50_pg$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 1, col = "#000000")
points(I(preds_tc_50_cg$predictor[c(1,200)]), inv.as(preds_tc_50_pg$predictions[c(1,200)]), pch = 22, col = "#000000", bg = "#000000")
lines(inv.as(preds_tc_50_pf$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_tc_50_cg$predictor[c(1,200)]), inv.as(preds_tc_50_pf$predictions[c(1,200)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_tc_50_sh$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_tc_50_cg$predictor[c(1,200)]), inv.as(preds_tc_50_sh$predictions[c(1,200)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
text(x = -30, y = .18, labels = "50% CWD", cex = 1.3)
mtext(text = "Change in live canopy (%)", side = 1, line = 2.2)
mtext(text = "(d)", side = 1, line = -10, adj = 0.05)


plot(NA,
     ylim = c(0,.25),
     xlim = c(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(inv.as(preds_tc_90_cg$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
points(I(preds_tc_90_cg$predictor[c(1,200)]), inv.as(preds_tc_90_cg$predictions[c(1,200)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_tc_90_pg$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 1, col = "#000000")
points(I(preds_tc_90_cg$predictor[c(1,200)]), inv.as(preds_tc_90_pg$predictions[c(1,200)]), pch = 22, col = "#000000", bg = "#000000")
lines(inv.as(preds_tc_90_pf$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_tc_90_cg$predictor[c(1,200)]), inv.as(preds_tc_90_pf$predictions[c(1,200)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_tc_90_sh$predictions) ~ I(preds_tc_90_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_tc_90_cg$predictor[c(1,200)]), inv.as(preds_tc_90_sh$predictions[c(1,200)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
text(x = -30, y = .18, labels = "90% CWD", cex = 1.3)
mtext(text = "(e)", side = 1, line = -10, adj = 0.05)


dev.off()

par(pardefault)


###################################################################################
# Cheatgrass change over time
# Figure 2
###################################################################################
## new data
model <- cg_change_lm
model2 <- cg_change_lm_no_outliers

# model <- model2

compare$delta_cg <- compare$Cheatgrass - compare$Cheatgrass.Cover


compare2 <- compare[-c(5, 10), ]
compare_outliers <- compare[c(5,10), ]

#create dataframe of new data to calculate predictions and envelope
C_cg_change_10 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .15)),
  Delta_tc = seq(-50,10, length.out = 200)
)

C_cg_change_50 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .5)),
  Delta_tc = seq(-50,10, length.out = 200)
)


C_cg_change_90 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .85)),
  Delta_tc = seq(-50,10, length.out = 200)
)

C_cg <- list(C_cg_change_10, C_cg_change_50, C_cg_change_90)

#function to calculate predictions and envelope
preds_cg_change <- function(model, C, predictor){
    preds <- data.frame(
    predictions = predict(model, se = TRUE, newdata = C, type = "response")$fit,
    lo = predict(model, se = TRUE, newdata = C)$fit - 1.96*predict(model, se = TRUE, newdata = C)$se.fit,
    hi = predict(model, se = TRUE, newdata = C)$fit + 1.96*predict(model, se = TRUE, newdata = C)$se.fit,
    predictor = C[, predictor]
  )
  
  return(preds)
}

#calculate predictions and prediction envelope for each level of CWD
cg_change_10 <- preds_cg_change(cg_change_lm, C_cg_change_10, "Delta_tc")
cg_change_50 <- preds_cg_change(cg_change_lm, C_cg_change_50, "Delta_tc")
cg_change_90 <- preds_cg_change(cg_change_lm, C_cg_change_90, "Delta_tc")

cg_change_10_no_out <- preds_cg_change(cg_change_lm_no_outliers, C_cg_change_10, "Delta_tc")
cg_change_50_no_out <- preds_cg_change(cg_change_lm_no_outliers, C_cg_change_50, "Delta_tc")
cg_change_90_no_out <- preds_cg_change(cg_change_lm_no_outliers, C_cg_change_90, "Delta_tc")

cg_change <- list(cg_change_10, cg_change_50, cg_change_90)
cg_change_no_out <- list(cg_change_10_no_out, cg_change_50_no_out, cg_change_90_no_out)

#generate the figure
tiff(filename="./outputs/Figure_2_cheatgrass_change_effect.tif", 
    type="cairo",
    units="in", 
    width = 4, 
    height=3, 
    pointsize=12,
    compression = "lzw", 
    res=600)

layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE))
par(mar = c(0,1,0,0), oma = c(6,4,2,2), bty = 'n')

panel_labels <- c("10% CWD", "50% CWD", "90% CWD")

for(i in c(1:3)){
  plot(NA,
       ylim = c(-0.06, 0.16),
       xlim = c(-70, 10 ),
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex = 0.8,
       cex.lab = 1.5)
  abline(h = 0, lty = 4, lwd = 1.3)
  
  lines(cg_change[[i]]$predictions ~ cg_change[[i]]$predictor, lwd = 2, lty = 4, col = "#1b9e77")
  lines(cg_change_no_out[[i]]$predictions ~ cg_change[[i]]$predictor, lwd = 2, lty = 1, col = "#1b9e77")
  lines(cg_change_no_out[[i]]$lo ~ cg_change[[i]]$predictor, lwd = 1, lty = 1, col = "#1b9e77")
  lines(cg_change_no_out[[i]]$hi ~ cg_change[[i]]$predictor, lwd = 1, lty = 1, col = "#1b9e77")
  polygon(c(cg_change_no_out[[i]]$predictor, rev(cg_change_no_out[[i]]$predictor)), c(cg_change_no_out[[i]]$hi, 
                                                                    rev(cg_change_no_out[[i]]$lo)),
          col = addTrans("#68EBC4",30), border = NA) #fills in the area between high and low confidence intervals
  
  # I also hate this: plot the residuals ~ delta_tc for each level of CWD
      if(i == 1){
        points(compare2[compare2$cwd_normal_cum < unname(quantile(compare2$cwd_normal_cum, 0.3)),]$delta_cg/100 ~ 
          compare2[compare2$cwd_normal_cum < unname(quantile(compare2$cwd_normal_cum, 0.3)),]$Delta_tc,
          bg='grey60',
          col = 'grey30',
          pch = 21,
          cex = 1.2)
      }else if(i ==2){
        points(compare2[findInterval(compare2$cwd_normal_cum, unname(quantile(compare2$cwd_normal_cum, c(0.3, 0.7)))) == 1,]$delta_cg/100 ~ 
          compare2[findInterval(compare2$cwd_normal_cum, unname(quantile(compare2$cwd_normal_cum, c(0.3, 0.7)))) == 1,]$Delta_tc,
          bg='grey60',
          col = 'grey30',
          pch = 21,
          cex = 1.2)
        points(compare_outliers[1, ]$delta_cg/100 ~ compare_outliers[1, ]$Delta_tc, 
               bg='white',
               col = 'grey30',
               pch = 21,
               cex = 1.2)
      }else if (i == 3){
        points(compare2[compare2$cwd_normal_cum > unname(quantile(compare2$cwd_normal_cum, 0.7)),]$delta_cg/100 ~ 
          compare2[compare2$cwd_normal_cum > unname(quantile(compare2$cwd_normal_cum, 0.7)),]$Delta_tc,
          bg='grey60',
          col = 'grey30',
          pch = 21,
          cex = 1.2)
        points(compare_outliers[2, ]$delta_cg/100 ~ compare_outliers[2, ]$Delta_tc, 
               bg='white',
               col = 'grey30',
               pch = 21,
               cex = 1.2)
      }
  
 if(i == 1){
   axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
   # axis(side = 2, at = c(-0.1, 0, 0.1), labels = c(-10, 0, 10))
   }
 axis(side = 1, at = c(-40, -20, 0), labels = TRUE)
 axis(side = 1, at = c(-50, -30, -10, 10), labels = FALSE, tcl = -.25)
 text(x = -30, y = .07, labels = panel_labels[i], cex = 1.2)

}
   
  mtext(side = 1, text = "Change in live canopy (%)", outer = TRUE, line = 2.6)
  mtext(side = 2, text = "Change in cheatgrass cover (%)", outer = TRUE, line = 2.3)

dev.off()



#------------------------------------------------------------------------------
# Effects plots with partial residuals for supplement
#------------------------------------------------------------------------------
library("effects")
# Perennial grasses

tiff(filename="./outputs/Supplementary figure effects cgrass.tiff", 
     type="cairo",
     units="in", 
     width = 7, 
     height=5, 
     pointsize=15,
     compression = "lzw",
     res=600)

#graphical params
layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))

par(oma = c(2,4,0,0), mar = c(3,1,1,1), bty = 'n')


panel_labels <- c("10% CWD", "50% CWD", "90% CWD")
k <- 1

#which  model to use?
model <- pgrass_plot_noscale

let <- letters

vars <- c("Tree_cover", "AWC", "Delta_tc", "Delta_tc", "Delta_tc")
x_labels <- c("Tree cover (%)", "Soil AWC (%)", "Change in live canopy (%)")

for(i in 1:2){

  var <- vars[i]
  
  eff <- Effect(model, partial.residuals = TRUE, focal.predictors = var)
  
  line_data <- data.frame(y = inv.as(eff$fit),
                    lower = inv.as(eff$lower),
                    upper = inv.as(eff$upper),
                    x = eff[["x"]][[var]]
                    )
  
  x_new <- eff[["x"]][[var]]
  x_orig <- eff[["x.all"]][[var]]
  
  y_trans <- eff$fit
  
  fitted <- y_trans[closest(x_orig, x_new)] #match original data to the nearest new x value, so
  # that we can find the closest fitted value and add the residual for that 
  
  p_resids <- inv.as(eff$residuals + fitted)
  # eff[["data"]][[var]]
  
  points_data <- data.frame(x = x_orig,
                            y = p_resids)
  
  plot(NA,
       ylim = c(min(c(y, points_data$y)), max(c(y, points_data$y))),
       xlim = c(min(x_new), max(x_new)),
       xlab = "",
       ylab = "",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex.lab = 1.5,
       bty = 'n',
       xaxt = 'n',
       yaxt = 'n')
  
  lines(line_data$y ~ line_data$x, lwd = 2, lty = 1, col = "#1b9e77")
  lines(line_data$upper ~ line_data$x)
  lines(line_data$lower ~ line_data$x)
  polygon(c(line_data$x, rev(line_data$x)), 
          c(line_data$upper, rev(line_data$lower)),
          col = addTrans("#68EBC4",30), border = NA)
  
  points(points_data$y ~ points_data$x,
         pch = 21, col = "#1b9e77", bg = "#1b9e77")
  
  axis(side = 1)
  axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
  mtext(text = var, side = 1, line = 2.2)
  mtext(text = "Understory cover (%)", side = 2, outer = TRUE, line = 2, cex = 1)
  mtext(text = "(a)", side = 1, line = -10, adj = 0.05)

}

plot(NA)

for(i in 1:3){
  
  eff <- Effect(model, partial.residuals = TRUE, focal.predictors = c("Delta_tc", "cwd_normal_cum"),
                xlevels = list(Delta_tc = 100),
                quantiles = c(0.1, 0.5, 0.9))
  
  cwd_levels <- eff$variables$cwd_normal_cum$levels
  
  line_data <- data.frame(y = inv.as(eff$fit[1:99 + (i-1) * 100]),
                          lower = inv.as(eff$lower[1:99 + (i-1) * 100]),
                          upper = inv.as(eff$upper[1:99 + (i-1) * 100]),
                          x = eff[["x"]][["Delta_tc"]][1:99 + (i-1) * 100]
  )
  
  x_new <- eff[["x"]][["Delta_tc"]]
  x_orig_all <- eff[["x.all"]]
  which_x <- x_orig_all$cwd_normal_cum - cwd_levels[i] < 0.001
  x_orig <- x_orig_all[which_x, ][["Delta_tc"]]
  
  y_trans <- eff$fit[1:99 + (i-1) * 100]
  
  fitted <- y_trans[closest(x_orig, x_new)] #match original data to the nearest new x value, so
  # that we can find the closest fitted value and add the residual for that 
  
  p_resids <- inv.as(eff$residuals[which_x] + fitted)
  
  points_data <- data.frame(x = x_orig,
                            y = p_resids)
  
  plot(NA,
       ylim = c(0, max(c(y, points_data$y), na.rm = TRUE)),
       xlim = c(min(x_new), max(x_new)),
       xlab = "",
       ylab = "",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex.lab = 1.5,
       bty = 'n',
       xaxt = 'n',
       yaxt = 'n')
  
  lines(line_data$y ~ line_data$x, lwd = 2, lty = 1, col = "#1b9e77")
  lines(line_data$upper ~ line_data$x)
  lines(line_data$lower ~ line_data$x)
  polygon(c(line_data$x, rev(line_data$x)), 
          c(line_data$upper, rev(line_data$lower)),
          col = addTrans("#68EBC4",30), border = NA)
  
  points(points_data$y ~ points_data$x,
         pch = 21, col = "#1b9e77", bg = "#1b9e77")
  
  axis(side = 1)
  axis(side = 2, at = axTicks(2), labels = axTicks(2)*100)
  mtext(text = var, side = 1, line = 2.2)
  mtext(text = "Understory cover (%)", side = 2, outer = TRUE, line = 2, cex = 1)
  mtext(text = "(a)", side = 1, line = -10, adj = 0.05)

}

dev.off()

