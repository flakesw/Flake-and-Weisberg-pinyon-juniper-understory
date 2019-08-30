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
# Bad practices but whatcha gonna do. 

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
labels <- c("Tree cover", "Change in live canopy (%)")

tiff(filename="./outputs/all_understory_effects.tiff", 
     type="cairo",
     units="in", 
     width = 3, 
     height=5, 
     pointsize=12, 
     res=600)

par(mfrow = c(2,1), oma = c(2,3,0,0), mar = c(3,1,1,1), family = "serif", bty = 'n')

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
       ylim = c(0, 0.5),
       xlab = "",
       ylab = "")
  
  points(resids ~ eff$x.all[[var]], 
         pch = 21, 
         bg='grey60',
         col = 'grey30')
  
  lines(inv.as(y) ~ x, lwd = 2)
  lines(inv.as(eff$lower) ~ x)
  lines(inv.as(eff$upper) ~ x)
  polygon(c(x, rev(x)), c(inv.as(eff$upper), rev(inv.as(eff$lower))),
          col = addTrans("#68EBC4",30), border = NA)

  mtext(side = 2, text = "Understory cover", outer = TRUE, line = 1.7)
  mtext(side = 1, text = labels[i], line = 2)
}

dev.off()


####################################################################
## Multipanel figure to compare between functional types
# Figure 4
####################################################################

# Calculate residuals from predicted values and observed values
resid <- residuals(model)

# for unscaling variables -- these things come from a full model in the model set, you'll have to change
# the number or the path depending upon what kind of model object and the order of your models, etc.
# You can also just get them from the raw data.
tc_mean <- mean(plot_data$Tree_cover)
tc_sd <- sd(plot_data$Tree_cover)

cwd_mean <-  mean(plot_data$cwd_normal_cum)
cwd_sd <- sd(plot_data$cwd_normal_cum)

Delta_pdc_mean <-  mean(plot_data$Delta_pdc)
Delta_pdc_sd <- sd(plot_data$Delta_pdc)

#--------------------------------------------------------------------------
## Calcuate partial residuals, effects
#--------------------------------------------------------------------------

C_tc <- data.frame(
  Tree_cover = seq(min(plot_data$Tree_cover), max(plot_data$Tree_cover), length.out = 102),
  cwd_normal_cum = mean(plot_data$cwd_normal_cum),
  Delta_pdc = mean(plot_data$Delta_pdc),
  AWC = mean(plot_data$AWC)
)

C_awc <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = mean(plot_data$cwd_normal_cum),
  Delta_pdc = mean(plot_data$Delta_pdc),
  AWC = seq(min(plot_data$AWC), max(plot_data$AWC), length.out = 102)
)

C_pdc_10 <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = unname(quantile(plot_data$cwd_normal_cum, .1)),
  Delta_pdc = seq(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc), length.out = 102),
  AWC = mean(plot_data$AWC)
)

C_pdc_50 <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = unname(quantile(plot_data$cwd_normal_cum, .5)),
  Delta_pdc = seq(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc), length.out = 102),
  AWC = mean(plot_data$AWC)
)

C_pdc_90 <- data.frame(
  Tree_cover = mean(plot_data$Tree_cover),
  cwd_normal_cum = unname(quantile(plot_data$cwd_normal_cum, .9)),
  Delta_pdc = seq(min(plot_data$Delta_pdc), max(plot_data$Delta_pdc), length.out = 102),
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
# this is the dumbest way to do this but I don't want to rewrite it
preds_tc_cg <- calculate_preds(cg, C_tc, "Tree_cover")
preds_tc_pg <- calculate_preds(pg, C_tc, "Tree_cover")
preds_tc_pf <- calculate_preds(pf, C_tc, "Tree_cover")
preds_tc_sh <- calculate_preds(sh, C_tc, "Tree_cover")

preds_awc_cg <- calculate_preds(cg, C_awc, "AWC")
preds_awc_pg <- calculate_preds(pg, C_awc, "AWC")
preds_awc_pf <- calculate_preds(pf, C_awc, "AWC")
preds_awc_sh <- calculate_preds(sh, C_awc, "AWC")

preds_pdc_10_cg <- calculate_preds(cg, C_pdc_10, "Delta_pdc")
preds_pdc_10_pg <- calculate_preds(pg, C_pdc_10, "Delta_pdc")
preds_pdc_10_pf <- calculate_preds(pf, C_pdc_10, "Delta_pdc")
preds_pdc_10_sh <- calculate_preds(sh, C_pdc_10, "Delta_pdc")

preds_pdc_50_cg <- calculate_preds(cg, C_pdc_50, "Delta_pdc")
preds_pdc_50_pg <- calculate_preds(pg, C_pdc_50, "Delta_pdc")
preds_pdc_50_pf <- calculate_preds(pf, C_pdc_50, "Delta_pdc")
preds_pdc_50_sh <- calculate_preds(sh, C_pdc_50, "Delta_pdc")

preds_pdc_90_cg <- calculate_preds(cg, C_pdc_90, "Delta_pdc")
preds_pdc_90_pg <- calculate_preds(pg, C_pdc_90, "Delta_pdc")
preds_pdc_90_pf <- calculate_preds(pf, C_pdc_90, "Delta_pdc")
preds_pdc_90_sh <- calculate_preds(sh, C_pdc_90, "Delta_pdc")

# create plots of log-partial-residuals y-axis versus natural x-scale
# this is also dumb and I could do it in a loop except I don't want to 
# put all the predictions in a list
setwd("C:/Users/Sam/Documents/Research/MS Thesis/Understory/outputs") #where the plots go

opar <- par(no.readonly = TRUE)
par(opar)

tiff(filename="understory_effects.tiff", 
    type="cairo",
    units="in", 
    width = 7, 
    height=5, 
    pointsize=15, 
    res=600)

layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))

par(oma = c(2,4,0,0), mar = c(3,1,1,1), family = "serif", bty = 'n')


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
points(I(preds_tc_cg$predictor[c(1,102)]*100), inv.as(preds_tc_cg$predictions[c(1,102)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_tc_pg$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 2, col = "#000000")
points(I(preds_tc_cg$predictor[c(1,102)]*100), inv.as(preds_tc_pg$predictions[c(1,102)]), pch = 22, col = "#000000", bg = "#000000")
lines(inv.as(preds_tc_pf$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 1, col = "#E69F00")
points(I(preds_tc_cg$predictor[c(1,102)]*100), inv.as(preds_tc_pf$predictions[c(1,102)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_tc_sh$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 1, col = "#56B4E9")
points(I(preds_tc_cg$predictor[c(1,102)]*100), inv.as(preds_tc_sh$predictions[c(1,102)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2)
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
points(I(preds_awc_cg$predictor[c(1,102)]), inv.as(preds_awc_cg$predictions[c(1,102)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_awc_pg$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#000000")
points(I(preds_awc_cg$predictor[c(1,102)]), inv.as(preds_awc_pg$predictions[c(1,102)]), pch = 21, col = "#000000", bg = "#000000")
lines(inv.as(preds_awc_pf$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_awc_cg$predictor[c(1,102)]), inv.as(preds_awc_pf$predictions[c(1,102)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_awc_sh$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_awc_cg$predictor[c(1,102)]), inv.as(preds_awc_sh$predictions[c(1,102)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2)
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
lines(inv.as(preds_pdc_10_cg$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
points(I(preds_pdc_10_cg$predictor[c(1,102)]), inv.as(preds_pdc_10_cg$predictions[c(1,102)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_pdc_10_pg$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 1, col = "#000000")
points(I(preds_pdc_10_cg$predictor[c(1,102)]), inv.as(preds_pdc_10_pg$predictions[c(1,102)]), pch = 21, col = "#000000", bg = "#000000")
lines(inv.as(preds_pdc_10_pf$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_pdc_10_cg$predictor[c(1,102)]), inv.as(preds_pdc_10_pf$predictions[c(1,102)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_pdc_10_sh$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_pdc_10_cg$predictor[c(1,102)]), inv.as(preds_pdc_10_sh$predictions[c(1,102)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2)
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
lines(inv.as(preds_pdc_50_cg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
points(I(preds_pdc_90_cg$predictor[c(1,102)]), inv.as(preds_pdc_50_cg$predictions[c(1,102)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_pdc_50_pg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 1, col = "#000000")
points(I(preds_pdc_50_cg$predictor[c(1,102)]), inv.as(preds_pdc_50_pg$predictions[c(1,102)]), pch = 21, col = "#000000", bg = "#000000")
lines(inv.as(preds_pdc_50_pf$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_pdc_50_cg$predictor[c(1,102)]), inv.as(preds_pdc_50_pf$predictions[c(1,102)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_pdc_50_sh$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_pdc_50_cg$predictor[c(1,102)]), inv.as(preds_pdc_50_sh$predictions[c(1,102)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2)
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
lines(inv.as(preds_pdc_90_cg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
points(I(preds_pdc_90_cg$predictor[c(1,102)]), inv.as(preds_pdc_90_cg$predictions[c(1,102)]), pch = 21, col = "#1b9e77", bg = "#1b9e77")
lines(inv.as(preds_pdc_90_pg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 1, col = "#000000")
points(I(preds_pdc_90_cg$predictor[c(1,102)]), inv.as(preds_pdc_90_pg$predictions[c(1,102)]), pch = 21, col = "#000000", bg = "#000000")
lines(inv.as(preds_pdc_90_pf$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 2, col = "#E69F00")
points(I(preds_pdc_90_cg$predictor[c(1,102)]), inv.as(preds_pdc_90_pf$predictions[c(1,102)]), pch = 23, col = "#E69F00", bg = "#E69F00")
lines(inv.as(preds_pdc_90_sh$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 2, col = "#56B4E9")
points(I(preds_pdc_90_cg$predictor[c(1,102)]), inv.as(preds_pdc_90_sh$predictions[c(1,102)]), pch = 24, col = "#56B4E9", bg = "#56B4E9")

axis(side = 1)
axis(side = 2)
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

#create dataframe of new data to calculate predictions and envelope
C_cg_change_10 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .1)),
  Delta_pdc = seq(-70,15, length.out = 100)
)


C_cg_change_50 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .5)),
  Delta_pdc = seq(-70,15, length.out = 100)
)


C_cg_change_90 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .9)),
  Delta_pdc = seq(-70,15, length.out = 100)
)

C_cg <- list(C_cg_change_10, C_cg_change_50, C_cg_change_90)


# calculate partial residuals for different levels of cwd
# this is an utterly repugnant way to do this

part_resids_10 <- residuals(model) + model$model[, "Delta_pdc"] * coef(model)["Delta_pdc"] + 
  unname(quantile(compare$cwd_normal_cum, .1)) * coef(model)["cwd_normal_cum"] +
  model$model[, "Delta_pdc"] * unname(quantile(compare$cwd_normal_cum, .1)) * coef(model)["Delta_pdc:cwd_normal_cum"] +
  coef(model)["(Intercept)"] 

part_resids_50 <- residuals(model) + model$model[, "Delta_pdc"] * coef(model)["Delta_pdc"] + 
  unname(quantile(compare$cwd_normal_cum, .5)) * coef(model)["cwd_normal_cum"] +
  model$model[, "Delta_pdc"] * unname(quantile(compare$cwd_normal_cum, .5)) * coef(model)["Delta_pdc:cwd_normal_cum"] +
  coef(model)["(Intercept)"] 

part_resids_90 <- residuals(model) + model$model[, "Delta_pdc"] * coef(model)["Delta_pdc"] + 
  unname(quantile(compare$cwd_normal_cum, .9)) * coef(model)["cwd_normal_cum"] +
  model$model[, "Delta_pdc"] * unname(quantile(compare$cwd_normal_cum, .9)) * coef(model)["Delta_pdc:cwd_normal_cum"] +
  coef(model)["(Intercept)"] 

part_resids <- list(part_resids_10, part_resids_50, part_resids_90)

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
cg_change_10 <- preds_cg_change(cg_change_lm, C_cg_change_10, "Delta_pdc")
cg_change_50 <- preds_cg_change(cg_change_lm, C_cg_change_50, "Delta_pdc")
cg_change_90 <- preds_cg_change(cg_change_lm, C_cg_change_90, "Delta_pdc")

cg_change <- list(cg_change_10, cg_change_50, cg_change_90)


#generate the figure
tiff(filename="./outputs/cheatgrass_change_effect.tif", 
    type="cairo",
    units="in", 
    width = 4, 
    height=3, 
    pointsize=12, 
    res=600)

layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE))
par(mar = c(0,1,0,0), oma = c(6,4,2,2), family = "serif", bty = 'n')

panel_labels <- c("10% CWD", "50% CWD", "90% CWD")

for(i in c(1:3)){
  plot(NA,
       ylim = c(-0.1, 0.15),
       xlim = c(-70, 10 ),
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex.lab = 1.5)
  abline(h = 0, lty = 4, lwd = 1.3)
  lines(cg_change[[i]]$predictions ~ cg_change[[i]]$predictor, lwd = 2, lty = 1, col = "#1b9e77")
  lines(cg_change[[i]]$lo ~ cg_change[[i]]$predictor, lwd = 1, lty = 1, col = "#1b9e77")
  lines(cg_change[[i]]$hi ~ cg_change[[i]]$predictor, lwd = 1, lty = 1, col = "#1b9e77")
  polygon(c(cg_change[[i]]$predictor, rev(cg_change[[i]]$predictor)), c(cg_change[[i]]$hi, 
                                                                    rev(cg_change[[i]]$lo)),
          col = addTrans("#68EBC4",30), border = NA) #fills in the area between high and low confidence intervals
  
  # I also hate this: plot the residuals ~ delta_pdc for each level of CWD
    points(
      if(i == 1){
        part_resids[[i]][model$model[, "cwd_normal_cum"] < unname(quantile(compare$cwd_normal_cum, 0.3))] ~ 
           model$model[model$model[, "cwd_normal_cum"] < unname(quantile(compare$cwd_normal_cum, 0.3)), "Delta_pdc"]
      }else if(i ==2){
        part_resids[[i]][findInterval(model$model[, "cwd_normal_cum"], unname(quantile(compare$cwd_normal_cum, c(0.3, 0.7)))) == 1] ~ 
      model$model[findInterval(model$model[, "cwd_normal_cum"], unname(quantile(compare$cwd_normal_cum, c(0.3, 0.7)))) == 1, "Delta_pdc"]
      }else if (i == 3){
        part_resids[[i]][model$model[, "cwd_normal_cum"] > unname(quantile(compare$cwd_normal_cum, 0.7))] ~ 
          model$model[model$model[, "cwd_normal_cum"] > unname(quantile(compare$cwd_normal_cum, 0.7)), "Delta_pdc"]
      },
         bg='grey60',
         col = 'grey30',
         pch = 21,
         cex = 1.5)
  
 if(i == 1){
   axis(side = 2, at = c(-0.05, 0.05), labels = FALSE, tcl = -0.25)
   axis(side = 2, at = c(-0.1, 0, 0.1))
   }
 axis(side = 1, at = c(-60, -30, 0), labels = TRUE)
 axis(side = 1, at = c(-50, -40, -20, -10, 10), labels = FALSE, tcl = -.25)
 text(x = -30, y = .11, labels = panel_labels[i], cex = 1.2)

}
   
  mtext(side = 1, text = "Change in live canopy (%)", outer = TRUE, line = 2.6)
  mtext(side = 2, text = "Change in cheatgrass cover", outer = TRUE, line = 2.3)

dev.off()
