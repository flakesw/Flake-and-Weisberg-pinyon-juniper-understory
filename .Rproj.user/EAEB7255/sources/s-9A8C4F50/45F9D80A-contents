### Code to make effects plots

# TODO scheck what models are fit, fit them in another script and import them to here

library(visreg)
library(car)
library(effects)
library(lme4)

pardefault <- par(no.readonly = T)
source('addTrans.R', echo=FALSE)

### set some parameters to be use throughout
cg <- cheatgrass_ag
pg <- pgrass_ag
pf <- pforb_ag
sh <- shrub_ag

# function to calculate standard errors for a given vcov matrix and row of x values, to get
# point estimates of prediction error for a given prediction. See http://www.ats.ucla.edu/stat/r/faq/deltamethod.htm
# This gets used for the confidence interval of the effects plot

calc.se <- function(x){
  sqrt(as.matrix(x) %*% vcov(model) %*% t(x))
} 

##############################################
## Creating new predictions
##############################################

# Calculate residuals from predicted values and observed values
resid <- residuals(model)

# for unscaling variables -- these things come from a full model in the model set, you'll have to change
# the number or the path depending upon what kind of model object and the order of your models, etc.
# You can also just get them from the raw data.
tc_mean <- mean(ag_data$Tree_cover)
tc_sd <- sd(ag_data$Tree_cover)

cwd_mean <-  mean(ag_data$cwd_normal_cum)
cwd_sd <- sd(ag_data$cwd_normal_cum)

Delta_pdc_mean <-  mean(ag_data$Delta_pdc)
Delta_pdc_sd <- sd(ag_data$Delta_pdc)

################################
## Calcuate partial residuals, effects, for avg_BA
################################

model <- cg

C_tc <- data.frame(
  Tree_cover = seq(min(ag_data$Tree_cover), max(ag_data$Tree_cover), length.out = 100),
  cwd_normal_cum = mean(ag_data$cwd_normal_cum),
  Delta_pdc = mean(ag_data$Delta_pdc),
  AWC = mean(ag_data$AWC)
)

C_awc <- data.frame(
  Tree_cover = mean(ag_data$Tree_cover),
  cwd_normal_cum = mean(ag_data$cwd_normal_cum),
  Delta_pdc = mean(ag_data$Delta_pdc),
  AWC = seq(min(ag_data$AWC), max(ag_data$AWC), length.out = 100)
)

C_pdc_10 <- data.frame(
  Tree_cover = mean(ag_data$Tree_cover),
  cwd_normal_cum = unname(quantile(ag_data$cwd_normal_cum, .1)),
  Delta_pdc = seq(min(ag_data$Delta_pdc), max(ag_data$Delta_pdc), length.out = 100),
  AWC = mean(ag_data$AWC)
)

C_pdc_50 <- data.frame(
  Tree_cover = mean(ag_data$Tree_cover),
  cwd_normal_cum = unname(quantile(ag_data$cwd_normal_cum, .5)),
  Delta_pdc = seq(min(ag_data$Delta_pdc), max(ag_data$Delta_pdc), length.out = 100),
  AWC = mean(ag_data$AWC)
)

C_pdc_90 <- data.frame(
  Tree_cover = mean(ag_data$Tree_cover),
  cwd_normal_cum = unname(quantile(ag_data$cwd_normal_cum, .9)),
  Delta_pdc = seq(min(ag_data$Delta_pdc), max(ag_data$Delta_pdc), length.out = 100),
  AWC = mean(ag_data$AWC)
)



model <- cg
C <- C_tc
predictor <- "Tree_cover"
calculate_preds <- function(model, C, predictor){
  preds <- data.frame(
    predictions = predict(model, se = TRUE, newdata = C, type = "response")$fit,
    lo = predict(model, se = TRUE, newdata = C)$fit - 1.96*predict(model, se = TRUE, newdata = C)$se.fit,
    hi = predict(model, se = TRUE, newdata = C)$fit + 1.96*predict(model, se = TRUE, newdata = C)$se.fit,
    predictor = C[, predictor]
  )
  
  return(preds)
  
}

# Use the function to get all the stuff we need (predictions, partial residuals, and SEs)
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

setwd("C:\\Users\\Sam\\Google Drive\\Thesis related\\Understory\\outputs") #where the plots go

opar <- par(no.readonly = TRUE)
par(opar)

png(filename="understory_effects.png", 
    type="cairo",
    units="in", 
    width = 7, 
    height=5, 
    pointsize=15, 
    res=160)

layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))

par(oma = c(2,4,0,0), mar = c(3,1,1,1), family = "serif", bty = 'n')


plot(NA,
     ylim = c(0,.1),
     xlim = c(I(min(ag_data$Tree_cover)*100), I(max(ag_data$Tree_cover)*100)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(exp(preds_tc_cg$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 1, col = "#1b9e77")
lines(exp(preds_tc_pg$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 2, col = "#d95f02")
lines(exp(preds_tc_pf$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 3, col = "#7570b3")
lines(exp(preds_tc_sh$predictions) ~ I(preds_tc_cg$predictor*100), lwd = 2, lty = 4, col = "#e7298a")
axis(side = 1)
axis(side = 2)
mtext(text = "Tree Cover (%)", side = 1, line = 2.2)
mtext(text = "Predicted Cover", side = 2, outer = TRUE, line = 2, cex = 1)
mtext(text = "(a)", side = 1, line = -10, adj = 0.05)


plot(NA,
     ylim = c(0,.1),
     xlim = c(min(ag_data$AWC), max(ag_data$AWC)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(exp(preds_awc_cg$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
lines(exp(preds_awc_pg$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 2, col = "#d95f02")
lines(exp(preds_awc_pf$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 3, col = "#7570b3")
lines(exp(preds_awc_sh$predictions) ~ I(preds_awc_cg$predictor), lwd = 2, lty = 4, col = "#e7298a")
axis(side = 1)
axis(side = 2)
mtext(text = "Soil AWC (%)", side = 1, line = 2.2)
mtext(text = "(b)", side = 1, line = -10, adj = 0.05)


plot.new()
legend("topright", legend = c("Cheatgrass", "Perr. Grass", "Perr. Forb", "Shrub"), 
       lty = c(1,2,3,4), lwd = 2, cex = 1.3, col = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))

plot(NA,
     ylim = c(0,.1),
     xlim = c(min(ag_data$Delta_pdc), max(ag_data$Delta_pdc)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(exp(preds_pdc_10_cg$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
lines(exp(preds_pdc_10_pg$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 2, col = "#d95f02")
lines(exp(preds_pdc_10_pf$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 3, col = "#7570b3")
lines(exp(preds_pdc_10_sh$predictions) ~ I(preds_pdc_10_cg$predictor), lwd = 2, lty = 4, col = "#e7298a")
axis(side = 1)
axis(side = 2)
text(x = -30, y = .09, labels = "10% CWD", cex = 1.3)
mtext(text = "(c)", side = 1, line = -10, adj = 0.05)

plot(NA,
     ylim = c(0,.1),
     xlim = c(min(ag_data$Delta_pdc), max(ag_data$Delta_pdc)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(exp(preds_pdc_50_cg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
lines(exp(preds_pdc_50_pg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 2, col = "#d95f02")
lines(exp(preds_pdc_50_pf$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 3, col = "#7570b3")
lines(exp(preds_pdc_50_sh$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 4, col = "#e7298a")
axis(side = 1)
axis(side = 2)
text(x = -30, y = .09, labels = "50% CWD", cex = 1.3)
mtext(text = "Change in Live Canopy (%)", side = 1, line = 2.2)
mtext(text = "(d)", side = 1, line = -10, adj = 0.05)


plot(NA,
     ylim = c(0,.1),
     xlim = c(min(ag_data$Delta_pdc), max(ag_data$Delta_pdc)),
     xlab = "",
     ylab = "",
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex.lab = 1.5,
     bty = 'n',
     xaxt = 'n',
     yaxt = 'n')
lines(exp(preds_pdc_90_cg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 1, col = "#1b9e77")
lines(exp(preds_pdc_90_pg$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 2, col = "#d95f02")
lines(exp(preds_pdc_90_pf$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 3, col = "#7570b3")
lines(exp(preds_pdc_90_sh$predictions) ~ I(preds_pdc_90_cg$predictor), lwd = 2, lty = 4, col = "#e7298a")
axis(side = 1)
axis(side = 2)
text(x = -30, y = .09, labels = "90% CWD", cex = 1.3)
mtext(text = "(e)", side = 1, line = -10, adj = 0.05)


dev.off()

par(pardefault)


#----------------------------------------------------------------------------------
# Cheatgrass change over time
#----------------------------------------------------------------------------------
## new data
model <- 

C_cg_change_10 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .1)),
  Delta_pdc = seq(-60,15, length.out = 100)
)


C_cg_change_50 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .5)),
  Delta_pdc = seq(-60,15, length.out = 100)
)


C_cg_change_90 <- data.frame(
  cwd_normal_cum = unname(quantile(compare$cwd_normal_cum, .9)),
  Delta_pdc = seq(-60,15, length.out = 100)
)

#calculate partial residuals for different levels of cwd

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


#function to calculate predictions
preds_cg_change <- function(model, C, predictor){
    preds <- data.frame(
    predictions = predict(model, se = TRUE, newdata = C, type = "response")$fit,
    lo = predict(model, se = TRUE, newdata = C)$fit - 1.96*predict(model, se = TRUE, newdata = C)$se.fit,
    hi = predict(model, se = TRUE, newdata = C)$fit + 1.96*predict(model, se = TRUE, newdata = C)$se.fit,
    predictor = C[, predictor]
  )
  
  return(preds)
}

cg_change_10 <- preds_cg_change(cg_change_lm, C_cg_change_10, "Delta_pdc")
cg_change_50 <- preds_cg_change(cg_change_lm, C_cg_change_50, "Delta_pdc")
cg_change_90 <- preds_cg_change(cg_change_lm, C_cg_change_90, "Delta_pdc")



setwd("C:\\Users\\Sam\\Google Drive\\Thesis related\\Understory\\outputs") #where the plots go

png(filename="cheatgrass_change_effect.png", 
    type="cairo",
    units="in", 
    width = 5, 
    height=3, 
    pointsize=15, 
    res=160)

layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE))
par(mar = c(0,1,0,0), oma = c(3,4,2,2), family = "serif", bty = 'n')

  plot(NA,
       ylim = c(-.03,.06),
       xlim = c(-50,10),
       xlab = "",
       ylab = "",
       xaxt = "n",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex.lab = 1.5)
  abline(h = 0, lty = 4, lwd = 1.3)
  lines((exp(cg_change_10$predictions) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 2, lty = 1, col = "#1b9e77")
  lines((exp(cg_change_10$lo) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 1, lty = 1, col = "#1b9e77")
  lines((exp(cg_change_10$hi) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 1, lty = 1, col = "#1b9e77")
  polygon(c(cg_change_10$predictor, rev(cg_change_10$predictor)), c((exp(cg_change_10$hi) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)), 
                                                                    rev((exp(cg_change_10$lo) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)))),
          col = addTrans("#68EBC4",30), border = NA) #fills in the area between high and low confidence intervals
  
  points((exp(part_resids_10[model$model[, "cwd_normal_cum"] < unname(quantile(compare$cwd_normal_cum, .3))])
         +  min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100))~ 
           model$model[model$model[, "cwd_normal_cum"] < unname(quantile(compare$cwd_normal_cum, .3)), "Delta_pdc"],
         bg='grey60',
         col = 'grey30',
         pch = 21,
         cex = 1.5)
 axis(side = 1, at = c(-40, -20, 0), labels = TRUE, mgp=c(1.5,-0.5,-1.5))
 axis(side = 1, at = c(-50, -30, -10, 10), labels = FALSE, tcl = -.25, mgp=c(1.5,-0.5,-1.5))
 text(x = -20, y = .06, labels = "10% CWD")
  
  
  plot(NA,
       ylim = c(-.03,.06),
       xlim = c(-50,10),
       xlab = "",
       ylab = "",
       yaxt = "n",
       xaxt = "n",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex.lab = 1.5)
  abline(h = 0, lty = 4, lwd = 1.3)
  lines((exp(cg_change_50$predictions) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 2, lty = 1, col = "#1b9e77")
  lines((exp(cg_change_50$lo) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 1, lty = 1, col = "#1b9e77")
  lines((exp(cg_change_50$hi) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 1, lty = 1, col = "#1b9e77")
  polygon(c(cg_change_50$predictor, rev(cg_change_50$predictor)), c((exp(cg_change_50$hi) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)), 
                                                                    rev((exp(cg_change_50$lo) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)))),
          col = addTrans("#68EBC4",30), border = NA) #fills in the area between high and low confidence intervals
  
  
   points((exp(part_resids_50[model$model[, "cwd_normal_cum"] > unname(quantile(compare$cwd_normal_cum, .3)) &
                            model$model[, "cwd_normal_cum"] < unname(quantile(compare$cwd_normal_cum, .7))])
          +  min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ 
           model$model[model$model[, "cwd_normal_cum"] > unname(quantile(compare$cwd_normal_cum, .3)) &
                         model$model[, "cwd_normal_cum"] < unname(quantile(compare$cwd_normal_cum, .7)), "Delta_pdc"],
          bg='grey60',
          col = 'grey30',
          pch = 21,
          cex = 1.5)
  axis(side = 2, labels = FALSE)
  axis(side = 1, at = c(-40, -20, 0), labels = TRUE, mgp=c(1.5,-0.5,-1.5))
  axis(side = 1, at = c(-50, -30, -10, 10), labels = FALSE, tcl = -.25, mgp=c(1.5,-0.5,-1.5))
  text(x = -20, y = .06, labels = "50% CWD")
  
  
  plot(NA,
       ylim = c(-.03,.06),
       xlim = c(-50,10),
       xlab = "",
       ylab = "",
       yaxt = "n",
       xaxt = "n",
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex.lab = 1.5)
  abline(h = 0, lty = 4, lwd = 1.3)
  lines((exp(cg_change_90$predictions) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 2, lty = 1, col = "#1b9e77")
  lines((exp(cg_change_90$lo) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 1, lty = 1, col = "#1b9e77")
  lines((exp(cg_change_90$hi) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ I(cg_change_10$predictor), lwd = 1, lty = 1, col = "#1b9e77")
  polygon(c(cg_change_90$predictor, rev(cg_change_90$predictor)), c((exp(cg_change_90$hi) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)), 
                                                                    rev((exp(cg_change_90$lo) + min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)))),
          col = addTrans("#68EBC4",30), border = NA) #fills in the area between high and low confidence intervals
  
  points((exp(part_resids_90[model$model[, "cwd_normal_cum"] > unname(quantile(compare$cwd_normal_cum, .7))])
          +  min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100)) ~ 
           model$model[model$model[, "cwd_normal_cum"] > unname(quantile(compare$cwd_normal_cum, .7)), "Delta_pdc"],
         bg='grey60',
         col = 'grey30',
         pch = 21,
         cex = 1.5)
  axis(side = 2, labels = FALSE)
  axis(side = 1, at = c(-40, -20, 0), labels = TRUE, mgp=c(1.5,-0.5,-1.5))
  axis(side = 1, at = c(-50, -30, -10, 10), labels = FALSE, tcl = -.25, mgp=c(1.5,-0.5,-1.5))
  text(x = -20, y = .06, labels = "90% CWD")
  
  mtext(side = 1, text = "Change in Live Crown", outer = TRUE, line = 1.7)
  mtext(side = 2, text = "Pred. Change Cheatgrass Cover", outer = TRUE, line = 2.3)

dev.off()
