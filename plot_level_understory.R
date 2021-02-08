# Plot-level understory analysis
# author: Sam Flake
# email: sflake@gmail.com
# Description: this script runs all of the plot-level analysis for the study,
# not including the electivity analysis. It takes the processed data from
# understory_data_prep.R and does statistical analysis, as well as creating 
# Figure 2.

#load packages
library("MuMIn")
library("lme4")
library("car")
library("effects")
library("plyr")
library("lmerTest")
library("vegan")
library("reshape2")
library("plyr")

#settings
set.seed(218218218)
setwd("C:/Users/Sam/Documents/Research/MS Thesis/Understory/")

# import data created by data prep script
plot_data <- read.csv("./clean data/plot_data.csv")
plot_data$Delta_tc <- (plot_data$Tree_cover*100 - plot_data$Total_cover)
plot_data$Delta_ba <- plot_data$Live_ba - plot_data$Live_ba_2005
plot_data <- plot_data[plot_data$Cluster != "NPELECTRICEEL", ] #remove new plot without prior data


# understory variables from Greenwood and Weisberg 2008
greenwood_under <- read.csv("./raw data/Greenwood_Understory_Variables_SF_edits.csv")

#------------------------------------------------------------
# Analysis
#------------------------------------------------------------
#---------------------------------------------------------------
#linear mixed effects models for functional groups at plot level
#-------------------------------------------------------------------

## all understory vegetation

all_plot <- lmer(asin(sqrt(All/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                          scale(AWC) + (1|Cluster), data = plot_data)

summary(all_plot, ddf = "Kenward-Roger")
r.squaredGLMM(all_plot)
plot(allEffects(all_plot, partial.residuals = TRUE))
plot(all_plot)
AICc(all_plot)
scatter.smooth(residuals(all_plot) ~ predict(all_plot))

## Cheatgrass

cheatgrass_plot <- lmer(asin(sqrt(Cheatgrass/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                          scale(AWC) + (1|Cluster), data = plot_data)
summary(cheatgrass_plot, ddf = "Kenward-Roger")
r.squaredGLMM(cheatgrass_plot)
plot(allEffects(cheatgrass_plot, partial.residuals = TRUE))
plot(cheatgrass_plot)
AICc(cheatgrass_plot)
scatter.smooth(residuals(cheatgrass_plot) ~ predict(cheatgrass_plot))

## Perr grass

pgrass_plot <- lmer(asin(sqrt(Pgrass/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                      scale(AWC) + (1|Cluster), data = plot_data)
summary(pgrass_plot, ddf = "Kenward-Roger")
r.squaredGLMM(pgrass_plot)
plot(allEffects(pgrass_plot, partial.residuals = TRUE))
plot(pgrass_plot)

## Perr forbs

pforb_plot <- lmer(asin(sqrt(Pforb/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                     scale(AWC) + (1|Cluster), data = plot_data)
summary(pforb_plot, ddf = "Kenward-Roger")
r.squaredGLMM(pforb_plot)
plot(allEffects(pforb_plot, partial.residuals = TRUE))
plot(inv.as(predict(pforb_plot)) ~ I(plot_data$Pforb/100))
abline(0,1)
plot(pforb_plot)


## Shrubs
shrub_plot <- lmer(asin(sqrt(Shrub_cover_li)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                     scale(AWC) + (1|Cluster), data = plot_data)
summary(shrub_plot, ddf = "Kenward-Roger")
r.squaredGLMM(shrub_plot)
plot(allEffects(shrub_plot, partial.residuals = TRUE))
plot(inv.as(predict(shrub_plot)) ~ I(plot_data$Shrub_cover_li))
abline(0,1)

#-----------------------------------------------
# Comparison between 2005 and 2015 samples
#-----------------------------------------------
compare <- join(plot_data, greenwood_under, by = "Plot", type = "inner")

#cheatgrass
plot(compare$Cheatgrass ~ compare$Cheatgrass.Cover)
summary(lm(compare$Cheatgrass ~ compare$Cheatgrass.Cover))
cor.test(compare$Cheatgrass, compare$Cheatgrass.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(compare$Cheatgrass,  compare$Cheatgrass.Cover)
mean(compare$Cheatgrass-compare$Cheatgrass.Cover)
wilcox.test(compare$Cheatgrass, compare$Cheatgrass.Cover, paired = TRUE, alternative="greater")
t.test(compare$Cheatgrass, compare$Cheatgrass.Cover, paired = TRUE, alternative="greater")

cor.test(compare$Cheatgrass - compare$Cheatgrass.Cover, compare$cwd_normal_cum, method = "pearson",
         alternative = "two.sided", exact = FALSE)

# linear model of log change ~ delta pdc and cwd
# compare <- compare[-c(5, 10), ] #outlier to remove?

cg_change_lm <- lm(I((Cheatgrass - Cheatgrass.Cover)/100) ~ Delta_tc*cwd_normal_cum, data= compare)

summary(cg_change_lm)
plot(cg_change_lm)
plot(allEffects(cg_change_lm, partial.residuals = TRUE))


#seeing how far out these outliers are
sd(compare$log_cg_change[-c(5,10)])
mean(compare$log_cg_change[-c(5,10)])
(compare$log_cg_change[c(5)] - 1.72) / .18
(compare$log_cg_change[c(10)] - 1.72) / .18

summary(cg_change_lm)
plot(cg_change_lm)
plot(allEffects(cg_change_lm, partial.residuals = TRUE))

#Perr grass
plot(compare$Pgrass ~ compare$Perrenial.Grass.Cover)
summary(lm(compare$Pgrass ~ compare$Perrenial.Grass.Cover + 0))
cor.test(compare$Pgrass, compare$Perrenial.Grass.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(log(compare$Pgrass/compare$Perrenial.Grass.Cover))
mean(compare$Pgrass-compare$Perrenial.Grass.Cover)
t.test(compare$Pgrass, compare$Perrenial.Grass.Cover, paired = TRUE, alternative="less")
wilcox.test(compare$Pgrass, compare$Perrenial.Grass.Cover, paired = TRUE, alternative="less")

pg_change_lm <- lm(Pgrass - Perrenial.Grass.Cover ~ cwd_normal_cum + Delta_tc, data= compare)
summary(pg_change_lm)
plot(allEffects(pg_change_lm))

## Perr forb
plot(I(compare$Aforb + compare$Pforb) ~ compare$Forb.Cover)
summary(lm(I(compare$Aforb + compare$Pforb) ~ compare$Forb.Cover + 0))
cor.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(I(compare$Aforb + compare$Pforb) - compare$Forb.Cover)
mean(I(compare$Aforb + compare$Pforb) - compare$Forb.Cover)
t.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover, paired = TRUE, alternative="less")
wilcox.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover, paired = TRUE, alternative="less")

pf_change_lm <- lm(Aforb + Pforb - Forb.Cover ~ Delta_tc*cwd_normal_cum, data= compare)
summary(pf_change_lm)

## Shrub
plot(compare$Shrub ~ I(compare$Shrub.Cover.Total))
  abline(0,1)
summary(lm(compare$Shrub ~ I(compare$Shrub.Cover.Total) + 0))
cor.test(compare$Shrub, compare$Shrub.Cover.Total,
         method = c("spearman"), exact = FALSE)
boxplot(compare$Shrub - compare$Shrub.Cover.Total)
mean(compare$Shrub - compare$Shrub.Cover.Total)
t.test(compare$Shrub, compare$Shrub.Cover.Total, paired = TRUE, alternative="less")
wilcox.test(compare$Shrub, compare$Shrub.Cover.Total, paired = TRUE, alternative="less")
hist(compare$Shrub - I(compare$Shrub.Cover.Total))

s_change_lm <- lm(Shrub - Shrub.Cover.Total ~ Delta_tc*cwd_normal_cum, data= compare)
summary(s_change_lm)

######################
# Plot of comparisons between 2005 and 2015
# Figure 2
######################
opar <- par(no.readonly = TRUE)

tiff(filename="./outputs/Figure_2_change_in_cover.tif", 
    type="cairo",
    units="in", 
    width = 4, 
    height=4, 
    pointsize=12, 
    compression = "lzw", 
    res=600)

layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))

par(oma = c(2,2,0,0), bty = 'n')


par(mar = c(2,2,2,1))
plot(compare$Pgrass ~ compare$Perrenial.Grass.Cover, xlim = c(0,35), ylim = c(0, 35),
     xlab = "", ylab = "", main = "Perennial Grass", xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 0.7,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,10,20,30), labels = TRUE)
  axis(side = 1, at = c(0,5,15,25,35), tcl = -.24, labels = FALSE)
  axis(side = 2, at = c(0,10,20,30), labels = TRUE)
  axis(side = 2, at = c(0,5,15,25,35), tcl = -.24, labels = FALSE)
  abline(coef(lm(compare$Pgrass ~ compare$Perrenial.Grass.Cover)))
  abline(0,1, lty = 2, col = 'grey30')
  
par(mar = c(2,2,2,1))
plot(compare$Cheatgrass ~ compare$Cheatgrass.Cover, xlim = c(0, 20), ylim = c(0, 20),
       xlab = "", ylab = "", main = "Cheatgrass", xaxt = 'n', yaxt = 'n',
       bg='grey60',
       col = 'grey30',
       pch = 21,
       cex = 0.7,
       cex.axis = .8,
       cex.main = 1.3)
  axis(side = 1, at = c(0,5,10,15,20), labels = TRUE)
  axis(side = 1, at = c(0,2.5,7.5,12.5,17.5), tcl = -.25, labels = FALSE)
  axis(side = 2, at = c(0,5,10,15,20), labels = TRUE)
  axis(side = 2, at = c(0,2.5,7.5,12.5,17.5), tcl = -.25, labels = FALSE)
  abline(0,1, lty = 2, col = 'grey30')
  
  
par(mar = c(2,2,2,1))
plot(I(compare$Pforb + compare$Aforb) ~ compare$Forb.Cover, xlim = c(0, 12), ylim = c(0, 12),
     main = "Perennial Forb", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 0.7,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,4,8,12), labels = TRUE)
  axis(side = 1, at = c(0,2,6,10), tcl = -.25, labels = FALSE)
  axis(side = 2, at = c(0,4,8,12), labels = TRUE)
  axis(side = 2, at = c(0,2,6,10), tcl = -.25, labels = FALSE)
  abline(coef(lm(I(compare$Pforb + compare$Aforb) ~ compare$Forb.Cover)))
  abline(0,1, lty = 2, col = 'grey30')
  

par(mar = c(2,2,2,1))
plot(compare$Shrub ~ compare$Shrub.Cover.Total, xlim = c(0, 30), ylim = c(0,30),
     xlab = "", ylab = "", main = "Shrub",xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 0.7,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,10,20,30), labels = TRUE)
  axis(side = 1, at = c(0,5,15,25), tcl = -.25, labels = FALSE)
  axis(side = 2, at = c(0,10,20,30), labels = TRUE)
  axis(side = 2, at = c(0,5,15,25), tcl = -.25, labels = FALSE)
  abline(0,1, lty = 2, col = 'grey30')
  

mtext("2005 Cover (%)", side = 1, outer = TRUE)
mtext("2015 Cover (%)", side = 2, outer = TRUE)

dev.off()

par(opar)





#-----------------------------------------------
# Comparison between 2005 and 2015 samples
#-----------------------------------------------
compare <- join(plot_data, greenwood_under, by = "Plot", type = "inner")

#cheatgrass
plot(compare$Cheatgrass ~ compare$Cheatgrass.Cover)
summary(lm(compare$Cheatgrass ~ compare$Cheatgrass.Cover))
cor.test(compare$Cheatgrass, compare$Cheatgrass.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(compare$Cheatgrass,  compare$Cheatgrass.Cover)
mean(compare$Cheatgrass-compare$Cheatgrass.Cover)
wilcox.test(compare$Cheatgrass, compare$Cheatgrass.Cover, paired = TRUE, alternative="greater")
t.test(compare$Cheatgrass, compare$Cheatgrass.Cover, paired = TRUE, alternative="greater")

cor.test(compare$Cheatgrass - compare$Cheatgrass.Cover, compare$cwd_normal_cum, method = "pearson",
         alternative = "two.sided", exact = FALSE)

cg_change_lm <- lm(I((Cheatgrass - Cheatgrass.Cover)/100) ~ Delta_pdc*cwd_normal_cum, data= compare)

cg_change_lm_no_outliers <- lm(I((Cheatgrass - Cheatgrass.Cover)/100) ~ 
                                 Delta_pdc*cwd_normal_cum, data= compare[-c(5,10), ])

summary(cg_change_lm)
plot(cg_change_lm)
plot(allEffects(cg_change_lm, partial.residuals = TRUE))

summary(cg_change_lm_no_outliers)
plot(cg_change_lm)
plot(allEffects(cg_change_lm_no_outliers, partial.residuals = TRUE))

#seeing how far out these outliers are
sd(compare$log_cg_change[-c(5,10)])
mean(compare$log_cg_change[-c(5,10)])
(compare$log_cg_change[c(5)] - 1.72) / .18
(compare$log_cg_change[c(10)] - 1.72) / .18

#Perr grass
plot(log(compare$Pgrass) ~ log(compare$Perrenial.Grass.Cover))
summary(lm(compare$Pgrass ~ compare$Perrenial.Grass.Cover + 0))
cor.test(compare$Pgrass, compare$Perrenial.Grass.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(log(compare$Pgrass/compare$Perrenial.Grass.Cover))
mean(compare$Pgrass-compare$Perrenial.Grass.Cover)
t.test(compare$Pgrass, compare$Perrenial.Grass.Cover, paired = TRUE)
wilcox.test(compare$Pgrass, compare$Perrenial.Grass.Cover, paired = TRUE)

pg_change_lm <- lm(Pgrass - Perrenial.Grass.Cover ~ Delta_pdc*cwd_normal_cum, data= compare)
summary(pg_change_lm)
plot(allEffects(pg_change_lm, partial.residuals = TRUE))

pg_change_lm <- lm(Pgrass - Perrenial.Grass.Cover ~ dppt, data= compare)
summary(pg_change_lm)
plot(allEffects(pg_change_lm, partial.residuals = TRUE))

## Perr forb
plot(I(compare$Aforb + compare$Pforb) ~ compare$Forb.Cover)
summary(lm(I(compare$Aforb + compare$Pforb) ~ compare$Forb.Cover + 0))
cor.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(I(compare$Aforb + compare$Pforb) - compare$Forb.Cover)
mean(I(compare$Aforb + compare$Pforb) - compare$Forb.Cover)
t.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover, paired = TRUE)
wilcox.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover, paired = TRUE, exact = FALSE)

pf_change_lm <- lm(Aforb + Pforb - Forb.Cover ~ Delta_pdc*cwd_normal_cum, data= compare)
summary(pf_change_lm)

## Shrub
plot(compare$Shrub ~ I(compare$Shrub.Cover.Total))
abline(0,1)
summary(lm(compare$Shrub ~ I(compare$Shrub.Cover.Total) + 0))
cor.test(compare$Shrub, compare$Shrub.Cover.Total,
         method = c("spearman"), exact = FALSE)
boxplot(compare$Shrub - compare$Shrub.Cover.Total)
mean(compare$Shrub - compare$Shrub.Cover.Total)
t.test(compare$Shrub, compare$Shrub.Cover.Total, paired = TRUE)
wilcox.test(compare$Shrub, compare$Shrub.Cover.Total, paired = TRUE, exact = FALSE)
hist(compare$Shrub - I(compare$Shrub.Cover.Total))

s_change_lm <- lm(Shrub - Shrub.Cover.Total ~ Delta_pdc*cwd_normal_cum, data= compare)
summary(s_change_lm)
