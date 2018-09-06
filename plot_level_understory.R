#Run "understory_data_prep.R" first!
library("MuMIn")
library("lme4")
library("car")
library("glmmADMB")
library("effects")
library("plyr")
library("lmerTest")
library("vegan")

plot_data <- plot_data[!(plot_data$Plot %in% c("NPELECTRICEEL", "NPELECTRICEEL120",
                                           "NPELECTRICEEL240", "NPELECTRICEEL360")), ]
plot_data$Cluster <- droplevels(plot_data$Cluster)

#exploration
hist(asin(sqrt(plot_data$Cheatgrass/100)))
hist(log(plot_data$Cheatgrass/100 + .01))

nrow(plot_data[plot_data$Cheatgrass != 0, ])

hist(asin(sqrt(plot_data$Pgrass/100)))
nrow(plot_data[plot_data$Pgrass != 0, ])

hist(asin(sqrt(plot_data$Pforb/100)))
nrow(plot_data[plot_data$Pforb != 0, ])

hist(asin(sqrt(plot_data$Shrub_cover_li)))
nrow(plot_data[plot_data$Shrub_cover_li != 0, ])

plot(plot_data$Cheatgrass ~ plot_data$cwd_normal_cum)

#---------------------------------------------------------------
#linear mixed effects models for functional groups at plot level
#-------------------------------------------------------------------

## Cheatgrass

cheatgrass_plot <- lmer(log(Cheatgrass/100 + .01) ~ scale(Tree_cover) + scale(Dead_ba)*scale(cwd_normal_cum) +
                          scale(AWC) + scale(Avg_depth) + scale(Dead_ba)*scale(cwd_normal_cum) + (1|Cluster), data = plot_data, 
                        na.action = na.omit)
summary(cheatgrass_plot, ddf = "Kenward-Roger")
r.squaredGLMM(cheatgrass_plot)
plot(allEffects(cheatgrass_plot, partial.residuals = TRUE))
plot(cheatgrass_plot)
AICc(cheatgrass_plot)
scatter.smooth(residuals(cheatgrass_plot) ~ predict(cheatgrass_plot))

cgrass_set <- dredge(cheatgrass_plot)
head(cgrass_set)

cg_model <- lmer(log(Cheatgrass/100 + .01) ~ scale(Delta_pdc)*scale(cwd_normal_cum) + (1|Cluster), data = plot_data, 
                 na.action = na.fail)
summary(cg_model, ddf = "Kenward-Roger")
r.squaredGLMM(cg_model)

## Perr grass

pgrass_plot <- lmer(log(Pgrass/100+.01) ~ scale(Tree_cover) + scale(Dead_ba)*scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                      scale(AWC) + scale(Avg_depth) + scale(Dead_ba)*scale(cwd_normal_cum) + scale(Dead_rat) + (1|Cluster), data = plot_data, 
                    na.action = na.omit)
summary(pgrass_plot, ddf = "Kenward-Roger")
r.squaredGLMM(pgrass_plot)
plot(allEffects(pgrass_plot, partial.residuals = TRUE))
plot(pgrass_plot)

pgrass_set <- dredge(pgrass_plot)
head(pgrass_set)

pg_model <- lmer(log(Pgrass/100) ~ (1|Cluster), data = plot_data, na.action = na.fail)
summary(pg_model)
r.squaredGLMM(pg_model)

## Perr forbs

pforb_plot <- lmer(log(Pforb/100+.01) ~ scale(Tree_cover)  + scale(Delta_pdc)*scale(cwd_normal_cum) +
                     scale(AWC) + scale(Avg_depth) + scale(Dead_ba)*scale(cwd_normal_cum) + (1|Cluster), data = plot_data, 
                   na.action = na.fail)
summary(pforb_plot, ddf = "Kenward-Roger")
r.squaredGLMM(pforb_plot)
plot(allEffects(pforb_plot, partial.residuals = TRUE))
plot(pforb_plot)

pforb_set <- dredge(pforb_plot)
head(pforb_set)

pf_model <- lmer(log(Pforb/100 +.01) ~ (1|Cluster), data = plot_data, na.action = na.fail)
summary(pf_model)
r.squaredGLMM(pf_model)

## Shrubs
shrub_plot <- lmer(log(Shrub_cover_li + .01) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                     scale(AWC) + scale(Avg_depth) + scale(Dead_ba)*scale(cwd_normal_cum) + (1|Cluster), data = plot_data, 
                   na.action = na.fail)
summary(shrub_plot, ddf = "Kenward-Roger")
r.squaredGLMM(shrub_plot)
plot(allEffects(shrub_plot, partial.residuals = TRUE))
plot(shrub_plot)

shrub_set <- dredge(shrub_plot)
head(shrub_set)

shrub_model<- lmer(log(Shrub_cover_li + .01) ~ scale(Tree_cover) + (1|Cluster), data = plot_data, 
                   na.action = na.fail)
summary(shrub_model)
r.squaredGLMM(shrub_model)
plot(allEffects(shrub_model, partial.residuals = TRUE))

## presence/absence of cheatgrass
cg_pa <- glmer(ifelse(plot_data$Cheatgrass == 0, 0, 1) ~ scale(Live_ba) + scale(Delta_pdc) + scale(cwd_normal_cum) +
                scale(AWC) + (1|Cluster), 
               family = "binomial",
               link = "logit",
               data = plot_data)
summary(cg_pa)



#-----------------------------------------------
# Comparison of greenwood and 2015
#-----------------------------------------------
compare <- join(plot_daub_cover, greenwood_under, by = "Plot", type = "inner")
compare <- join(compare, tcover[, c("Plot", "Tree_cover", "Shrub_cover_li")], by = "Plot", type = "inner")
compare <- join(compare, plot_data[, c("Plot", "Dead_ba", "Delta_pdc", "cwd_normal_cum")], by = "Plot")

#cheatgrass
plot(compare$Cheatgrass ~ compare$Cheatgrass.Cover)
summary(lm(compare$Cheatgrass ~ compare$Cheatgrass.Cover + 0))
cor.test(compare$Cheatgrass, compare$Cheatgrass.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(compare$Cheatgras-compare$Cheatgrass.Cover)
mean(compare$Cheatgrass-compare$Cheatgrass.Cover)
wilcox.test(compare$Cheatgrass, compare$Cheatgrass.Cover, paired = TRUE, alternative="greater")
t.test(compare$Cheatgrass, compare$Cheatgrass.Cover, paired = TRUE, alternative="greater")

cor.test(compare$Cheatgrass - compare$Cheatgrass.Cover, compare$cwd_normal_cum, method = "pearson",
         alternative = "two.sided", exact = FALSE)
#linear mode of log change ~ delta pdc and cwd
compare$log_cg_change <- log(I(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100 - min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100) + .01))
cg_change_lm <- lm(log_cg_change ~ Delta_pdc*cwd_normal_cum, data= compare[-c(5,10),])

summary(cg_change_lm)
plot(allEffects(cg_change_lm))


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

pg_change_lm <- lm(Pgrass - Perrenial.Grass.Cover ~ Delta_pdc*cwd_normal_cum, data= compare[-5,])
summary(pg_change_lm)

## Perr forb
plot(I(compare$Aforb + compare$Pforb) ~ compare$Forb.Cover)
summary(lm(I(compare$Aforb + compare$Pforb) ~ compare$Forb.Cover + 0))
cor.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover,
         method = c("spearman"), exact = FALSE)
boxplot(I(compare$Aforb + compare$Pforb) - compare$Forb.Cover)
mean(I(compare$Aforb + compare$Pforb) - compare$Forb.Cover)
t.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover, paired = TRUE, alternative="less")
wilcox.test(I(compare$Aforb + compare$Pforb), compare$Forb.Cover, paired = TRUE, alternative="less")

pf_change_lm <- lm(Aforb + Pforb - Forb.Cover ~ Delta_pdc*cwd_normal_cum, data= compare[-5,])
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

s_change_lm <- lm(Shrub - Shrub.Cover.Total ~ Delta_pdc*cwd_normal_cum, data= compare[-5,])
summary(s_change_lm)

######################
# Plot of comparisons between 2005 and 2015
######################
opar <- par(no.readonly = TRUE)

png(filename="change_in_cover.png", 
    type="cairo",
    units="in", 
    width = 4, 
    height=4, 
    pointsize=12, 
    res=160)

layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))

par(oma = c(2,2,0,0), family = "serif", bty = 'n')


par(mar = c(2,2,2,1))
plot(compare$Pgrass ~ compare$Perrenial.Grass.Cover, xlim = c(0,35), ylim = c(0, 35),
     xlab = "", ylab = "", main = "Perennial Grass", xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 1.5,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,10,20,30), labels = TRUE)
  axis(side = 1, at = c(0,5,15,25,35), tcl = -.24, labels = FALSE)
  axis(side = 2, at = c(0,10,20,30), labels = TRUE)
  axis(side = 2, at = c(0,5,15,25,35), tcl = -.24, labels = FALSE)
  abline(0,1, lty = 2, col = 'grey30')
  
par(mar = c(2,2,2,1))
plot(compare$Pforb ~ compare$Forb.Cover, xlim = c(0, 12), ylim = c(0, 12),
     main = "Perennial Forb", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 1.5,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,4,8,12), labels = TRUE)
  axis(side = 1, at = c(0,2,6,10), tcl = -.25, labels = FALSE)
  axis(side = 2, at = c(0,4,8,12), labels = TRUE)
  axis(side = 2, at = c(0,2,6,10), tcl = -.25, labels = FALSE)
  abline(0,1, lty = 2, col = 'grey30')
  

par(mar = c(2,2,2,1))
plot(compare$Shrub ~ compare$Shrub.Cover.Total, xlim = c(0, 30), ylim = c(0,30),
     xlab = "", ylab = "", main = "Shrub",xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 1.5,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,10,20,30), labels = TRUE)
  axis(side = 1, at = c(0,5,15,25), tcl = -.25, labels = FALSE)
  axis(side = 2, at = c(0,10,20,30), labels = TRUE)
  axis(side = 2, at = c(0,5,15,25), tcl = -.25, labels = FALSE)
  abline(0,1, lty = 2, col = 'grey30')
  

par(mar = c(2,2,2,1))
plot(compare$Cheatgrass ~ compare$Cheatgrass.Cover, xlim = c(0, 20), ylim = c(0, 20),
     xlab = "", ylab = "", main = "Cheatgrass", xaxt = 'n', yaxt = 'n',
     bg='grey60',
     col = 'grey30',
     pch = 21,
     cex = 1.5,
     cex.axis = .8,
     cex.main = 1.3)
  axis(side = 1, at = c(0,5,10,15,20), labels = TRUE)
  axis(side = 1, at = c(0,2.5,7.5,12.5,17.5), tcl = -.25, labels = FALSE)
  axis(side = 2, at = c(0,5,10,15,20), labels = TRUE)
  axis(side = 2, at = c(0,2.5,7.5,12.5,17.5), tcl = -.25, labels = FALSE)
  abline(0,1, lty = 2, col = 'grey30')
  
mtext("2005 Cover (%)", side = 1, outer = TRUE)
mtext("2015 Cover (%)", side = 2, outer = TRUE)
dev.off()

par(opar)

#-----------------------------------------------------------------
## Aggregating by cluster
#-----------------------------------------------------------------
library(car)
ag_data <- aggregate(plot_data, by = list(plot_data$Cluster), FUN = mean)

#---------------------------------------------------------
ag_data$log_cg <- log(ag_data$Cheatgrass/100 + .01)

cheatgrass_ag <- lm(log(ag_data$Cheatgrass/100 + .01) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                      scale(AWC), data = ag_data, 
                        na.action = na.fail)
summary(cheatgrass_plot)
summary(cheatgrass_ag)
plot(allEffects(cheatgrass_ag))
cg_ag_set <- dredge(cheatgrass_ag)
head(cg_ag_set)
cg_best <- lm(log(Cheatgrass/100 + .01) ~ scale(cwd_normal_cum)*scale(Delta_pdc) + scale(Tree_cover),
              data = ag_data)
summary(cg_best)

cg_avg <- model.avg(get.models(cg_ag_set, subset = delta <4), fit = TRUE)
summary(cg_avg)

vif(cheatgrass_ag)
#---------------------------------------------------

pgrass_ag <- lm(log(Pgrass/100 + .01) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                  scale(AWC), data = ag_data, 
                    na.action = na.fail)
summary(pgrass_plot)
summary(pgrass_ag)

pgrass_ag_set <- dredge(pgrass_ag)
head(pgrass_ag_set)
pgrass_best <- lm(log(Pgrass/100 + .01) ~ scale(Delta_pdc), data = ag_data, 
                  na.action = na.fail)
summary(pgrass_best)

pg_avg <- model.avg(get.models(pgrass_ag_set, subset = delta <4), fit = TRUE)
summary(pg_avg)

vif(pgrass_ag)

#-------------------------------------------
pforb_ag <- lm(log(Pforb/100 + .01) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                 scale(AWC), data = ag_data, 
                na.action = na.fail)

summary(pforb_plot)
summary(pforb_ag)
pforb_ag_set <- dredge(pforb_ag)
head(pforb_ag_set)
pforb_best <- lm(log(Pforb/100 + .01) ~ scale(Tree_cover) + scale(AWC), data = ag_data)
summary(pforb_best)

pf_avg <- model.avg(get.models(pforb_ag_set, subset = delta <4), fit = TRUE)
summary(pf_avg)

plot(allEffects(pforb_ag, partial.residuals = TRUE))
#---------------------------------------------------------
aforb_ag <- lm(log(Aforb/100 + .01) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                 scale(AWC), data = ag_data, 
               na.action = na.fail)

summary(aforb_ag)
pforb_ag_set <- dredge(pforb_ag)
head(pforb_ag_set)
pforb_best <- lm(log(Pforb/100 + .01) ~ scale(Tree_cover) + scale(AWC), data = ag_data)
summary(pforb_best)

pf_avg <- model.avg(get.models(pforb_ag_set, subset = delta <4), fit = TRUE)
summary(pf_avg)

plot(allEffects(pforb_ag, partial.residuals = TRUE))


#--------------------------------------------------------

shrub_ag <- lm(log(Shrub_cover_li + .01) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                 scale(AWC), data = ag_data, 
               na.action = na.fail)
summary(shrub_plot)
summary(shrub_ag)


shrub_ag_set <- dredge(shrub_ag)
head(shrub_ag_set)
shrub_best <- lm(log(Shrub_cover_li/100 + .01) ~ scale(Tree_cover), data = ag_data)
summary(shrub_best)

shrub_avg <- model.avg(get.models(shrub_ag_set, subset = delta <4), fit = TRUE)
summary(shrub_avg)


plot(allEffects(shrub_ag, partial.residuals = TRUE))




#----------------------------------------
# Ordination analysis
#----------------------------------------
raw_cover <- read.csv("spp_cover.csv")

spp_matrix <- matrix(ncol = length(unique(spp_cov2)))
