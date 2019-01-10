library("MuMIn")
library("lme4")
library("car")
library("effects")
library("plyr")
library("lmerTest")
library("vegan")
library("reshape2")
library("plyr")

setwd("C:/Users/Sam/Documents/Research/MS Thesis/Understory")

plot_data <- read.csv("./clean data/plot_data.csv")

# process other files and import

#------------------------------------------------------------
# Analysis
#------------------------------------------------------------
# 

#exploration
hist(asin(sqrt(plot_data$All/100)))
hist(logit(plot_data$All/100 + min(plot_data[plot_data$All != 0, "All"])))
hist(log(plot_data$All/100 + .01))

hist(asin(sqrt(plot_data$Cheatgrass/100)))
hist(logit(plot_data$Cheatgrass/100 + min(plot_data[plot_data$Cheatgrass != 0, "Cheatgrass"])))
hist(log(plot_data$Cheatgrass/100 + .01))

nrow(plot_data[plot_data$Cheatgrass != 0, ])

hist(asin(sqrt(plot_data$Pgrass/100)))
hist(logit(plot_data$Pgrass/100))
nrow(plot_data[plot_data$Pgrass != 0, ])

hist(asin(sqrt(plot_data$Pforb/100)))
hist(logit(plot_data$Pforb/100))
nrow(plot_data[plot_data$Pforb != 0, ])

hist(asin(sqrt(plot_data$Shrub_cover_li)))
hist(logit(plot_data$Shrub_cover_li))
nrow(plot_data[plot_data$Shrub_cover_li != 0, ])

plot(plot_data$Cheatgrass ~ plot_data$cwd_normal_cum)


#---------------------------------------------------------------
#linear mixed effects models for functional groups at plot level
#-------------------------------------------------------------------
## Cheatgrass

cheatgrass_plot <- lmer(asin(sqrt(Cheatgrass/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                          scale(AWC) + (1|Cluster), data = plot_data)

cheatgrass_plot <- lmer(logit(Cheatgrass/100) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                          scale(AWC) + (1|Cluster), data = plot_data)

summary(lm(asin(sqrt(Cheatgrass/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
             scale(AWC), data = plot_data))

summary(cheatgrass_plot, ddf = "Kenward-Roger")
r.squaredGLMM(cheatgrass_plot)
plot(allEffects(cheatgrass_plot, partial.residuals = TRUE))
plot(cheatgrass_plot)
AICc(cheatgrass_plot)
scatter.smooth(residuals(cheatgrass_plot) ~ predict(cheatgrass_plot))

leveneTest(residuals(cheatgrass_plot) ~ plot_data$Cheatgrass)

## Perr grass

pgrass_plot <- lmer(asin(sqrt(Pgrass/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                      scale(AWC) + (1|Cluster), data = plot_data)

summary(lm(asin(sqrt(Pgrass/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
             scale(AWC), data = plot_data))

summary(pgrass_plot, ddf = "Kenward-Roger")
r.squaredGLMM(pgrass_plot)
plot(allEffects(pgrass_plot, partial.residuals = TRUE))
plot(pgrass_plot)

## Perr forbs

pforb_plot <- lmer(asin(sqrt(Pforb/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                     scale(AWC) + (1|Cluster), data = plot_data)

summary(lm(asin(sqrt(Pforb/100)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
             scale(AWC), data = plot_data))


summary(pforb_plot, ddf = "Kenward-Roger")
r.squaredGLMM(pforb_plot)
plot(allEffects(pforb_plot, partial.residuals = TRUE))
plot(pforb_plot)


## Shrubs
shrub_plot <- lmer(asin(sqrt(Shrub_cover_li)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
                     scale(AWC) + (1|Cluster), data = plot_data)

summary(lm(asin(sqrt(Shrub_cover_li)) ~ scale(Tree_cover) + scale(Delta_pdc)*scale(cwd_normal_cum) +
             scale(AWC), data = plot_data))

summary(shrub_plot, ddf = "Kenward-Roger")
r.squaredGLMM(shrub_plot)
plot(allEffects(shrub_plot, partial.residuals = TRUE))
plot(shrub_plot)


#-----------------------------------------------
# Comparison of greenwood and 2015
#-----------------------------------------------
compare <- join(plot_daub_cover, greenwood_under, by = "Plot", type = "inner")
compare <- join(compare, tcover[, c("Plot", "Tree_cover", "Shrub_cover_li")], by = "Plot", type = "inner")
compare <- join(compare, plot_data[, c("Plot", "Dead_ba", "Delta_pdc", "cwd_normal_cum")], by = "Plot")

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

#linear model of log change ~ delta pdc and cwd
compare$log_cg_change <- log(I(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100 - min(compare$Cheatgrass/100 - compare$Cheatgrass.Cover/100) + .01))
cg_change_lm <- lm(log_cg_change ~ Delta_pdc*cwd_normal_cum, data= compare)

summary(cg_change_lm)
plot(cg_change_lm)
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
    height=6, 
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

# 
# #----------------------------------------
# # Ordination analysis
# #----------------------------------------
# raw_cover <- read.csv("./Raw data/spp_cover2.csv") #spp_cover2 has unknowns filled with "unknown"
# raw_cover <- raw_cover[raw_cover$Transect %in% c("E", "N", "S", "W"), ] #take out the "bonus" quadrats
# 
# spp_summary <- read.csv("./Raw data/species_summary.csv")
# spp_priority <- spp_summary[order(spp_summary[, "x"], decreasing = TRUE), ]
# 
# species_list <- unique(raw_cover$Species)
# plot_list <- unique(raw_cover$Plot)[order(unique(raw_cover$Plot))]
# 
# spp_matrix <- matrix(ncol = length(species_list), nrow = length(plot_list))
# row.names(spp_matrix) <- plot_list
# colnames(spp_matrix) <- species_list
# 
# env_data <- plot_data[, c("cwd_normal_cum", "Delta_pdc", "AWC", "Live_ba", 
#                           "Dead_ba", "Avg_depth", "Elev", "Slope", "Swness", 
#                           "tmean", "ppt")]
# 
# 
# for(i in 1:nrow(spp_matrix)){
#   for(j in 1:ncol(spp_matrix)){
#     
#     present <- species_list[j] %in% raw_cover[raw_cover$Plot == plot_list[i], "Species"]
#     if(present){
#       plot_cover <- raw_cover[raw_cover$Plot == plot_list[i] & raw_cover$Species == species_list[j], ]
#       spp_matrix[i, j] <- sum(plot_cover$Cover) / 20 / 100
#       } else{
#         spp_matrix[i, j] <- 0
#     }
#     
#     
#   }
# }
# 
# 
# nmds_test <- metaMDS(spp_matrix, try = 50)
# nmds_test
# plot(nmds_test, display = "species")
# # text(nmds_test, display = "species")
# orditorp(nmds_test, display = "species", priority = spp_priority$Group.1,
#          col = "forestgreen", pch = 2, cex = 1, air = 0.3)
# 
# plot(envfit(nmds_test, env_data, labels = list(vectors = names(env_data))))
