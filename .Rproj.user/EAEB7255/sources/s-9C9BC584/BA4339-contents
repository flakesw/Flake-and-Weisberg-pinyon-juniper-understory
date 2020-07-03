## Understory electivity
# author: Sam Flake
# email: sflake@gmail.com
# Description: this script processes quadrat-level data to estimate electivity
# and calculates a null distribution from Monte Carlo randomizations. Outputs 
# an .rds file of the all the randomizations as well as Figure 5. Takes raw data
# as inputs, and does not rely on the data_prep.R file. 

#load libraries
library(plyr)
library(vioplot)
library(reshape2)
library(multcompView)

set.seed(16091315)

# 2015 quadrat data
daub <- read.csv("./Raw data/daub_cover.csv", stringsAsFactors = FALSE)

#some data proofing
  daub <- daub[(daub$Transect %in% c("N", "E", "S", "W")), ]
  daub$Plot <- as.character(daub$Plot)
  daub[daub$Plot == "NPElectricEel", "Plot"] <- "NPELECTRICEEL"
  daub[daub$Plot == "NPElectricEel120", "Plot"] <- "NPELECTRICEEL120"
  daub[daub$Plot == "NPElectricEel240", "Plot"] <- "NPELECTRICEEL240"
  daub[daub$Plot == "NPElectricEel360", "Plot"] <- "NPELECTRICEEL360"
daub$Midpoint.value <- as.numeric(as.character((daub$Midpoint.value)))
daub$unique_quad <- paste0(daub$Plot, daub$Transect, daub$Meter) 
daub[daub$Cover.type == "Perennial forb ", "Cover.type"] <- "Perennial forb"
daub[daub$Cover.type == "Shrub ", "Cover.type"] <- "Shrub"

# 2015 species-specific data, only used for summary statistics
species<- read.csv("./raw data/spp_cover2.csv")
species <- species[(species$Transect %in% c("N", "E", "S", "W")), ]
species$unique_quad <- paste0(species$Plot, species$Transect, species$Meter)

count(species, vars = c("Plot", "Transect"))


#little test to look at species relationships with CWD
# species_aggregate <- aggregate(species[c("Cover")], by = list(species$Plot, species$Species), FUN = mean)
# names(species_aggregate)[c(1,2)] <- c("Plot", "Species")
# species_aggregate <- join(species_aggregate, plot_data[c("Plot", "cwd_normal_cum")], by = c("Plot"))
# 
# plot(log(Cover) ~ cwd_normal_cum, data = species_aggregate[species_aggregate$Species == "ARTTRIV", ],
#      xlim = c(200, 450))
# 

#-----------------------------------------------------------------------
# Calculate mean cover, cover by quadrat, cover by plot
#-------------------------------------------------------------------
# Mean overall cover
length(unique(daub$unique_quad))
spp_abund <- aggregate(species$Cover / 2040, by = list(species$Species), FUN = sum)
spp_abund <- spp_abund[order(spp_abund[, 1], decreasing = FALSE), ]



#percent of quadrats occupied
quadrat_abund <- as.data.frame(table(species[species$Cover != 0, ]$Species))
quadrat_abund <- quadrat_abund[order(quadrat_abund[, 1], decreasing = FALSE), ]
quadrat_abund[, 2] <- quadrat_abund[, 2] * 100 / 2040


count <- table(species$Species, species$Plot)
is.not.zero <- function(x){
  if((x) == 0){return(0)}
  else{return(1)}
}

presence <- apply(count, c(1,2), FUN = is.not.zero)
plot_presence <- as.data.frame(rowSums(presence))/102*100

spp_abund <- cbind(spp_abund, quadrat_abund[-1, 2], plot_presence[-1, 1])

write.csv(spp_abund, "./outputs/species_summary.csv")


## generate cover data for different subsets
n_quads_occ <- length(unique(species$unique_quad)) #number of quadrat-by-group records
# 
# spp_cov <- data.frame(unique_quad = unique(species$unique_quad),
#                      poasec = numeric(n_quads_occ),
#                      othergrass = numeric(n_quads_occ),
#                      phlhoo = numeric(n_quads_occ),
#                      otherforb = numeric(n_quads_occ))
# 
# 
# for(i in 1:n_quads_occ){
#   spp_cov$poasec[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species == "POASEC",
#     species[species$unique_quad == spp_cov$unique_quad[i] & species$Species == "POASEC", ]$Cover, 0)
#   
#   spp_cov$othergrass[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species %in% c("PSESPI", "LEYCIN", "ELYELY", "STITHU"),
#                                    sum(species[species$unique_quad == spp_cov$unique_quad[i] 
#                                                & species$Species %in% c("PSESPI", "LEYCIN", "ELYELY", "STITHU"), ]$Cover), 0)
#   
#   spp_cov$phlhoo[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species == "PHLHOO",
#                               species[species$unique_quad == spp_cov$unique_quad[i] & species$Species == "PHLHOO", ]$Cover, 0)
#   
#   spp_cov$otherforb[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species %in% c("CREACA", "CREACU", "STEACA", "ERICAE", "LEPPUN", "ERIUMB", "AREACU","LESKIN","GALAPA","ASTPUR","BALSAG","CRYFLA","ERIMIC","MINKIN","ASTOOP","OPUPOL","ARAHOL","ANTDIM","BOEHOL"),
#                                   sum(species[species$unique_quad == spp_cov$unique_quad[i] 
#                                               & species$Species %in% c("CREACA", "CREACU", "STEACA", "ERICAE", "LEPPUN", "ERIUMB", "AREACU","LESKIN","GALAPA","ASTPUR","BALSAG","CRYFLA","ERIMIC","MINKIN","ASTOOP","OPUPOL","ARAHOL","ANTDIM","BOEHOL"), ]$Cover), 0)
# }
                     
#----------------------------------------------------------------------------

# import and recode microsite data
ms <- read.csv("./raw data/microsite.csv")
ms$Microsite <- toupper(ms$Microsite)
ms$ms <- ifelse(ms$Microsite %in% c("PI",  "JI",  "CI", "PO", "JO", "CO"), "Live",
                # ifelse(ms$Microsite %in% c("PO", "JO", "CO"), "Live Outer", 
                ifelse(ms$Microsite %in% c("PI(S)", "PO(S)", "JI(S)", "JO(S)", "CI(S)", "CO(S)", "LOG"), "Dead",
                       # ifelse(ms$Microsite =="LOG", "Log",
                       ifelse(ms$Microsite == "I", "Inter", NA)))#))
ms[ms$Transect == "e", "Transect"] <- "E"
ms[ms$Transect == "s", "Transect"] <- "S"
ms[ms$Transect == "w", "Transect"] <- "W"
ms[ms$Transect == "n", "Transect"] <- "N"

ms$Plot <- as.character(ms$Plot)

ms <- ms[(ms$Transect %in% c("N", "S", "E", "W")), ]

ms[ms$Plot == "NPElectricEel120", "Plot"] <- "NPELECTRICEEL120"


ms$unique_quad <- paste0(ms$Plot, ms$Transect, ms$Meter)

#get total cover for each plot
total_cover <- aggregate(daub$Midpoint.value, by = list(daub$Plot, daub$Cover.type), FUN = sum)

## select which plots have enough microsites of each type
# this is a real friggin mess but it pulls out the plots which have 
# at least n_plots dead and at least n_plots live ms
plots_to_use_dead <- NA
plots_to_use_live <- NA
n_plots <- 1

for(i in 1:length(unique(ms$Plot))){
   table <- table(ms[ms$Plot == unique(ms$Plot)[i], ]$ms)
  
  if("Dead" %in% names(table)){
    plots_to_use_dead[i] <- table["Dead"]
    } else{
      plots_to_use_dead[i] <- 0
    }
  
  if("Live" %in% names(table)){
    plots_to_use_live[i] <- table["Live"]
    } else{
      plots_to_use_live[i] <- 0
    }
}

plots_to_use_dead <- (plots_to_use_dead >= n_plots)
plots_to_use_live <- (plots_to_use_live >= n_plots)

plots_to_use <- unique(ms$Plot)[plots_to_use_dead & plots_to_use_live]


#------------------------------------------------------------------------------
# Calculate test and randomized distributions
#------------------------------------------------------------------------------
#perform analysis for the four FTs
types <- c("Perennial grass", "Cheatgrass", "Perennial forb", "Shrub")
ntypes <- length(types)
#initialize a list of dataframes to catch the electivities at each plot/FT
elect_results <- list()
for(i in 1:ntypes){
  elect_results[[i]] <- data.frame(Plot = numeric(0),
                                   Dead = numeric(0),
                                   Live = numeric(0),
                                   Inter = numeric(0),
                                   DL = numeric(0),
                                   DI = numeric(0),
                                   LI = numeric(0))
}
names(elect_results) <- types


for(type in types){ # calculate electivity for each functional type
  
  for (i in 1:length(plots_to_use)){
    
    if(type == "All"){
      daub_cov <- daub[daub$Plot == as.character(plots_to_use[i]), ]
      daub_cov <- daub_cov[daub_cov$Cover.type %in% c("Cheatgrass", "Other Ann Grass", 
                        "Perennial grass", "Annual forb", "Perennial forb", "Shrub"), ]
      daub_cov_2 <- aggregate(daub_cov$Midpoint.value, by = list(daub_cov$unique_quad), FUN = sum)
      names(daub_cov_2) <- c("unique_quad", "Midpoint.value")
      daub_cov <- join(daub_cov_2, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
    }else{
      daub_cov <- daub[daub$Cover.type == type & daub$Plot == as.character(plots_to_use[i]), ]
      daub_cov <- join(daub_cov, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
     
    }
    
    daub_cov$prop_cov <- daub_cov$Midpoint.value / sum(daub_cov$Midpoint.value)
    
    cov <- aggregate(daub_cov[, "prop_cov"], by = list(daub_cov$ms), FUN = sum)
    
    prev <- table(daub_cov$ms)/20
    
    #this is a mess because tables are hard to use. There's definitely a better way to do this
    elect <- data.frame(Plot = plots_to_use[i],
                        Dead = (cov[cov$Group.1 == "Dead",2] - prev[which(names(prev) == "Dead")]) / 
                          (cov[cov$Group.1 == "Dead", 2] + prev[which(names(prev) == "Dead")]),
                        Live = (cov[cov$Group.1 == "Live",2] - prev[which(names(prev) == "Live")]) / 
                          (cov[cov$Group.1 == "Live",2] + prev[which(names(prev) == "Live")]),
                        Inter = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                          (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]))
    elect$DL = (cov[cov$Group.1 == "Dead",2] - cov[cov$Group.1 == "Live",2]) / (cov[cov$Group.1 == "Dead",2] + cov[cov$Group.1 == "Live",2])
    elect$DI = (cov[cov$Group.1 == "Dead",2] - cov[cov$Group.1 == "Inter",2]) / (cov[cov$Group.1 == "Dead",2] + cov[cov$Group.1 == "Inter",2])
    elect$LI = (cov[cov$Group.1 == "Live",2] - cov[cov$Group.1 == "Inter",2]) / (cov[cov$Group.1 == "Live",2] + cov[cov$Group.1 == "Inter",2])
    
    elect_results[[type]] <- rbind(elect_results[[type]], elect)
  }
}

saveRDS(elect_results, paste0("./outputs/results_elect_", n_plots, "dead.rds"))
# elect_results <- readRDS("./outputs/results_elect_1dead.rds")


#------------------------------------------------------------------------------
#Calculate monte carlo means
#------------------------------------------------------------------------------
nit <- 999 #number of monte carlo draws

#initialize a list of dataframes to catch the electivities at each plot/FT
elect_means <- list()
for(i in 1:ntypes){
  elect_means[[i]] <- data.frame(Iter = numeric(0),
                                 Dead = numeric(0),
                                 Live = numeric(0),
                                 Inter = numeric(0),
                                 DL = numeric(0),
                                 DI = numeric(0),
                                 LI = numeric(0))
}
names(elect_means) <- types

system.time(#takes about a half hour

# do the electivities!
  
for(type in types){
  
  plot_to_use_type <- subset(elect_results[[type]], !is.na(Dead))$Plot
  
  for(j in 1:nit){
    
    #temporary storage for each iteration
    elect_rand <- data.frame(Plot = character(0),
                             Dead = numeric(0),
                             Live = numeric(0),
                             Inter = numeric(0),
                             DL = numeric(0),
                             DI = numeric(0),
                             DL = numeric(0))
  
    for (plot_use in plot_to_use_type){
      if(type == "All"){
        daub_cov <- daub[daub$Plot == as.character(plot_use), ]
        daub_cov <- daub_cov[daub_cov$Cover.type %in% c("Cheatgrass", "Other Ann Grass", 
                                                        "Perennial grass", "Annual forb", "Perennial forb", "Shrub"), ]
        daub_cov_2 <- aggregate(daub_cov$Midpoint.value, by = list(daub_cov$unique_quad), FUN = sum)
        names(daub_cov_2) <- c("unique_quad", "Midpoint.value")
        daub_cov <- join(daub_cov_2, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
      }else{
        daub_cov <- daub[daub$Cover.type == type & daub$Plot == as.character(plot_use), ]
        daub_cov <- join(daub_cov, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
        
      }
      
      daub_cov$prop_cov <- daub_cov$Midpoint.value / sum(daub_cov$Midpoint.value)
      daub_cov$prop_cov_ran <- sample(daub_cov$prop_cov, size = 20, replace = FALSE)
      
      cov <- aggregate(daub_cov[, "prop_cov_ran"], by = list(daub_cov$ms), FUN = sum)
      prev <- table(daub_cov$ms)/20
      
      #temporary df to store results from each plot
      elect <- data.frame(Plot = plot_use,
                          Dead = (cov[cov$Group.1 == "Dead",2] - prev[which(names(prev) == "Dead")]) / 
                            (cov[cov$Group.1 == "Dead", 2] + prev[which(names(prev) == "Dead")]),
                          Live = (cov[cov$Group.1 == "Live",2] - prev[which(names(prev) == "Live")]) / 
                            (cov[cov$Group.1 == "Live",2] + prev[which(names(prev) == "Live")]),
                          Inter = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                            (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]))
      elect$DL = (cov[cov$Group.1 == "Dead",2] - cov[cov$Group.1 == "Live",2]) / (cov[cov$Group.1 == "Dead",2] + cov[cov$Group.1 == "Live",2])
      elect$DI = (cov[cov$Group.1 == "Dead",2] - cov[cov$Group.1 == "Inter",2]) / (cov[cov$Group.1 == "Dead",2] + cov[cov$Group.1 == "Inter",2])
      elect$LI = (cov[cov$Group.1 == "Live",2] - cov[cov$Group.1 == "Inter",2]) / (cov[cov$Group.1 == "Live",2] + cov[cov$Group.1 == "Inter",2])
      
      elect_rand <- rbind(elect_rand, elect) #add data from plot to temporary df
    }
    
    # add mean from electivity from this iteration to global df
    means <- apply(elect_rand[, c(2:7)], 2, FUN = function(x){mean(x, na.rm = TRUE)})
    elect_means[[type]][j, 1] <- j
    elect_means[[type]][j, 2:7] <- means
    
  
  }
  
}
)

# initialize a dataframe to store extracted information from the monte carlo draws
results_boots <- list()
for(i in 1:ntypes){
  results_boots[[i]] <- data.frame(Dead = numeric(2),
                                 Live = numeric(2),
                                 Inter = numeric(2),
                                 DL = numeric(2),
                                 DI = numeric(2),
                                 LI = numeric(2))
  rownames(results_boots[[i]]) <- c("Empirical", "N_boots_greater")
}
names(results_boots) <- types

#calculate p-values from distribution of bootstrapped mean electivities
for(i in 1:ntypes){
emp_mean <- apply(elect_results[[i]][, 2:7], 2, FUN = function(x){mean(x, na.rm = TRUE)})

pvals <- c(sum(elect_means[[i]][2] > emp_mean[1]), 
           sum(elect_means[[i]][3] > emp_mean[2]), 
           sum(elect_means[[i]][4] > emp_mean[3]),
           sum(elect_means[[i]][5] > emp_mean[4]), 
           sum(elect_means[[i]][6] > emp_mean[5]), 
           sum(elect_means[[i]][7] > emp_mean[6]))

#flip around large p-values to make them small p-values for the left tail
pvals <- ifelse(pvals > (nit + 1)/2, (nit + 1 - pvals)/(nit + 1), pvals/(nit + 1))

results_boots[[i]][1, ] <- emp_mean
results_boots[[i]][2, ] <- pvals
}

#save and read results depending on what you need
# saveRDS(results_boots, paste0("./outputs/results_boots_", n_plots, "dead.rds"))

# results_boots <- readRDS("./outputs/results_boots_1dead.rds")


#-----------------------------------------------------------------------------------------------
# Summary table
#-----------------------------------------------------------------------------------------------
summary <- as.data.frame(matrix(nrow = 12, ncol = 8))
for(i in 1:4){
  for(j in 1:6){
    summary[3*i - 2, j+2] <- results_boots[[i]][[j]][[1]]
    summary[3*i - 1, j+2] <- results_boots[[i]][[j]][[1]] - mean(elect_means[[i]][[j + 1]])
    summary[3*i, j+2] <- results_boots[[i]][[j]][[2]]
  }
}

summary[, 2] <- rep(c("Empirical", "Difference", "p-val"), times = 4)
summary[, 1] <- rep(c("Perennial grass", "Cheatgrass", "Perennial forb", "Shrub"), each = 3)
names(summary) <- c("FT", "var", "Dead", "Live", "Inter", "D-L", "D-I", "L-I")
write.csv(summary, "./outputs/electivity summary table.csv")

#-----------------------------------------------------------------------------------------------
# Plot of electivity for each FT
# Figure 5
#-----------------------------------------------------------------------------------------------

tiff(filename="./outputs/Figure_5_electivity.tiff", 
    type="cairo",
    units="in", 
    width = 4, 
    height=4, 
    pointsize=10, 
    compression = "lzw", 
    res=1000)

par(mfrow = c(2,2),
    mar = c(2,1,1,2),
    oma = c(2,3,1,0))

for(i in 1:ntypes){

  melt_elect <- melt(elect_results[[i]][, 2:4])
  plot(NA,
       ylim = c(-1,1),
       xlim = c(.7, 3.3),
       xaxt = "n")
  pvals <- results_boots[[i]][2, ]
  abline(h = 0)
  
  vioplot(elect_means[[i]][[2]], add = TRUE, col = addTrans("blue", 30), drawRect = FALSE)
  vioplot(elect_means[[i]][[3]], at = 2, add = TRUE, col = addTrans("blue", 30), drawRect = FALSE)
  vioplot(elect_means[[i]][[4]], at = 3, add = TRUE, col = addTrans("blue", 30), drawRect = FALSE)
  
  points(melt_elect$value ~ I(as.numeric(melt_elect$variable)+ runif(nrow(melt_elect), -.1, .1 )),
         pch = 21,
         bg = "grey")
  means <- aggregate(melt_elect$value, by = list(melt_elect$variable), FUN = function(x){mean(x, na.rm = TRUE)})
  
  segments(x0 = c(0.85, 1.85, 2.85), y0 = means$x, x1 = c(1.15, 2.15, 3.15), lwd = 3)

  
  if(i %in% c(1,2)){
    axis(1, at = c(1,2,3), labels = FALSE)
  }
  
  if(i %in% c(3,4)){
  axis(1, at = c(1,2,3), labels =  c("Dead", "Live", "Inter"))
  }
  
  # text(x = c(1,2,3), y = 0.9, labels = pvals[4:6])
  mtext(text = types[i], outer = FALSE, side = 3, line = 0.3)
  mtext(text = paste0("(", letters[i], ")"), outer = FALSE, side = 3, at = 0.5, line = 0.3)
  mtext(text = "Ivlev's E", outer = TRUE, side = 2, line = 1.5)
  

  
}

dev.off()

