## Understory electivity
library(plyr)
library(vioplot)
library(reshape2)

daub <- read.csv("./raw data/daub_cover.csv")
  daub <- daub[(daub$Transect %in% c("N", "E", "S", "W")), ]
  daub$Plot <- as.character(daub$Plot)
  daub[daub$Plot == "NPElectricEel", "Plot"] <- "NPELECTRICEEL"
  daub[daub$Plot == "NPElectricEel120", "Plot"] <- "NPELECTRICEEL120"
  daub[daub$Plot == "NPElectricEel240", "Plot"] <- "NPELECTRICEEL240"
  daub[daub$Plot == "NPElectricEel360", "Plot"] <- "NPELECTRICEEL360"
daub$Midpoint.value <- as.numeric(as.character((daub$Midpoint.value)))
daub$unique_quad <- paste0(daub$Plot, daub$Transect, daub$Meter)

species<- read.csv("./raw data/spp_cover2.csv")
species <- species[(species$Transect %in% c("N", "E", "S", "W")), ]
species$unique_quad <- paste0(species$Plot, species$Transect, species$Meter)

count(species, vars = c("Plot", "Transect"))

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

spp_cov <- data.frame(unique_quad = unique(species$unique_quad),
                     poasec = numeric(n_quads_occ),
                     othergrass = numeric(n_quads_occ),
                     phlhoo = numeric(n_quads_occ),
                     otherforb = numeric(n_quads_occ))


for(i in 1:n_quads_occ){
  spp_cov$poasec[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species == "POASEC",
    species[species$unique_quad == spp_cov$unique_quad[i] & species$Species == "POASEC", ]$Cover, 0)
  
  spp_cov$othergrass[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species %in% c("PSESPI", "LEYCIN", "ELYELY", "STITHU"),
                                   sum(species[species$unique_quad == spp_cov$unique_quad[i] 
                                               & species$Species %in% c("PSESPI", "LEYCIN", "ELYELY", "STITHU"), ]$Cover), 0)
  
  spp_cov$phlhoo[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species == "PHLHOO",
                              species[species$unique_quad == spp_cov$unique_quad[i] & species$Species == "PHLHOO", ]$Cover, 0)
  
  spp_cov$otherforb[i] <- ifelse(species[species$unique_quad == spp_cov$unique_quad[i], ]$Species %in% c("CREACA", "CREACU", "STEACA", "ERICAE", "LEPPUN", "ERIUMB", "AREACU","LESKIN","GALAPA","ASTPUR","BALSAG","CRYFLA","ERIMIC","MINKIN","ASTOOP","OPUPOL","ARAHOL","ANTDIM","BOEHOL"),
                                  sum(species[species$unique_quad == spp_cov$unique_quad[i] 
                                              & species$Species %in% c("CREACA", "CREACU", "STEACA", "ERICAE", "LEPPUN", "ERIUMB", "AREACU","LESKIN","GALAPA","ASTPUR","BALSAG","CRYFLA","ERIMIC","MINKIN","ASTOOP","OPUPOL","ARAHOL","ANTDIM","BOEHOL"), ]$Cover), 0)
}
                     
#----------------------------------------------------------------------------

# import and recode microsite data
ms <- read.csv("./raw data/microsite.csv")
ms$ms <- ifelse(ms$Microsite %in% c("PI",  "JI",  "CI"), "Live Inner",
                ifelse(ms$Microsite %in% c("PO", "JO", "CO"), "Live Outer", 
                ifelse(ms$Microsite %in% c("PI(S)", "PO(S)", "JI(S)", "JO(S)", "CI(S)", "CO(S)"), "Dead",
                       ifelse(ms$Microsite =="LOG", "Log",
                       ifelse(ms$Microsite == "I", "Inter", NA)))))
ms[ms$Transect == "e", "Transect"] <- "E"
ms[ms$Transect == "s", "Transect"] <- "S"
ms[ms$Transect == "w", "Transect"] <- "W"
ms[ms$Transect == "n", "Transect"] <- "N"

ms$Plot <- as.character(ms$Plot)

ms <- ms[(ms$Transect %in% c("N", "S", "E", "W")), ]

ms[ms$Plot == "NPElectricEel120", "Plot"] <- "NPELECTRICEEL120"


ms$unique_quad <- paste0(ms$Plot, ms$Transect, ms$Meter)

unique(ms$Plot)[!(unique(ms$Plot) %in% unique(daub$Plot))] 
unique(daub$Plot)[!(unique(daub$Plot) %in% unique(ms$Plot))] #WHIS64240

#how many quadrats are messed up?
table(ms[!(ms$unique_quad %in% unique(daub$unique_quad)), "Plot"])[table(ms[!(ms$unique_quad %in% unique(daub$unique_quad)), "Plot"]) > 0] 
    #SPR1575 (7 quads), TOI1577360
table(daub[!(daub$unique_quad %in% unique(ms$unique_quad)), "Plot"])/11 #same as above, plus WHIS64240

#which quadrats are messed up?
unique(ms[!(ms$unique_quad %in% unique(daub$unique_quad)), "unique_quad"]) 
unique(daub[!(daub$unique_quad %in% unique(ms$unique_quad)), "unique_quad"]) 

#get total cover for each plot
total_cover <- aggregate(daub$Midpoint.value, by = list(daub$Plot, daub$Cover.type), FUN = sum)



## select which plots have enough microsites of each type
plots_to_use_dead <- NA
plots_to_use_live <- NA
for(i in 1:length(unique(ms$Plot))){
  #this is a real friggin mess but it pulls out the plots which have at least one dead and at least one live ms
  plots_to_use_dead[i] <- length(table(ms[ms$Plot == unique(ms$Plot)[i], ]$ms)[which((names(table(ms[ms$Plot == unique(ms$Plot)[i], ]$ms)) == "Dead"))]) != 0L
  plots_to_use_live[i] <- length(table(ms[ms$Plot == unique(ms$Plot)[i], ]$ms)[which((names(table(ms[ms$Plot == unique(ms$Plot)[i], ]$ms)) == "Live"))]) != 0L
  }

plots_to_use <- unique(ms$Plot)[plots_to_use_dead & plots_to_use_live]


grass_elect_results <- data.frame(Plot = character(0),
                                  Dead = numeric(0),
                                  Live = numeric(0),
                                  Inter = numeric(0))

i<-23
for (i in 1:length(plots_to_use)){
  
test_grass_cov <- daub[daub$Cover.type == "Perennial grass" & daub$Plot == as.character(plots_to_use[i]), ]
test_grass_cov <- join(test_grass_cov, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")

test_grass_cov$prop_cov <- test_grass_cov$Midpoint.value / sum(test_grass_cov$Midpoint.value)

cov <- aggregate(test_grass_cov[, "prop_cov"], by = list(test_grass_cov$ms), FUN = sum)
prev <- table(test_grass_cov$ms)/20

elect <- data.frame(Plot = test_grass_cov$Plot[1],
                    Dead = (cov[cov$Group.1 == "Dead",2] - prev[which(names(prev) == "Dead")]) / 
                      (cov[cov$Group.1 == "Dead", 2] + prev[which(names(prev) == "Dead")]),
                    Live = (cov[cov$Group.1 == "Live",2] - prev[which(names(prev) == "Live")]) / 
                      (cov[cov$Group.1 == "Live",2] + prev[which(names(prev) == "Live")]),
                    Inter = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                      (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]))

grass_elect_results <- rbind(grass_elect_results, elect)
}

vioplot(grass_elect_results$Inter, grass_elect_results$Live, grass_elect_results$Dead)



#------------------------------------------------------------------------------------------------------------

forb_elect_results <- data.frame(Plot = character(0),
                                  Dead = numeric(0),
                                  Live = numeric(0),
                                  Inter = numeric(0))
i<-1
for (i in 1:length(plots_to_use)){
  
  test_grass_cov <- daub[daub$Cover.type == "Perennial forb " & daub$Plot == as.character(plots_to_use[i]), ]
  test_grass_cov <- join(test_grass_cov, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
  
  test_grass_cov$prop_cov <- test_grass_cov$Midpoint.value / sum(test_grass_cov$Midpoint.value)
  
  cov <- aggregate(test_grass_cov[, "prop_cov"], by = list(test_grass_cov$ms), FUN = sum)
  prev <- table(test_grass_cov$ms)/20
  
  elect <- data.frame(Plot = test_grass_cov$Plot[1],
                      Dead = (cov[cov$Group.1 == "Dead",2] - prev[which(names(prev) == "Dead")]) / 
                        (cov[cov$Group.1 == "Dead", 2] + prev[which(names(prev) == "Dead")]),
                      Live = (cov[cov$Group.1 == "Live",2] - prev[which(names(prev) == "Live")]) / 
                        (cov[cov$Group.1 == "Live",2] + prev[which(names(prev) == "Live")]),
                      Inter = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                        (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]))
  
  forb_elect_results <- rbind(forb_elect_results, elect)
}


vioplot(forb_elect_results$Inter, forb_elect_results$Live, forb_elect_results$Dead)



#----------------------------------------------------------------------------------------------------------

shrub_elect_results <- data.frame(Plot = character(0),
                                 Dead = numeric(0),
                                 Live = numeric(0),
                                 Inter = numeric(0),
                                 Log = numeric(0))
i<-11
for (i in 1:length(plots_to_use)){
  
  test_grass_cov <- daub[daub$Cover.type == "Shrub " & daub$Plot == as.character(plots_to_use[i]), ]
  test_grass_cov <- join(test_grass_cov, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
  
  test_grass_cov$prop_cov <- test_grass_cov$Midpoint.value / sum(test_grass_cov$Midpoint.value)
  
  cov <- aggregate(test_grass_cov[, "prop_cov"], by = list(test_grass_cov$ms), FUN = sum)
  prev <- table(test_grass_cov$ms)/20
  
  elect <- data.frame(Plot = test_grass_cov$Plot[1],
                      Dead = (cov[cov$Group.1 == "Dead",2] - prev[which(names(prev) == "Dead")]) / 
                        (cov[cov$Group.1 == "Dead", 2] + prev[which(names(prev) == "Dead")]),
                      
                      Live = (cov[cov$Group.1 == "Live",2] - prev[which(names(prev) == "Live")]) / 
                        (cov[cov$Group.1 == "Live",2] + prev[which(names(prev) == "Live")]),
                      
                      Inter = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                        (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]),
                      Log = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                        (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]))
  
  shrub_elect_results <- rbind(shrub_elect_results, elect)
}


vioplot(shrub_elect_results$Inter, shrub_elect_results$Live, shrub_elect_results$Dead)

#-----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------

cheat_elect_results <- data.frame(Plot = character(0),
                                  Dead = numeric(0),
                                  Live = numeric(0),
                                  Inter = numeric(0))
i<-11
for (i in 1:length(plots_to_use)){
  
  test_grass_cov <- daub[daub$Cover.type == "Cheatgrass" & daub$Plot == as.character(plots_to_use[i]), ]
  test_grass_cov <- join(test_grass_cov, ms[, c("unique_quad", "ms")], by = "unique_quad", type = "inner")
  
  test_grass_cov$prop_cov <- test_grass_cov$Midpoint.value / sum(test_grass_cov$Midpoint.value)
  
  cov <- aggregate(test_grass_cov[, "prop_cov"], by = list(test_grass_cov$ms), FUN = sum)
  prev <- table(test_grass_cov$ms)/20
  
  elect <- data.frame(Plot = test_grass_cov$Plot[1],
                      Dead = (cov[cov$Group.1 == "Dead",2] - prev[which(names(prev) == "Dead")]) / 
                        (cov[cov$Group.1 == "Dead", 2] + prev[which(names(prev) == "Dead")]),
                      
                      Live = (cov[cov$Group.1 == "Live",2] - prev[which(names(prev) == "Live")]) / 
                        (cov[cov$Group.1 == "Live",2] + prev[which(names(prev) == "Live")]),
                      
                      Inter = (cov[cov$Group.1 == "Inter",2] - prev[which(names(prev) == "Inter")]) / 
                        (cov[cov$Group.1 == "Inter",2] + prev[which(names(prev) == "Inter")]))
  
  cheat_elect_results <- rbind(cheat_elect_results, elect)
}


vioplot(cheat_elect_results$Inter, cheat_elect_results$Live, cheat_elect_results$Dead)

#-----------------------------------------------------------------------------------------------
# For whole study area
#-----------------------------------------------------------------------------------------------
all_merged <- join(daub, ms, by = "unique_quad", type = "inner")

daub$unique_quad
ms$unique_quad


all_plots_prev <- table(all_merged$ms)
all_plots_prev <- all_plots_prev / sum(all_plots_prev)

all_plots_cover <- aggregate(all_merged$Midpoint.value, by = list(all_merged$Cover.type, all_merged$ms), FUN = sum)
all_plots_cover <- all_plots_cover[all_plots_cover$Group.1 %in% c("Perennial grass", "Perennial forb ", "Shrub ", "Cheatgrass"), ]

for (i in 1:4){ # standardized cover so it adds to 1 for each functional type
  types <- c("Cheatgrass", "Perennial forb ", "Perennial grass", "Shrub ")
  all_plots_cover[all_plots_cover$Group.1 == types[i], "x"] <- all_plots_cover[all_plots_cover$Group.1 == types[i], "x"] / 
    sum(all_plots_cover[all_plots_cover$Group.1 == types[i], "x"])
}



all_plots_cover <- dcast(all_plots_cover, Group.1 ~ Group.2) #reshapes the table

all_elect <- data.frame(type = c("Cheatgrass", "Perennial forb", "Perennial grass", "Shrub"),
                        Dead = numeric(4),
                        Inter = numeric(4),
                        LiveInner = numeric(4),
                        LiveOuter = numeric(4),
                        Log = numeric(4))

for (i in 2:6){ #calculate electivity
  for (j in 1:4){
    all_elect[j, i] <- (all_plots_cover[j,i] - all_plots_prev[i - 1]) / (all_plots_cover[j,i] + all_plots_prev[i - 1])
  }
}


#--------------------------------------------------------------------

spp_merged <- join(spp_cov, ms, by = "unique_quad", type = "inner")
head(spp_merged)


spp_plots_prev <- table(spp_merged$ms)
spp_plots_prev <- spp_plots_prev / sum(spp_plots_prev)

spp_merged$poasec <- spp_merged$poasec / sum(spp_merged$poasec)
spp_merged$othergrass <- spp_merged$othergrass / sum(spp_merged$othergrass)
spp_merged$phlhoo <- spp_merged$phlhoo / sum(spp_merged$phlhoo)
spp_merged$otherforb <- spp_merged$otherforb / sum(spp_merged$otherforb)

spp_ag <- aggregate(spp_merged[, c("poasec", "othergrass", "phlhoo", "otherforb")], by = list(spp_merged$ms), FUN = sum)


spp_elect <- data.frame(type = c("POASEC", "Other Perr Grass", "PHLHOO", "Other Forb"),
                        Dead = numeric(4),
                        Inter = numeric(4),
                        LiveInner = numeric(4),
                        LiveOuter = numeric(4),
                        Log = numeric(4))

for (i in 1:5){ #calculate electivity
  for (j in 1:4){
    spp_elect[j, i+1] <- (unname(spp_ag[i,j + 1]) - unname(all_plots_prev[i])) / (unname(spp_ag[i,j + 1]) + unname(all_plots_prev[i]))
  }
}