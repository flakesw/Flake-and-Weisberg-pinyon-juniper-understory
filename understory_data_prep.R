# Understory data formatting
# author: Sam Flake
# email: sflake@gmail.com
# Description: this script takes all the raw data and outputs clean plot-level data
# including predictor and response variables

library("reshape2")
library("plyr")
library("effects")


# import daubenmire cover data
daub <- read.csv("./raw data/daub_cover.csv", stringsAsFactors = FALSE)

# some data proofing
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
  
# 2015 tree data
trees <- read.csv("./raw data/trees_updated_with_logs_041716.csv")
tree_pdc <- read.csv("./raw data/all_trees_with_delta_and_ENN_041916.csv")

# individual tree data from Greenwood and Weisberg 2008
greenwood_trees <- read.csv("./raw data/Individual_TreeData_AllData_SF_edits.csv")

# 2005 understory data, from Greenwood and Weisberg, 2008, 
# Density-dependent tree mortality in pinyon-juniper woodlands, FEM
greenwood_under <- read.csv("./raw data/Greenwood_Understory_Variables_SF_edits.csv")

# 2015 line-intercept tree cover data
tcover <- read.csv("./raw data/tree_and_shrub_cover_020815.csv")
  names(tcover)[3] <- "Shrub_cover_li"
  
# 2005 tree cover and other variables from Greenwood and Weisberg 2008
greenwood_tree_cover <- read.csv("./raw data/Structure_vs_environmental_all_data_SF_edits.csv")
  
# calculate basal area of ingrowths
# in_ba <- aggregate(trees[trees$Code == "P", "BA_m2_per_ha"], 
#           by = list(trees[trees$Code == "P", "Plot"]), FUN = sum)
# names(in_ba) <- c("Plot", "In_ba")
# in_ba_data <-  join(in_ba, plot_data, by = c("Plot"))

# soil data
source("./calculate_awc.R")
soil <- calculate_awc(soil_raw = "./raw data/soils_missing_imputed_012016.csv")

# climate data from PRISM
clim <- read.csv("./raw data/ALL_climate_variables.csv")
  names(clim)[2] <- "Plot"
  
# some other derived variables
other_vars <- read.csv("./raw data/all_vars_EXPORT.csv")

#remaking daubenmire data into plot average cover
plot_daub_cover <- dcast(daub, Plot ~ Cover.type, value.var = "Midpoint.value", fun.aggregate = mean)
names(plot_daub_cover) <- c("Plot", "Aforb", "Bg", "Cheatgrass", "Crust", "Gravel", "Litter",
                            "Agrass", "Pforb", "Pgrass", "Rock", "Shrub")
plot_daub_cover$All <- plot_daub_cover$Aforb + plot_daub_cover$Cheatgrass + plot_daub_cover$Agrass + plot_daub_cover$Pforb +
                      plot_daub_cover$Pgrass + plot_daub_cover$Shrub


#-------------------------------------------------------------
#Create tree data plot-level

#impute means for missing values
for (i in which(sapply(trees, is.numeric))) {
  for (j in which(is.na(trees[, i]))) {
    trees[j, i] <- mean(trees[trees[, "Plot"] == trees[j, "Plot"], i],  na.rm = TRUE)
  }
}

# calculate stand structural variables
plot_trees <- data.frame(Plot = character(102),
                         Live_ba = numeric(102),
                         Live_ba_2005 = numeric(102),
                         Dead_ba = numeric(102),
                         Total_ba = numeric(102),
                         Dead_rat = numeric(102),
                         Delta_pdc = numeric(102))

plot_trees$Plot <- as.character(unique(plot_daub_cover$Plot))

for(i in (1:nrow(plot_trees))){
  plot_trees$Live_ba[i] <- sum(trees[trees$Live == "Y" & trees$Plot == as.character(plot_trees$Plot[i]), ]$BA_m2_per_ha,
                               na.rm = TRUE)
  plot_trees$Live_ba_2005[i] <- sum((((greenwood_trees[greenwood_trees$Plot == as.character(plot_trees$Plot[i]) &
                                                         greenwood_trees$LiveDead == "L", ]$Diam)/2)^2))/1000
  plot_trees$Dead_ba[i] <- sum(trees[trees$Live == "N" & trees$Plot == as.character(plot_trees$Plot[i]), ]$BA_m2_per_ha,
                               na.rm = TRUE)
  plot_trees$Total_ba[i] <- sum(trees[trees$Plot == as.character(plot_trees$Plot[i]), ]$BA_m2_per_ha, na.rm = TRUE)
  plot_trees$Dead_rat[i] <- plot_trees$Dead_ba[i] / plot_trees$Total_ba[i]
  plot_trees$Delta_pdc[i] <- weighted.mean(tree_pdc[tree_pdc$Plot == as.character(plot_trees$Plot[i]), "Delta_pdc"],
                                        tree_pdc[tree_pdc$Plot == as.character(plot_trees$Plot[i]), "BA_cm"])
  
  
  }


#-----------------------------------------------
# Plot-level cover for everything and also predictor variables
# join all the data together by plot
plot_data <- join(plot_daub_cover, plot_trees, by = c("Plot"))
plot_data <- join(plot_data, clim[, c("Plot", "cwd_normal_cum")], by = "Plot")
plot_data <- join(plot_data, soil[, c("Plot", "AWC")], by = "Plot")
plot_data <- join(plot_data, tcover[, c("Plot", "Tree_cover", "Shrub_cover_li")], by = "Plot")
plot_data <- join(plot_data, greenwood_tree_cover[, c("Plot", "Total_cover", "BA_plot")], by = "Plot")
plot_data <- join(plot_data, other_vars[, c("Plot", "Cluster", "Avg_depth")], by = "Plot")


#fill in any NAs with the mean 
for (i in which(sapply(plot_data, is.numeric))) {
  for (j in which(is.na(plot_data[, i]))) {
    plot_data[j, i] <- mean(plot_data[, i],  na.rm = TRUE)
  }
}

#rename a cluster
levels(plot_data$Cluster) <- c(levels(plot_data$Cluster), "NPELECTRICEEL")
plot_data[(plot_data$Plot %in% c("NPELECTRICEEL", "NPELECTRICEEL120",
                                 "NPELECTRICEEL240", "NPELECTRICEEL360")), "Cluster"] <- "NPELECTRICEEL"


write.csv(plot_data, "./clean data/plot_data.csv")