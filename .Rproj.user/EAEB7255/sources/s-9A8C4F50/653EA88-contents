##Understory data formatting
##
library("reshape2")
library("plyr")
library("effects")

daub <- read.csv("./raw data/daub_cover.csv")
  daub$Midpoint.value <- as.numeric(as.character((daub$Midpoint.value)))
trees <- read.csv("./raw data/trees_updated_with_logs_041716.csv")
tree_pdc <- read.csv("./raw data/all_trees_with_delta_and_ENN_041916.csv")
greenwood_under <- read.csv("./raw data/Greenwood_Understory_Variables_SF_edits.csv")
tcover <- read.csv("./raw data/tree_and_shrub_cover_020815.csv")
  names(tcover)[3] <- "Shrub_cover_li"
source("./calculate_awc.R")
soil <- calculate_awc(soil_raw = "./raw data/soils_missing_imputed_012016.csv")
clim <- read.csv("./raw data/ALL_climate_variables.csv")
  names(clim)[2] <- "Plot"
other_vars <- read.csv("./raw data/all_vars_EXPORT.csv")


#remaking data into plot average cover
plot_daub_cover <- dcast(daub, Plot ~ Cover.type, value.var = "Midpoint.value", fun.aggregate = mean)
names(plot_daub_cover) <- c("Plot", "Aforb", "Bg", "Cheatgrass", "Crust", "Gravel", "Litter",
                            "Agrass", "Pforb", "Pgrass", "Rock", "Shrub")
#-------------------------------------------------------------
#Create tree data plot-level

#impute meanas for missing values
for (i in which(sapply(trees, is.numeric))) {
  for (j in which(is.na(trees[, i]))) {
    trees[j, i] <- mean(trees[trees[, "Plot"] == trees[j, "Plot"], i],  na.rm = TRUE)
  }
}

plot_trees <- data.frame(Plot = character(102),
                         Live_ba = numeric(102),
                         Dead_ba = numeric(102),
                         Total_ba = numeric(102),
                         Dead_rat = numeric(102),
                         Delta_pdc = numeric(102))

plot_trees$Plot <- as.character(unique(plot_daub_cover$Plot))

for(i in (1:nrow(plot_trees))){
  plot_trees$Live_ba[i] <- sum(trees[trees$Live == "Y" & trees$Plot == as.character(plot_trees$Plot[i]), ]$BA_cm,
                               na.rm = TRUE)
  plot_trees$Dead_ba[i] <- sum(trees[trees$Live == "N" & trees$Plot == as.character(plot_trees$Plot[i]), ]$BA_cm,
                               na.rm = TRUE)
  plot_trees$Total_ba[i] <- sum(trees[trees$Plot == as.character(plot_trees$Plot[i]), ]$BA_cm, na.rm = TRUE)
  plot_trees$Dead_rat[i] <- plot_trees$Dead_ba[i] / plot_trees$Total_ba[i]
  plot_trees$Delta_pdc[i] <- weighted.mean(tree_pdc[tree_pdc$Plot == as.character(plot_trees$Plot[i]), "Delta_pdc"],
                                        tree_pdc[tree_pdc$Plot == as.character(plot_trees$Plot[i]), "BA_cm"])
}


#-----------------------------------------------
# Plot-level cover for everything and also predictor variables

#fill in any NAs

plot_data <- join(plot_daub_cover, plot_trees, by = c("Plot"))
plot_data <- join(plot_data, clim[, c("Plot", "cwd_normal_cum")], by = "Plot")
plot_data <- join(plot_data, soil[, c("Plot", "AWC")], by = "Plot")
plot_data <- join(plot_data, tcover[, c("Plot", "Tree_cover", "Shrub_cover_li")], by = "Plot")
plot_data <- join(plot_data, other_vars[, c("Plot", "Cluster", "Avg_depth")], by = "Plot")


for (i in which(sapply(plot_data, is.numeric))) {
  for (j in which(is.na(plot_data[, i]))) {
    plot_data[j, i] <- mean(plot_data[, i],  na.rm = TRUE)
  }
}

levels(plot_data$Cluster) <- c(levels(plot_data$Cluster), "NPELECTRICEEL")
plot_data[(plot_data$Plot %in% c("NPELECTRICEEL", "NPELECTRICEEL120",
                                 "NPELECTRICEEL240", "NPELECTRICEEL360")), "Cluster"] <- "NPELECTRICEEL"


write.csv(plot_data, "./clean data/plot_data.csv")