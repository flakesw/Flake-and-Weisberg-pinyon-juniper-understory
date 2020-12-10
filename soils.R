# Compare soils
# 
library("raster")
library("rgdal")
library("plyr")
library("dplyr")
library("soiltexture")
source("./calculate_awc.R")

plot_centers <- read.csv("./Raw data/plot_centers.csv")

sites <- SpatialPointsDataFrame(plot_centers[, c(3:4)],
                                coords = plot_centers[, c(1:2)],
                                proj4string=CRS("+proj=utm +zone=11N +
                                                ellps=WGS84 +datum=NAD83 +units=m"))

sites <- spTransform(sites, CRS("+proj=longlat + ellps=WGS84 + datum=NAD83"))

ogrListLayers("./Raw data/gssurgo_g_nv/gSSURGO_NV.gdb")

# a polygon layer with all the map units. We'll intersect our points with these
# map unit polygons
ssurgo <- readOGR(dsn = "./Raw data/gssurgo_g_nv/gSSURGO_NV.gdb", 
                  layer = "MUPOLYGON")

#this has the component data; each map unit has several soil components
component <- readOGR(dsn = "./Raw data/gssurgo_g_nv/gSSURGO_NV.gdb", 
                layer = "component",
                dropNULLGeometries = FALSE)

#reduce number of columns in component
component_reduced <- component %>% dplyr::select(comppct_r, cokey, mukey)


#this has all the horizon data we need; each component has several horizons
hori <- readOGR(dsn = "./Raw data/gssurgo_g_nv/gSSURGO_NV.gdb", 
                layer = "chorizon",
                dropNULLGeometries = FALSE)


#reproject sites
sites <- spTransform(sites, crs(ssurgo))

#get the map unit for each point
points_joined <- sp::over(sites, ssurgo) %>% 
                 dplyr::rename_with(tolower) %>%
                 cbind(sites$Plot) %>%
                 dplyr::rename("Plot" = "sites$Plot")

component_bd <- component_reduced[component_reduced$mukey %in% points_joined$mukey, ] %>%
                join(hori[, c("cokey", "hzdept_r", "dbthirdbar_r")], by = "cokey")

points_joined_dens <- join(points_joined, component_bd, by = "mukey")

plot_list <- unique(points_joined_dens$Plot)
dens <- data.frame(Plot = plot_list,
                   dens = NA)

for(i in 1:length(plot_list)){
  #choose horizons within top 15 cm
  select <- points_joined_dens[points_joined_dens$Plot == plot_list[i], ] %>%
            subset(hzdept_r < 15)
  #average horizons for each cokey
  temp <- stats::aggregate(select, by = list(select$cokey), FUN = mean)
  #for each plot, average the components
  dens$dens[i] <- weighted.mean(x = temp$dbthirdbar_r, w = temp$comppct_r, 
                                na.rm = TRUE)
}

names(component)

# export data and run CalcPTF

# soil <- calculate_awc(soil_raw = "./raw data/soils_missing_imputed_012016.csv") 
# names(soil)[2:4] <- c("SAND", "SILT", "CLAY")
# soil <- cbind(soil, dens$dens)
# write.csv(soil, "soil_with_density.csv")

#soil triangle
soil <- read.csv("./Raw data/soils_missing_imputed_with_new_ptf_2020-12-7.csv", skip = 1)
names(soil)[12:14] <- c("SAND", "SILT", "CLAY")

TT.plot(class.sys = "USDA.TT",
        tri.data = soil,
        tri.sum.tst = FALSE
          )

#calculate field capacity and pwp for different models
brookscorey <- function(qr, f, hb, lambda, h){
  if(h <= hb){return(1)}else{
    return((hb/h)^lambda)
  }
}

vangenuchten <- function(qr, qs, a, n, m, h){
  return(1/((1 + (a * h)^n)^m))
}

soil2$awc <- (soil2$Field_cap - soil2$Perm_wilt)/100
soil2$awc1 <- soil2$X330 - soil2$X15000
soil2$awc2 <- soil2$X330.1 - soil2$X15000.1
soil2$awc3 <- soil2$X330.2 - soil2$X15000.2
soil2$awc4 <- soil2$X330.3 - soil2$X15000.3

#import results from CalcPTF
ptf_params <- read.csv("./Raw data/CalcPTF understory 2020-12-08.csv", skip = 1,
                       stringsAsFactors = FALSE)
ptf_params[, c(10:98)] <- apply(ptf_params[, c(10:98)], 2, FUN = as.numeric)

for(i in 1:102){
  #saxton
  soil2$awc5[i] <- brookscorey(ptf_params[i+1, 10], ptf_params[i+1, 11], 
                               ptf_params[i+1, 12], ptf_params[i+1, 13],
                               330) -
                  brookscorey(ptf_params[i+1, 10], ptf_params[i+1, 11], 
                               ptf_params[i+1, 12], ptf_params[i+1, 13],
                              15000)
  
  #campbell shiosawa
  soil2$awc6[i] <- brookscorey(ptf_params[i+1, 15], ptf_params[i+1, 16], 
                               ptf_params[i+1, 17], ptf_params[i+1, 18],
                               330) -
                    brookscorey(ptf_params[i+1, 15], ptf_params[i+1, 16], 
                                ptf_params[i+1, 17], ptf_params[i+1, 18],
                                15000)
  #rawls brackensiek
  soil2$awc7[i] <- brookscorey(ptf_params[i+1, 20], ptf_params[i+1, 21], 
                               ptf_params[i+1, 22], ptf_params[i+1, 23],
                               330) -
                  brookscorey(ptf_params[i+1, 20], ptf_params[i+1, 21], 
                              ptf_params[i+1, 22], ptf_params[i+1, 23],
                              15000)
  #williams
  soil2$awc8[i] <- brookscorey(ptf_params[i+1, 25], ptf_params[i+1, 26], 
                               ptf_params[i+1, 27], ptf_params[i+1, 28],
                               330) -
                  brookscorey(ptf_params[i+1, 25], ptf_params[i+1, 26], 
                              ptf_params[i+1, 27], ptf_params[i+1, 28],
                              15000)
  
  #oosterveld_chang
  soil2$awc9[i] <- brookscorey(ptf_params[i+1, 35], ptf_params[i+1, 36], 
                               ptf_params[i+1, 37], ptf_params[i+1, 38],
                               330) -
                  brookscorey(ptf_params[i+1, 35], ptf_params[i+1, 36], 
                              ptf_params[i+1, 37], ptf_params[i+1, 38],
                              15000)
  #wosten_chang 
  soil2$awc10[i] <- vangenuchten(ptf_params[i+1, 45], ptf_params[i+1, 46], 
                                 ptf_params[i+1, 47], ptf_params[i+1, 48],
                                 ptf_params[i+1, 49], 330) -
                    vangenuchten(ptf_params[i+1, 45], ptf_params[i+1, 46], 
                                 ptf_params[i+1, 47], ptf_params[i+1, 48],
                                 ptf_params[i+1, 49], 15000)
  
  #varallyay
  soil2$awc11[i] <- vangenuchten(ptf_params[i+1, 51], ptf_params[i+1, 52], 
                                 ptf_params[i+1, 53], ptf_params[i+1, 54],
                                 ptf_params[i+1, 55], 330) -
                    vangenuchten(ptf_params[i+1, 51], ptf_params[i+1, 52], 
                                 ptf_params[i+1, 53], ptf_params[i+1, 54],
                                 ptf_params[i+1, 55], 15000)
  
}

soil2$mean_awc <- apply(soil2[, c(27:38)], 1, mean)


write.csv(soil2, "./Raw data/soil_")