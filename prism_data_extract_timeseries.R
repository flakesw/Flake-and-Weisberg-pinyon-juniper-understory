# extract PRISM data to remake a figure 
# Sam Flake 6 August 2020
# some code and assistance courtesy Miranda Redmond and Tom Dilts

#load packages

library(raster)
library(rgdal)
library(foreign)
library(plyr)
source("addTrans.R")

set.seed(218218)
setwd("C:/Users/Sam/Documents/Research/MS Thesis/Understory/")

plot_centers <- read.csv("./Raw data/plot_centers.csv")

sites <- SpatialPointsDataFrame(plot_centers[, c(3:4)],
                                coords = plot_centers[, c(1:2)],
                                proj4string=CRS("+proj=utm +zone=11N +
                                                ellps=WGS84 +datum=NAD83 +units=m"))

sites <- spTransform(sites, CRS("+proj=longlat + ellps=WGS84 + datum=NAD83"))

months<-c('01','02','03','04','05','06','07','08','09','10','11','12') #months of interest
startyear <- 1895
endyear <- 2019
nyear <- endyear - startyear
npoints <- 102 #number of sites in shapefile

# N <- (endyear - startyear + 1) * length(months) * npoints

#intialize dataframe to catch all the data
clim_data <- data.frame(site = character(0),
                       year = numeric(0),
                       month = numeric(0),
                       yearmonth = character(0),
                       tdmean = numeric(0),
                       tmean = numeric(0),
                       ppt = numeric(0)
                       )

#double loop through years and months, extract tmean, tdmean, and ppt, at each site each month
for (j in startyear:endyear){
  
  for(i in 1:length(months)){
    #dewpoint rasters
    tdraster<-paste('./Raw data/prism_data/PRISM_tdmean_stable_4kmM3_',j,months[i],'_bil.bil',sep="")   #raster file locations
    tdrast <- raster(tdraster) #import raster
    proj4string(tdrast) <- CRS("+proj=longlat + ellps=GRS80 + datum=NAD83") #add proper projection
    
    #mean temperature rasters
    traster<-paste('./Raw data/prism_data/PRISM_tmean_stable_4kmM3_',j,months[i],'_bil.bil',sep="") 
    trast <- raster(traster)
    proj4string(trast) <- CRS("+proj=longlat + ellps=GRS80 + datum=NAD83") #add a projection
    
    #mean temperature rasters
    pptraster <- list.files("./Raw data/prism_data/", pattern=paste('.*ppt.*', j,months[i],'_bil.bil$', sep = ""))
    pptrast <- raster(paste0('./Raw data/prism_data/', pptraster))
    proj4string(pptrast) <- CRS("+proj=longlat + ellps=GRS80 + datum=NAD83") #add a projection
    
    #extract points
    tdext.poly <- raster::extract(tdrast, sites, fun = mean, na.rm=TRUE, df=TRUE, verbose=FALSE)
    tdmean<-tdext.poly[,2] / 100
    
    text.poly <- raster::extract(trast, sites, fun = mean, na.rm=TRUE, df=TRUE, verbose=FALSE)
    tmean<-text.poly[,2]
    
    pptext.poly <- raster::extract(pptrast, sites, fun = mean, na.rm = TRUE, df = TRUE, verbose = FALSE)
    ppt <- pptext.poly[, 2]
    
    #intialize and fill temporary df
    climate_month_data <- data.frame(site = sites$Plot,
                                     year = rep(j,length(tdext.poly[,2])),
                                     month = rep(months[i],length(tdext.poly[,2])),
                                     yearmonth = paste0(rep(j,length(tdext.poly[,2])), rep(months[i],length(tdext.poly[,2]))),
                                     tdmean = tdmean,
                                     tmean = tmean,
                                     ppt = ppt
                                     )

    #add temp df to main df
    clim_data <- rbind(clim_data, climate_month_data)
    }
  
}

### calculate vpd for each month (from park williams)
Tavg<-clim_data$tmean
a0<-6.107799961
a1<-.4436518521
a2<-.01428945805
a3<-.0002650648471
a4<-.000003031240396 
a5<-.00000002034080948
a6<-.00000000006136820929
D<-clim_data$tdmean
SVP <- (a0+Tavg*(a1+Tavg*(a2+Tavg*(a3+Tavg*(a4+Tavg*(a5+Tavg*a6))))))
AVP <- (a0+D*(a1+D*(a2+D*(a3+D*(a4+D*(a5+D*a6))))))
VPDm <- SVP-AVP
VPD <- round((0.2358 + 1.0694*(VPDm)),4)
clim_data$vpd<-VPD
clim_data[clim_data$vpd < 0, "vpd"]<- 0

write.csv(clim_data, file="./clean data/climate_data_monthly.csv")
clim_data <- read.csv("./clean data/climate_data_monthly.csv")
#some code to plot points on top of raster to make sure stuff looks right
# e <- drawExtent()
# plot(e)
# plot(tdrast, add = TRUE)
# points(sites)


#initialize data frame
clim_ann <- data.frame(plot = rep(unique(clim_data$site), each = nyear),
                       year = rep(seq(startyear+1, endyear), times = npoints),
                       Pndjfm = NA,
                       VPDaso = NA,
                       VPDmjj = NA,
                       logP = NA,
                       vpdtot = NA,
                       tmean = NA,
                       tmax = NA,
                       vpdmax = NA,
                       ppt_tot = NA)



#extract winter precip and last-fall + current-season VPD
for (i in 1:npoints){
  plot <- clim_ann$plot[i * nyear]
  temp <- clim_data[clim_data$site == plot, ] #split out all data for one plot
  
  for (j in 1:nyear){
    clim_ann$Pndjfm[(i-1) * nyear + j] <- sum(temp[temp$year == (startyear + j) & temp$month %in% c('01', '02', '03'), "ppt"],
                                            temp[temp$year == (startyear + j - 1) & temp$month %in% c('11', '12'), "ppt"])  
    
    clim_ann$VPDaso[(i-1) * nyear + j] <- sum(temp[temp$year == (startyear + j - 1) & temp$month %in% c('08', '09', '10'), "vpd"])  
    
    clim_ann$VPDmjj[(i-1) * nyear + j] <- sum(temp[temp$year == (startyear + j) & temp$month %in% c('01', '02', '03'), "vpd"])  
    
    clim_ann$tmean[(i-1) * nyear + j] <- mean(temp[temp$year == (startyear + j), "tmean"])  
    
    clim_ann$tmax[(i-1) * nyear + j] <- max(temp[temp$year == (startyear + j), "tmean"])  
    
    clim_ann$ppt_tot[(i-1) * nyear + j] <- sum(temp[temp$year == (startyear + j), "ppt"])  
    
    clim_ann$vpdmax[(i-1) * nyear + j] <- max(temp[temp$year == (startyear + j), "vpd"])
    
    }
  
}


#take logs and add previous year's vpd to current year's
clim_ann$logP <- log(clim_ann$Pndjfm)
clim_ann$vpdtot <- (clim_ann$VPDaso + clim_ann$VPDmjj) /2

means_sds <- data.frame(plot = unique(clim_ann$plot),
                        meanlogp = NA,
                        sdlogp = NA,
                        meanvpd = NA,
                        sdvpd = NA,
                        meanppt_tot = NA,
                        mean_tmean = NA)


#Calcualte means and sds to standardize each logP and vpd_tot value
for (i in 1:102){
  means_sds$meanlogp[i] <- mean(clim_ann[clim_ann$plot == means_sds$plot[i], "logP"])
  means_sds$meanvpd[i] <- mean(clim_ann[clim_ann$plot == means_sds$plot[i], "vpdtot"])
  means_sds$meanppt_tot[i] <- mean(clim_ann[clim_ann$plot == means_sds$plot[i], "ppt_tot"])
  means_sds$mean_tmean[i] <- mean(clim_ann[clim_ann$plot == means_sds$plot[i], "tmean"])
  means_sds$sdlogp[i] <- sd(clim_ann[clim_ann$plot == means_sds$plot[i], "logP"])
  means_sds$sdvpd[i] <- sd(clim_ann[clim_ann$plot == means_sds$plot[i], "vpdtot"])
}

#standardize everything
clim_ann$stdP <- NA
clim_ann$stdVPD <- NA
for (i in 1:nrow(clim_ann)){
  clim_ann$stdP[i] <- ((clim_ann$logP[i] - means_sds[means_sds$plot == clim_ann$plot[i], "meanlogp"]) / means_sds[means_sds$plot == clim_ann$plot[i], "sdlogp"])
  clim_ann$stdVPD[i] <- ((clim_ann$vpdtot[i] - means_sds[means_sds$plot == clim_ann$plot[i], "meanvpd"]) / means_sds[means_sds$plot == clim_ann$plot[i], "sdvpd"])
  clim_ann$MAP_anom[i] <- (clim_ann$ppt_tot[i] - means_sds[means_sds$plot == clim_ann$plot[i], "meanppt_tot"])
  clim_ann$MAT_anom[i] <-  (clim_ann$tmean[i] - means_sds[means_sds$plot == clim_ann$plot[i], "mean_tmean"])                     
  
}

#calculation from williams et al 2013
clim_ann$fdsi <- .44*clim_ann$stdP - .56*clim_ann$stdVPD


write.csv(clim_ann, file = "./clean data/climate_data_monthly.csv")

#-------------------------------------------------------------------------------
# Thornthwaite water balance
#-------------------------------------------------------------------------------


