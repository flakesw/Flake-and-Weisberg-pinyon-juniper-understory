# calculate soil AWC
# using regressions from Mohamed and Ali 2006 (Model M4 in appendix)
calculate_awc <- function(soil_raw = "soils_missing_imputed_012016.csv"){
soil <- read.csv(soil_raw)

calculate.fc <- function(Sa, Si, Cl){
  fc <- 118.932*(Cl) + 119.0866*(Si) + 119.1104*(Sa) +
    162.31731/(Cl) - 46.21921/(Si) - 5.12991/(Sa) +
    18.1733 * log(Cl) + 0.0013*(Cl)*(Si) + 0.0022*(Si)*(Sa) -
    11939.3493
  return(fc)
}

calculate.pwp <- function(Sa, Si, Cl){
  pwp <- -1.5722 * (Si) - 0.5423*(Sa) - 0.0072*((Cl)^2)+ 0.0072*((Si)^2) -
    0.0059*((Sa)^2) + 160.14591/(Cl) + 6.60011/(Sa) +
    0.0022*(Cl)*(Si) - 0.0039*(Cl)*(Sa) + 92.3851
  return(pwp)
}

Sa <- soil$Percent.Sand
Si <- soil$Percent.Silt
Cl <- soil$Percent.Clay

fc <- calculate.fc(Sa, Si, Cl)
pwp <- calculate.pwp(Sa, Si, Cl)

awc <- fc - pwp
soil_data <- data.frame(Plot = as.character(soil[, 1]), 
                           Pct_Sa = Sa, 
                           Pct_Si = Si, 
                           Pct_Cl = Cl, 
                           Field_cap = fc,
                           Perm_wilt = pwp, 
                           AWC = awc)
return(soil_data)
}
