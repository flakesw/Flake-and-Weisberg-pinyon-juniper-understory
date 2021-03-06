Understory
Analysis of understory vegetation response to tree mortality in pinyon-juniper woodlands
Author: Sam Flake
Contact: sflake@gmail.com

These data and scripts are free to use and modify for any non-commercial purpose. Please cite the manuscript (submitted -- will update after acceptance) associated with these data, or this repository, if the data are used. 

This project contains raw data and analysis scripts for work done as part of my master's thesis at the University of Nevada, Reno (Flake, 2016). The data were collected primarily in the summer of 2015 across central Nevada (contact me or Peter Weisberg at pweisberg@cabnr.unr.edu if you need additional data or plot location data). The project was supported by USDA NIFA, Hatch project 1003021. The data files are documented in a separate readme in the raw data folder. 

For details on methodology, see the following sources:
Flake, S.W. and P.J. Weisberg. Submitted. Drought-induced tree mortality alters understory community composition and facilitates cheatgrass invasion in pinyon-juniper woodlands. 
Flake, S. W. 2016. Stand dynamics during drought: Responses of adult trees, tree regeneration, and understory vegetation to multiyear drought in pinyon-juniper woodlands. University of Nevada, Reno.
Flake, S. W., and P. J. Weisberg. 2019. Fine-scale stand structure mediates drought-induced tree mortality in pinyon–juniper woodlands. Ecological Applications 29:e01831.
Greenwood, D. 2006. Landscape analysis of tree mortality and pinyon-juniper woodland structure in the Great Basin. University of Nevada, Reno.
Greenwood, D. L., and P. J. Weisberg. 2008. Density-dependent tree mortality in pinyon-juniper woodlands. Forest Ecology and Management 255:2129–2137.


The project contains:

understory_data_prep.R -- this script takes the raw data (from the /raw data folder) and outputs processed data (to the /Clean data folder). 
plot_level_understory.R -- this script performs regression analysis, t-tests, and some exploratory statistics to relate understory cover (at the plot level) to environmental variables. This script is the bulk of the stats.
understory_electivity.R -- this script conducts the electivity analysis, briefly, comparing cover in different microhabitats. This is a within-plot sort of analysis, rather than between plots.
understory_effects_plots.R -- this script just makes pretty effects plots of the regressions performed in plot_level_understory.R

some helper functions:
calculate_awc.R -- calculates some soil variables
addTrans.R -- adds transparency to figures

A few folders:
/raw data/ this is where the raw, unprocessed data goes. It's more or less proofed, though a little processing goes on in the understory_data_prep.R script
/Clean data/ this just has one file in it, the processed output from understory_data_prep.R
/outputs/ this folder contains the figures produced by the scripts, .rds files that save the raw Monte Carlo randomizations, and some tables of summary data. 

And some other junk associated with RStudio that isn't important.

Hopefully these scripts are self-explanatory. Please contact me (sflake@gmail.com) with questions.
