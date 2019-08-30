The raw data for the project contains, generally, understory measurements contained in Understory_update_110418.xlsx, the spreadsheets of which were split out into individual .csv files; two spreadsheets containing data on trees; climate and soil variables; and data from Greenwood and Weisberg (2008). The best information on methodologies is probably from two master's theses, Flake (2016) and Greenwood (2006). A lot of the data here is not used for the present analysis. 

The data were measured in 102 plots from 26 clusters, each of 4 plots. One cluster only had two plots sampled from it. Within each plot, we measured individual trees (Flake and Weisberg 2019) and understory vegetation. Understory vegetation was measured with 20 1-m2 quadats per plot, and shrubs were measured with the line-intercept method as well as within quadrats. Cover by individual species was measured in quadrats as well as by functional type (the analysis presented in the manuscript). The microsite was characterized for each quadrat (e.g., under tree, interspace, etc.). Plot IDs consist of several parts: the mountain range code (e.g. SS for Sulphur Spring; TOI for Toiyabe), an assigned cluster number (e.g. 533), and then a direction -- 120, 240, or 360 degrees from the central subplot. The central plot is named the same as the cluster (e.g. TOI533), and the other plots are named something like TOI533360. See Greenwood (2005) for more details. 

References:
Flake, S. W. 2016. Stand dynamics during drought: Responses of adult trees, tree regeneration, and understory vegetation to multiyear drought in pinyon-juniper woodlands. University of Nevada, Reno.
Flake, S. W., and P. J. Weisberg. 2019. Fine-scale stand structure mediates drought-induced tree mortality in pinyon–juniper woodlands. Ecological Applications 29:e01831.
Greenwood, D. 2006. Landscape analysis of tree mortality and pinyon-juniper woodland structure in the Great Basin. University of Nevada, Reno.
Greenwood, D. L., and P. J. Weisberg. 2008. Density-dependent tree mortality in pinyon-juniper woodlands. Forest Ecology and Management 255:2129–2137.




Details of data files:

Understory data:

Understory_update_110418.xlsx: contains several spreadsheets with understory vegetation information
  Shrub cover: line-intercept data for shrub cover
    Plot: ID code for the plot
    Date: Date sampled
    Observers: initials of who did the sampling
    Transect: which cardinal direction transect
    Shrub spp.: Species encountered
    Live: whether shrub was alive or dead
    Range1: beginning of intersection with transect
    Range2: end of intersection of transect
    Net cover: length of transect intersecting shrub
  New seedsap: belt-transect sampling of tree juveniles
    Plot: see above
    Date: see above
    Observer: see above
    Microsite: What habitat was the seedling or sapling found in?
    P Seed, P Sap, J Seed, J Sap, C Seed, C Sap: tallies of seedlings and saplings found in each microsite within the belt transect
  Daubenmire cover: cover types in binned classes, within each of 20 quadrats per plot
    Plot: see above
    Date: see above
    Observer: see above
    Transect: see above
    Meter: distance from center of the plot along the transect where the quadrat was positioned
    Cover type: cover type, including ground cover types and plant functional groups
    Cover class: Daubenmire cover class for the cover type within the quadrat, from 0 to 6
    Midpoint value: Midpoint of the cover class, to convert classes to percent cover
  Species quadrat cover: cover by individual species
    Plot, date, observer, transect, meter: see above
    Cover: percent cover of the species within the quadrat, estimated to the nearest percent
  microsite: Microhabitat characteristics for each quadrat
    Plot, date, observer, transect, meter: see above
    Microsite: microsite code describing the microhabitat of the quadrat. Can be I (interspace), PI (pinyon inner canopy), PO (pinyon outer), JI (juniper inner), JO (juniper outer), CI (Cercocarpus inner), CO (Cercocarpus outer), or log (fallen log). Codes can have (S) added after the code to indicate that the tree is a snag
    Tree: tree number associated with the microsite (which tree is covering the quadrat most)
    Alt microsite: another plausible microsite code when it was not clear. Often when two trees both covered the quadrat, or a tree and a log, or something of that sort. 
    Alt tree: other trees that cover the quadrat
  Site info: information about site conditions, including soil depth, aspect, slope steepness
    Plot, cluster, date, observer: see above
    Aspect: Dominant aspect measured with a compass
    Slope: slope steepness estimated with a clinometer
    Soil1 - Soil5: soil depth measured with a metal rod hammered into the soil
    SSURGO_depth: soil depth obtained from SSURGO gridded soil data
    Avg_depth: average of Soil1-Soil5, or the value from SSURGO
    Max_depth: max of Soil1-Soil5, or the value from SSURGO
  Spp codes: species list used by me to translate from codes written in the field to actual species identity
  
spp_cover2.csv: this corresponds to the "Species quadrat cover" sheet in Understory_update_110418.xlsx

microsite.csv: this corresponds to the "microsite" sheet in Understory_update_110418.xlsx

daub_cover.csv: this corresponds to the "Daubenmire cover" sheet in Understory_update_110418.xlsx

tree_and_shrub_cover_020815.csv: line-intercept cover for shrubs and trees
    Plot: plot ID
    Percent_cover: percent shrub cover (from LI)
    tree_cover: percent tree cover (from LI)
  
ALL_climate_variables.csv: climate variables extracted from PRISM climate data. See Flake (2016) or Flake and Weisberg (2019) for details on variable definitions. Only cwd_normal_cum is used here.

all_vars_EXPORT.csv: 
    Plot: Plot ID
    Avg_pdc: average percent dead crown of trees in 2015
    Avg_delta_pdc: average change (in percentage points) from 2005 to 2015
    Weighted_pdc: average change in pdc, weighted by basal area. The units are inscrutable and this column doesn't get used
    prop_died: proportion of trees which died
    n. died: number of trees which died
    Cluster: cluster ID
    Swness: how southwest the plot is facing. I forget how it's calculated; taking the cosine of the slope direction somehow
    Slope: slope angle, extracted from a DEM I believe
    Elev: elevation from a DEM
    Tpi: topographic position index. I forget how it was calculated.
    Cum_cwd: annual normal cumulative climatic water deficit. This variable is not used from this spreadsheet; rather, it is in the climate variable spreadsheet.
    Percent.clay: percent surface soil clay
    Avg_depth: average soil depth, measured in field or from SSURGO
    Pct_pimo: percent of trees which are pinyon pine
    Pct_juos: percent of trees which are juniper
    SDI.Plot: Reineke's stand density index
    Weighted_pimo: Weighted_pdc for pinyon
    Weighted_juos: weighted_pdc for juniper
    
 all_trees_with_delta_and_ENN_041916.csv: information on individual trees. This includes ONLY trees with data from both 2005 and 2015. For all tree data, use trees_updated_with_logs_041716.csv
    Number: tree number within plot
    Plot: plot ID
    Unique_tree: a unique identifier for each tree
    Date: date sampled
    Observers: who measured the trees
    Spp: species (PIMO for Pinus monophylla, JUOS for Juniperus osteosperma, CELE for Cercocarpus ledifolius)
    Live: tree alive (Y/N)
    Code: special case code: B blowdown; X tree missing (not found); I ingrowth (new adult-size tree); M missed (tree should have been sampled in 2005 but was missed); O out (tree out of plot boundaries). 
    X.Stems: no idea 00 should be number of stems but it got transformed somehow
    Diameters: largest diameter
    DecayClass: should be a code rom 1-4, but data got corrupted somehow
    Height: tree height in meters
    Cones.Brown: estimate of number of brown cones
    Cones.Green: estimate of green cones
    MortalityCause1 - MortalityCause5: mortality agents found on tree, mostly insects
    DMR: Dwarf Mistletoe rating, between 0 and 6
    PerDeadCrown: percent dead crown
    Easting and Northing: not used
    Age: Age class between 1 and 3 of tree
    Diameters2 - Diameters102: additional diameters for multi-stemmed trees
    BA_cm: cross-sectional area of tree in cm2
    BA_m2: cross-sectional area in m2, divided by 10 (sum this within the plot for BA per hectare)
    ENN_dist to SDI_6m: not used, see Flake and Weisberg (2019)
    LiveDead: Live or Dead in 2005? (L/D)
    PDC05: percent dead crown in 2005 (from Greenwood 2006)
    Delta_pdc: change in crown from 2005 to 2015. Used to calculate Avg_pdc used in this study
    
    
trees_updated_with_logs_041716.csv: raw tree data from 2015, including trees which fell between 2005 and 2015 (logs). The fields are all the same as above, but the data might be a little different because missing values have not been imputed here. Use this data if you want to know about all the trees sampled in 2015, and are not interested in change in canopy health. 

    


