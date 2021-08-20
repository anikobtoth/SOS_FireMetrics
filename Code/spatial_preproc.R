## load packages
#library(tidyverse)
library(parallel)
## load functions
source("./Code/Helper_functions.R")

## spatial processing 

ecosystem <- st_read("../Spatialdata_forAniko/TECDist/TEC5", "NSWTEC5_ShaleSandSydney") 
fire <- st_read("../Spatialdata_forAniko/FireNPWSHistory/firenpwsfirehistory", "Fire_NPWS_FireHistory") 
assess_year <- c(2020, 2015, 2010, 2005, 2000)

ncores <- min(length(assess_year), detectCores()) # ensure user's system is not overwhelmed

# parallelise to make it faster
cl <- makeCluster(ncores)
clusterEvalQ(cl,c(source('./Code/Helper_Functions.R')))
clusterExport(cl, c("fire", "ecosystem"))

# This took about 10 minutes on my computer
FireHist <- parLapply(cl, assess_year, function(x) firehistory(ecosystem, fire, x)) 
FireHist <- bind_rows(FireHist)

# To run subsets or multiple ecosystems, load new ecosystem shapefile and repeat. E.g.:
    # ecosystem_subset <- st_read("path/name/here", "layer_name_here") 
    # FireHist_subset <- parLapply(cl, assess_year, function(x) firehistory(ecosystem_subset, fire, x)) 
    # FireHist_subset <- bind_rows(FireHist_subset)

# Join into one table with ecosystems or subsets as columns
    # FireHist <- full_join(FireHist, FireHist_subset, by = c("LastFire", "Assessment_Year"), suffix = c("_Full", "_Subset")) 

# for a large number of ecosystems, could write a loop or helper function

## save as .csv to use with Shiny app
write_csv(FireHist, "./Input_data.csv")

# **** Input data MUST have columns "LastFire" and "Assessment_Year" as output by this code. *****
# **** Ecosystem area columns MUST contain "ObsArea" (can be prefix or suffix, doesn't matter) ****


### Metric calculations #####

tslf <- c(MIN = 6, MEAN = 7, MAX = 15) # Must be named

dat <- expected_fire(FireHist, tslf) # obs and expected percentages

shf <- shortfalls(dat, tslf) # shortfalls

# detailed table (throws warning if only one observed column was input)
shf %>% select(group, gi, interval, contains("Shortfall")) %>%
  rename_with(fix_names, contains("Shortfall")) %>%
  separate(group, into = c("Year", "TSLF"))

# summary shortfall metric table
shf %>% select(group, gi, interval, contains("Shortfall")) %>% 
  group_by(group) %>% summarise(across(contains("Shortfall"), sum)) %>% 
  separate(group, into = c("Year", "TSLF")) %>%
  rename_with(fix_names, contains("Shortfall"))

# Plot frequency curves
freq_curves(dat, yr = 2015)

# Plot area graph 
area_graph(shf)

# Ribbon plot (currently handles up to 5 observed columns)
ribbon_plot(shf)

