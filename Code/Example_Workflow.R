## load packages
#library(tidyverse)
library(parallel)
## load functions
source("./Code/Helper_functions.R")

# Load Fire data and assessment year
assess_year <- c(2020, 2015, 2010, 2005, 2000)
ncores <- min(length(assess_year), detectCores()) # ensure user's system is not overwhelmed

## spatial processing for SINGLE ECOSYSTEM #####
ecosystem <- st_read("../Spatialdata_forAniko/TECDist/TEC5", "NSWTEC5_ShaleSandSydney") 

# parallelise to make it faster
cl <- makeCluster(ncores)
clusterEvalQ(cl,c(source('./Code/Helper_Functions.R')))
clusterExport(cl, c("ecosystem"))
  
 # This took about 10 minutes on my computer for TEC5
FireHist <- parLapply(cl, assess_year, function(x) firehistory(ecosystem, x)) 
FireHist <- bind_rows(FireHist)
  
## Spatial processing for MULTIPLE ECOSYSTEMS ############
# for multiple ecosystems use a loop

ecosystems <- list.files("../Spatialdata_forAniko/Spatialdata2_forAniko/", ".shp$") %>% 
  word(1, 1, "\\.") %>% 
  purrr::map(~st_read("../Spatialdata_forAniko/Spatialdata2_forAniko", .x))

FireHist <- list()
for(i in seq_along(ecosystems)){
  ecosystem <- ecosystems[[i]]
  # parallelise to make it faster
  cl <- makeCluster(ncores)
  clusterEvalQ(cl,c(source('./Code/Helper_Functions.R')))
  clusterExport(cl, c("ecosystem"))
  
  # This took about 10 minutes on my computer
  FireHist[[i]] <- parLapply(cl, assess_year, function(x) firehistory(ecosystem, x)) 
  FireHist[[i]] <- bind_rows(FireHist[[i]])
  
}

FireHist <- FireHist %>% 
  setNames(list.files("../Spatialdata_forAniko/Spatialdata2_forAniko/", ".shp$") %>% 
             word(1, 1, "\\.") %>% word(2,2, sep = "_")) %>% 
  bind_rows(.id = "ecosystem") %>%  
  pivot_wider(names_from = "ecosystem", values_from = "ObsAreaKm2", names_prefix = "ObsAreaKm2_", values_fill = 0)


## save as .csv to use with Shiny app
write_csv(FireHist, "./Input_data.csv")

# **** Input data MUST have columns "LastFire" and "Assessment_Year" as output by this code. *****
# **** Ecosystem area columns MUST contain "ObsArea" (can be prefix or suffix, doesn't matter) ****
# **** Separate "ObsArea" prefix or suffix from other identifiers using _ symbol ****


### Metric calculations #####

tslf <- c(MIN = 6, MEAN = 7, MAX = 15) # Must be named

dat <- expected_fire(FireHist, tslf) # obs and expected percentages

shf <- shortfalls(dat, tslf) # shortfalls

# detailed table (throws warning if only one observed column was input)
outTable <- shf %>% select(group, gi, interval, contains("Shortfall")) %>%
  separate(group, into = c("Year", "TSLF")) %>% 
  rename_with(fix_names, contains("Shortfall"))
# summary shortfall metric table
summaryTable <- outTable %>%
  group_by(Year, TSLF) %>% summarise(across(where(is.double), sum))

# Plot frequency curves
freq_curves(dat, yr = 2020)

# Plot area graph 
area_graph(shf)

# Ribbon plot (currently handles up to 5 observed columns)
ribbon_plot(shf)

