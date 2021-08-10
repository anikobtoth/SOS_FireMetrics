## spatial processing 

library(rgdal)
library(raster)
library(sf)
library(tidyverse)


ecosystem <- st_read("../Spatialdata_forAniko/TECDist/TEC5", "NSWTEC5_ShaleSandSydney") %>% 
  st_transform(st_crs(AEA_Aus))
fire <- st_read("../Spatialdata_forAniko/FireNPWSHistory/firenpwsfirehistory", "Fire_NPWS_FireHistory") %>%
  st_transform(st_crs(AEA_Aus))
assess_year <- 2020

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y) %>% st_make_valid()))

firehistory <- function(ecosystem, fire, assess_year){
  # Australian albers EA projection
  AEA_Aus <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  ecosystem <- st_transform(ecosystem, st_crs(AEA_Aus)) %>% # transform
    st_make_valid() %>% st_union()                          # validate and lump to one multipolygon
  
  fire <- st_transform(fire, st_crs(AEA_Aus)) %>% # transform
    st_make_valid() %>%                           # Validate
    st_crop(st_bbox(ecosystem)) %>%               # crop to ecosystem extent
    mutate(FireAge = substr(Label, 0, 4)) %>%     # Add fireAge
    group_by(FireAge) %>%                         # union polygons from same fireAge
    summarize(geometry = st_union(geometry)) %>%
    sp_identity(keep_cols = "FireAge")
  
  curr_fire <- fire %>% filter(FireAge < assess_year)
  
  IDFireHist <- st_intersection(curr_fire, ecosystem) %>% st_intersection() #%>% st_union(by_feature = TRUE)
  IDFireHist <- IDFireHist %>%
    mutate(Area = st_area(IDFireHist)) %>% 
    filter(as.numeric(Area) > 0) %>%
    mutate(fgeom = geometry %>% as.character %>% as.factor %>% as.numeric)
  
  #IDFireHist$Area <- st_area(IDFireHist)
  #IDFireHist <- IDFireHist %>% filter(as.numeric(Area) > 0)
  #IDFireHist$fgeom <- IDFireHist$geometry %>% as.character() %>% as.factor() %>% as.numeric()
  
  test <- IDFireHist %>% group_by(fgeom, Area) %>% summarise(mostRecentFire = max(FireAge), n = n())
  test2 <- test %>% data.frame() %>% group_by(mostRecentFire) %>% summarise(Areakm2 = sum(Area)/1000000)
  
  #unburned <- st_difference(TEC, curr_fire)
}





