library(rgdal)
library(sf)
library(tidyverse)
library(lwgeom)
library(cowplot)
library(viridis)

###### Spatial Preprocessing ############
pair_diff <- function(x, y){
  print(y$FireAge)
  rbind(y, x) %>% st_difference(quiet = TRUE)
}

firehistory <- function(ecosystem, fire, assess_year){
  # Australian albers EA projection
  AEA_Aus <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  ecosystem <- st_transform(ecosystem, st_crs(AEA_Aus)) %>% # transform
    st_make_valid() %>% st_union()                          # validate and lump to one multipolygon
  
  fire <- st_transform(fire, st_crs(AEA_Aus)) %>%          # transform
    mutate(FireAge = as.numeric(substr(Label, 0, 4))) %>%  # Add FireAge as numeric
    filter(FireAge < assess_year) %>%                      # Filter to before assessment year
    st_make_valid() %>%                                    # Validate
    st_crop(st_bbox(ecosystem)) %>%                        # crop to ecosystem extent (to reduce processing)
    group_by(FireAge) %>%                                  # union polygons from same FireAge
    summarize(geometry = st_union(geometry)) %>%
    st_snap_to_grid(1) %>% st_make_valid()                  # snap to grid and validate
  
  fire_diff <- fire %>% split(.$FireAge) %>% 
    reduce(pair_diff)
  
  areas <- st_intersection(fire_diff, ecosystem) 
  
  
  unburned <- as.numeric((st_area(ecosystem) - (st_union(areas) %>% st_area()))/1000000)
  
  out <- areas %>% mutate(ObsAreaKm2 = as.numeric(st_area(areas)/1000000)) %>% 
    tibble() %>% dplyr::select(LastFire = FireAge, ObsAreaKm2) %>% 
    rbind(data.frame(LastFire = 0, ObsAreaKm2 = unburned)) %>% 
    mutate(Assessment_Year = assess_year)
  
  return(out)
}


##### Fire metric calculations #############

TSLF_intervals <- function(dat, tslf){
  GI <- rep(1:10, 2^(seq(0,9)))[1:nrow(dat)]
  intervals <- rep(paste(c("", which(diff(GI)>0)+1), c(which(diff(GI)>0),""), sep = "-") %>% str_replace("^-", "") %>% str_replace("-$", "+"), times = table(GI))
  
  dat <- dat %>% mutate(gi           = GI,
                        interval     = intervals)
  
  
  geointervals <- dat %>% group_by(gi, interval) %>% 
    summarise(across(contains("Area"), sum)) %>%
    mutate(across(contains("Obs"), list(d = ~ExpectedArea-.x))) %>% 
    mutate(across(ends_with("_d"), list(Shortfall = ~ifelse(.x < 0, 0, .x)))) %>%
    mutate(group = paste(dat$yrofas[1], tslf, sep = "_"))
  
  return(geointervals)
}

fix_names <- function(chr){
  strsplit(chr, split = "_") %>% 
    reduce(rbind) %>% data.frame() %>% 
    select_if(~n_distinct(.x)>1) %>% 
    apply(1, paste, collapse = "_") %>% 
    setNames(chr)
}

#### Data processing functions ########

expected_fire <- function(input_data, tslf){
 oldestyr <- 1950
 raw <- input_data %>% group_by(Assessment_Year) %>% group_split() %>%
    map(mutate, across(contains("Area"), list(Pct = ~100*.x/sum(.x))))

 calc1 <- list()
  for(i in seq_along(raw)){
    yoa <- raw[[i]]$Assessment_Year[1]
    raw[[i]] <- raw[[i]] %>% mutate(LastFire  = ifelse(LastFire == 0, oldestyr-1, LastFire))
    calc1[[i]] <- data.frame(yrofas = yoa, TSLF = 1:(yoa-oldestyr+1)) %>%
      mutate(LastFire = yrofas-TSLF,
             ExpectedPctMIN = 100* ((1-(1/tslf["MIN"]))^(TSLF-1)) * (1/tslf["MIN"]),
             ExpectedPctMEAN = 100* ((1-(1/tslf["MEAN"]))^(TSLF-1)) * (1/tslf["MEAN"]),
             ExpectedPctMAX = 100* ((1-(1/tslf["MAX"]))^(TSLF-1)) * (1/tslf["MAX"])) %>%
      full_join(raw[[i]], by = "LastFire") %>%
      mutate_if(is.numeric,coalesce,0) %>% dplyr::select(-Assessment_Year)
  }
 return(calc1)
}

shortfalls <- function(data, firereturn){
  grid <- expand_grid(1:length(data), 1:length(firereturn)) %>% t() %>% data.frame()
  
  purrr::map(grid, ~TSLF_intervals(data[[.x[1]]] %>% select(yrofas, LastFire, TSLF, ends_with("_Pct"), ExpectedArea = ends_with(names(firereturn[.x[2]]))), 
                                   firereturn[.x[2]])) %>% 
    setNames(map_chr(grid, ~paste(names(data)[.x[1]], firereturn[.x[2]], sep = "_tslf"))) %>%
    bind_rows()
} 
 

#### Plotting functions ############

freq_curves <- function(dat, yr = 2020){
  data <- dat %>% bind_rows() %>% filter(yrofas == yr) %>% 
    pivot_longer(cols = contains("Pct"), names_to = "variable")
  
  colors <- c("#073b4c", "#06d6a0", "#ef476f", "#ffd166", "#118ab2")
  colrs <- c("orange3", "orangered3", "red3", colors[1:(n_distinct(data$variable)-3)])
  
  ggplot(data, aes(x = TSLF, y = value, col = variable)) + geom_line(lwd = 1.3) + 
    labs(y = "Percent of ecosystem extent") +
    ggtitle(paste("Frequency curves for", yr)) + 
    scale_color_manual(values = colrs) +
    theme(plot.title = element_text(size = 16, face = "bold"), legend.position = c(.2, 0.85))
}

area_graph <- function(dat){
  outC <- dat %>% select(group, gi, interval, contains("Area"), -contains("_d")) %>% 
    separate(group, into = c("Year", "TSLF")) %>% 
    pivot_longer(contains("Area")) %>% 
    mutate(x = ifelse(name == "ExpectedArea", as.numeric(TSLF), as.numeric(Year)), 
           interval = recode(interval, `32+` = "32-63")) %>%
    distinct(Year, gi, interval, name, value, .keep_all = TRUE) %>% 
    filter(!(name == "ExpectedArea" & Year != 2020))
  
  p1 <- ggplot(outC %>% filter(name != "ExpectedArea"), aes(x = x, y = value, fill = as.factor(gi), group = TSLF)) + 
    geom_col(width = 3) + 
    facet_grid(~name, scales = "free") +
    scale_fill_viridis(discrete = T, name = "", labels = unique(outC$interval), option = "B") +
    labs(x = "Observed year", y = "Percent burned (%)") + 
    theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "cm")) 
  
  p2 <- ggplot(outC %>% filter(name == "ExpectedArea"), aes(x = as.factor(x), y = value, fill = as.factor(gi), group = TSLF)) +
    geom_col(width = 0.6) +
    facet_grid(~name, scales = "free") +
    scale_fill_viridis(discrete = T, name = "Interval", labels = unique(outC$interval), option = "B") +
    labs(x = "TSLF Benchmark", y = "") + 
    theme(plot.margin = unit(c(.2,.2,.2, 0), "cm"), axis.text.y = element_blank())
  
  title <- ggdraw() + 
    draw_label(
      "Observed and expected percent of areas burned",
      size = 16,
      fontface = 'bold',
      x = 0,
      hjust = -.1
    )
  plot_grid(title, 
            plot_grid(p1 + theme(legend.position = "none"), p2, nrow = 1, rel_widths = c(3,2)),
            ncol = 1,
            rel_heights = c(0.1, 1)) 
  
}

ribbon_plot <- function(dat){
  colors <- c("#073b4c", "#06d6a0", "#ef476f", "#ffd166", "#118ab2")
  outD <- dat %>% select(group, gi, interval, contains("Shortfall")) %>% 
    group_by(group) %>% summarise(across(contains("Shortfall"), sum)) %>%
    separate(group, into = c("Year", "TSLF")) %>% 
    pivot_longer(contains("Shortfall")) %>% 
    mutate(Year = as.numeric(Year), TSLF = as.numeric(TSLF)) %>%
    mutate(TSLF = ifelse(TSLF == min(TSLF), "Min", ifelse(TSLF == max(TSLF), "Max", "Mean"))) %>%
    pivot_wider(names_from = TSLF, values_from = value)
  if(n_distinct(outD$name)>1){
    outD <- outD %>% 
      mutate(name = recode(name, !!!fix_names(unique(outD$name))))
  }
  colrs <- colors[1:n_distinct(outD$name)]
  ggplot(outD, aes(x = Year, fill = name)) + 
    geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.5, lwd = 0) +
    geom_point(aes(y = Mean, col = name), pch = 16) + 
    geom_line(aes(y = Mean, col = name), lwd = 1) +
    labs(y = "Shortfall metric", x = "Year of condition assessment", fill = "", col = "") + 
    scale_fill_manual(values = colrs) +
    scale_color_manual(values = colrs) +
    ggtitle("Ecosystem total shortfall") + 
    theme(plot.title = element_text(size = 16, face = "bold"))
  
}
