
##################
# code lifted from this thread:
# https://stackoverflow.com/questions/44631044/efficient-extraction-of-all-sub-polygons-generated-by-self-intersecting-features

library(stringr)
get_difference_region <- function(cur, x, input_polys, keep_columns=c("id")){
  x <- x[!x==cur] # remove self 
  len <- length(x)
  input_poly_sfc <- st_geometry(input_polys)
  input_poly_attr <- as.data.frame(as.data.frame(input_polys)[, keep_columns])
  
  # base poly
  res_poly <- input_poly_sfc[[cur]]
  res_attr <- input_poly_attr[cur, ]
  
  # substract the intersection parts from base poly
  if(len > 0){
    for(i in 1:len){
      res_poly <- st_difference(res_poly, input_poly_sfc[[x[i]]])
    }
  }
  return(cbind(res_attr, data.frame(geom=st_as_text(res_poly))))
}

get_intersection_region <- function(cur, x, input_polys, keep_columns=c("id"), sep="&"){
  x <- x[!x<=cur] # remove self and remove duplicated obj 
  len <- length(x)
  input_poly_sfc <- st_geometry(input_polys)
  input_poly_attr <- as.data.frame(as.data.frame(input_polys)[, keep_columns])
  
  res_df <- data.frame()
  if(len > 0){
    for(i in 1:len){
      res_poly <- st_intersection(input_poly_sfc[[cur]], input_poly_sfc[[x[i]]])
      res_attr <- list()
      for(j in 1:length(keep_columns)){
        pred_attr <- str_split(input_poly_attr[cur, j], sep, simplify = TRUE)
        next_attr <- str_split(input_poly_attr[x[i], j], sep, simplify = TRUE)
        res_attr[[j]] <- paste(sort(unique(c(pred_attr, next_attr))), collapse=sep)
      }
      res_attr <- as.data.frame(res_attr)
      colnames(res_attr) <- keep_columns
      res_df <- rbind(res_df, cbind(res_attr, data.frame(geom=st_as_text(res_poly))))
    }
  }
  return(res_df)
}

sp_identity <- function(sf, keep_cols){
  # init
  close_df <- data.frame()
  open_sf <- sf
  
  # main loop
  while(!is.null(open_sf)) {
    flag <- st_intersects(open_sf, open_sf)
    for(i in 1:length(flag)) {
      message(paste("get diffs", i))
      cur_df[[i]] <- get_difference_region(i, flag[[i]], open_sf, keep_column = keep_cols)
      #close_df <- rbind(close_df, cur_df)
    }
    close_df <- rbind(close_df, reduce(cur_df, rbind))
    
    cur_open <- data.frame()
    for(i in 1:length(flag)) {
      message(paste("get intersections", i))
      cur_df[[i]] <- get_intersection_region(i, flag[[i]], open_sf, keep_column = keep_cols)
      #cur_open <- rbind(cur_open, cur_df)
    }
    cur_open <- reduce(cur_df, rbind)
    
    message(paste("Currently", nrow(cur_open), "open features"))
    if(nrow(cur_open) != 0) {
      cur_open <- cur_open[row.names(cur_open %>% select(-geom) %>% distinct()),]
      open_sf <- st_as_sf(cur_open, wkt="geom") %>% 
        st_make_valid() %>%                  # Validate
        st_collection_extract("POLYGON") %>%        # remove points and lines
        group_by(FireAge) %>%                       # regroup by fireAge
        summarize(geometry = st_combine(geom)) %>%
        st_make_valid()
    }else{
      open_sf <- NULL
    }
  }
  
  close_sf <- st_as_sf(close_df, wkt="geom") %>% filter(!st_is_empty(.))
  return(close_sf)
  #plot(close_sf[1])
}



