library(data.table)
library(magrittr)
library(stringr) 
library(sf)
library(dplyr)

# library(nngeo)
library(tidytransit)
# library(ggplot2)
library(sp)
library(leaflet)
# library(DBI)
library(leafem)
# library(lubridate)
library(RANN)
library(rgeos)

# notes -------------------------------------------------------------------
#GTFS 0 is NB
#VMH 0 is NB

# inputs
#takes lane type, direction
lane_type <- c("single bi-directional","separated uni-directional","none")
direction <- "NB"

# functions ---------------------------------------------------------------
seq_along_by <- function(x, by=1L, from = 1L) (seq_along(x) - 1L) * by + from

snap_points_to_line <- function(points,line){
  
  points_align <- st_nearest_points(points,line)%>%
    st_cast("POINT")
  
  points_new_geometry <- points_align[c(seq(2, length(points_align),by = 2))]
  
  points_align_end <- points %>%
    st_set_geometry(points_new_geometry)
  
}


# read old GTFS -----------------------------------------------------------
#there were some strange errors when reading this in so we are going to do it manually
old_gtfs <- lapply(paste0("data//GTFS Archive//GTFS 1806//"
                          , dir(path = "data//GTFS Archive//GTFS 1806//"
                                )
                          )
                   , fread
                   )

#set the names
names(old_gtfs) <- dir(path = "data//GTFS//1806//agency") %>%
  str_remove(".txt")

#copy so we can sf it
old_gtfs_sf <- old_gtfs

#sf shapes
old_gtfs_sf$shapes %<>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat")
           ,crs = 4326
  ) %>%
  group_by(shape_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

#get rid of trips with no shapes
old_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))

# old GTFS convenience objects --------------------------------------------
old_gtfs_sf$stops %<>%
  st_as_sf(coords = c("stop_lon","stop_lat")
           ,crs = 4326) 

old_route_trips_and_shapes_sf <- old_gtfs_sf$shapes %>% 
  right_join(old_gtfs_sf$trips)%>%
  right_join(old_gtfs_sf$routes)

#create a stops df for ease of use
old_stops_sf <- Filter(function(x)!all(is.na(x)),old_gtfs_sf$stops) %>% #removes columns that are all blank
  select(-location_type)

#adjust stop times so we can join later
old_gtfs_sf$stop_times %<>% mutate(stop_id = as.character(stop_id)) 



# read in current GTFS ----------------------------------------------------
current_gtfs <- read_gtfs("data//GTFS Archive//GTFS 2006.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>%
  set_hms_times() %>%
  set_servicepattern()

#remove trips with no shapes
current_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))

#import VMH
VMH_Raw <- data.table::fread("data//processed//VMH_Raw.csv")

VMH_Raw[,min(Transit_Day)]
VMH_Raw[,max(Transit_Day)]

VMH_direction_sf <- VMH_Raw[Inbound_Outbound == fifelse(direction == "SB"
                                                        , 1
                                                        , 0
                                                        )
                            ] %>%
  st_as_sf(
    coords = c("Longitude","Latitude")
    ,crs = 4326
  )


#bastard row names
VMH_direction_sf$rn <- seq_along(1:nrow(VMH_direction_sf))


# create convenience objects ----------------------------------------------
# merge trips and shapes
current_trips_and_shapes_sf <- current_gtfs_sf$shapes %>% 
  right_join(current_gtfs_sf$trips)

# join routes as well
current_route_trips_and_shapes_sf <- current_trips_and_shapes_sf %>%
  right_join(current_gtfs_sf$routes)

#stops df for ease of use
current_stops_sf <- Filter(function(x)!all(is.na(x)),current_gtfs_sf$stops) 

#pairs are Broad Ripple to 42nd, 38th to Statehouse, and New Jersey to University
endpoint_stop_names <- paste0(c("Broad.*","42.*","38.*","State.*","New.*","Uni.*"),direction)

segment_end_points_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names,collapse = "|"),stop_name)) %>%
  mutate(
    lane_type = case_when(
      stop_name %like% paste(endpoint_stop_names[1:2],collapse = "|") ~ "single bi-directional"
      ,stop_name %like% paste(endpoint_stop_names[3:4],collapse = "|") ~ "separated uni-directional"
      ,TRUE ~ "none"
    )#end case_when
  )#end mutate



#get 90 and direction shape
route_shape_sf <- current_route_trips_and_shapes_sf %>%
  filter(route_short_name == 90,
         direction_id == ifelse(direction == "SB",1,0)) %>% #this gets the right set of shapes
  filter(st_length(geometry) == max(st_length(geometry))) %>%
  slice(1)

#get route points of interest
route_shape_point_sf <- current_gtfs$shapes %>% 
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),
           crs = 4326) %>% 
  filter(shape_id == route_shape_sf$shape_id)

#match points to endpoints
#this nn2 works identically
route_point_indices <- route_shape_point_sf %>%
  st_coordinates %>%
  nn2(query = segment_end_points_sf %>% st_coordinates()
      ,k = 2) %>%
  .$nn.idx %>%
  apply(1, min)

#input for next
n <- nrow(segment_end_points_sf)

#split into segments
segment_shape_list <- lapply(
  X = seq_along_by(1:(n/2),2), function(x){
    
    route_segment_start <- route_point_indices[x]
    route_segment_end <- route_point_indices[x+1]
    
    linestring <- route_shape_point_sf[route_segment_start:route_segment_end,] %>%
      group_by(shape_id) %>%
      arrange(shape_pt_sequence) %>%
      summarize(do_union = F) %>%
      st_cast("LINESTRING")
    
    
    return(linestring)
  }
)

#add labels
shape_segments <- lapply(segment_shape_list,st_sf) %>%
  do.call(rbind,.) %>% 
  mutate(lane_type = segment_end_points_sf$lane_type[c(FALSE,TRUE)])#the false true repeats and grabs the lane types

# match VMH to SB_shape_segments ------------------------------------------

#get coordinate matrices
VMH_direction_sf_coords <- do.call(
  rbind
  ,st_geometry(VMH_direction_sf)
)

closest_VMH <- lapply(
  seq_along(1:nrow(shape_segments)), FUN = function(x){
    nn2(
      VMH_direction_sf_coords
      ,shape_segments[x,] %>% st_segmentize(5) %>% st_coordinates() %>% .[,1:2] #segment coordinate matrix
      ,k = 10000
      ,searchtype = "radius"
      ,radius = 0.00001*(10/1.11)
    ) %>%
      sapply(cbind) %>% #combine
      as_tibble() %>% #convert so we can pull
      distinct(nn.idx) %>%
      pull() %>%
      sort() %>%
      VMH_direction_sf[.,]
  }
)  

names(closest_VMH) <- shape_segments$lane_type

closest_VMH %>% lapply(sample_n,10000) %>%
  lapply(function(x){
    x %>%
      leaflet() %>%
      addFeatures() %>%
      addTiles()
  })

# get dist traveled on shape --------------------------------------------
# may or may not work

closest_VMH <- lapply(seq_along(1:length(closest_VMH)),function(i){
  
  VMH_sp <- closest_VMH[[i]] %>%
    as_Spatial() %>% 
    spTransform(CRS(st_crs(7328)$proj4string))
  
  segment_sp <- as_Spatial(shape_segments[i,] %>% st_transform(7328))
  
  segment_length <- gLength(segment_sp)
  
  shape_dist_traveled <- gProject(segment_sp,VMH_sp)
  
  closest_VMH[[i]]$dist_traveled <- shape_dist_traveled
  
  return(closest_VMH[[i]])
}) %>% `names<-`(shape_segments$lane_type)

# get DT with speeds --------------------------------------------

VMH_speed_DT <- lapply(seq_along(1:length(closest_VMH)), function(i){

    #pull in
  DT <- data.table(closest_VMH[[i]])
  #get seglen
  seglen <- gLength(as_Spatial(shape_segments[i,] %>% st_transform(7328)))
  #
  #DT <- 1
  grp <- quote(list(Trip,Transit_Day,Vehicle_ID))
  
  #clean DT
  DT <- DT[
    #order by trip, then veh_ID, then transit_Day, then Time
    order(Trip,Vehicle_ID,Transit_Day,Time)
    
    ][
      #remove non BYD
      Vehicle_ID %in% c(1899,1970:1999)
      
      ][
        #set dist_to_next, resetting at each new group
        ,`:=` (dist_to_next = c(diff(dist_traveled),0))
        ,grp
        
        ][
          #set mph for each point to point segment and get cumulative distance traveled, by group
          ,`:=`(mph = c(NA_real_
                        , diff(dist_traveled)/diff(as.integer(Time))/1.466667
                        )
                , cum_dist = cumsum(abs(dist_to_next)))
          , grp
          
          ][
            #flag negative
            , neg := dist_to_next <= 0
            
            ][
              #create rleid
              , rl := rleid(neg),by = grp
              
              ][
                #group by both and get length of each run
                , rl_len := .N
                , .(Trip,Transit_Day,Vehicle_ID,rl)
                
                ][
                  #filter rows that have more than 3 and are NEG
                  !(neg == T & rl_len >= 4)
                  ][
                    #redo calcs
                    , `:=` (dist_to_next = c(diff(dist_traveled),0))
                    , grp
                    ][
                      ,`:=`(mph = c(NA_real_
                                    , diff(dist_traveled)/diff(as.integer(Time))/1.466667
                                    )
                            ,cum_dist = cumsum(abs(dist_to_next)))
                      ,grp
                      ][
                        #add cum_dist_flag
                        ,cum_dist_ok := cum_dist <= seglen |
                          cum_dist-seglen < 20
                        ][
                          #filter
                          cum_dist_ok == T
                          ][
                            #do these calcs one more time
                            ,`:=` (dist_to_next = c(diff(dist_traveled),0))
                            ,grp
                            ][
                              ,`:=`(mph = c(NA_real_
                                            , diff(dist_traveled)/diff(as.integer(Time))/1.466667)
                                    ,cum_dist = cumsum(abs(dist_to_next)))
                              ,grp
                              ]
  DT <- DT[
    #convert to feet, get min and max time
    ,.(miles_traveled = max(cum_dist)/5280 #convert to feet
       ,start_time = min(Time) 
       ,end_time = max(Time)
       ,pings = .N)
    ,grp
    
    ][
      #convert to hours
      ,time := as.numeric((end_time - start_time)/60/60) #convert to hours
      ][
        #convert to speed
        , speed := miles_traveled/time #get speed
        ][
          #remove records with very low ping #
          pings >= 10
          ]
  
  
  return(DT)
}) %>% `names<-`(shape_segments$lane_type) %>%
  rbindlist(idcol = T)

VMH_speed_DT[,direction := direction]
