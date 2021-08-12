library(data.table)
library(leaflet)
library(openxlsx)
library(sf)
library(sp)

lane_type <- c("single bi-directional","separated uni-directional","none")
direction <- "NB"

shapes  = read.table("C://Users//theodore.galanthay//Downloads//GFI_RevEng//Ted's Bad Code//Vehicle_Speeds//data//GTFS Archive//GTFS 2006//shapes.txt", sep=",",header=TRUE)

colnames(shapes) <- c("shape_id", "Latitude", "Longitude", "shape_pt_sequence")

shapes_red_line <- shapes[shapes$shape_id %like% "900",]

shapes_red_line %>% leaflet::leaflet() %>% leaflet::addCircles() %>%
  leaflet::addTiles()

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
    ,crs = 7328
  )

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
