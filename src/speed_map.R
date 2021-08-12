library(data.table)
library(leaflet)
library(openxlsx)

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
    ,crs = 4326
  ) %>%  spTransform(CRS(st_crs(7328)$proj4string))
