# 7/7/20

# This script assesses utility of datasets for vehicle speeds, including both
# run times and dwell times.

### thoughts

# - We may want to add comparison from schedule...
# - Need to research ACTUALLY what drive time is...

# - snap to street grid?

# - consider changing "MPH" to something more descriptive, b/c it doesn't take 
#   dwell time into account

# - DimRouteSegment has a segment geography... 

# - We should consider whether layover time is to be included, or just drive time.

# - I think I have to add direction, so as to not confuse overlapping layers...

### revision history ###

# 7/10/20 - explored methods to import DimRouteSegment geography field.

# 7/13/20 - figured out how to import geography field.
#         - coalesced testing script into full-fledged script..

### future work ###

# 7/13/20 - may need to deal with overlapping fields.

# 7/14/20 - need to pull out deadhead routes...(probably from segment adherence)
#          (maybe from DimPattern?) (DONE!)
#         - Need to find better way to display values
#         - Still need to deal with overlapping fields..
#         - Still need to disaggregate by direction, time period..

# libraries

library(tidyverse)
library(data.table)

# db connection

con2 <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "AVAILDWHP01VW", 
                       Database = "DW_IndyGo", 
                       Port = 1433)

# get stops

DimStop <- tbl(con2, "DimStop") %>% collect()

# get time

DimTime <- tbl(con2, "DimTime") %>% collect()

# get dates

sample_dates <- tbl(con2, "DimDate") %>%
  filter(CalendarDate == "2019-10-26") %>%
  select(DateKey, CalendarDate) %>%
  collect()

# get FactSegmentActual

FactSegmentActual_sample <- tbl(con2, "FactSegmentActual") %>%
  filter(DateKey %in% !!sample_dates$DateKey) %>%
  collect()

# get FactSegmentAdherence

FactSegmentAdherence_sample <- tbl(con2, "FactSegmentAdherence") %>%
  filter(DateKey %in% !!sample_dates$DateKey) %>%
  collect()

# get patterns to take out deadheading

DimPattern <- tbl(con2, "DimPattern") %>% collect()

# clean out deadhead

FactSegmentAdherence_sample <- FactSegmentAdherence_sample %>%
  left_join(DimPattern, by = "PatternKey") %>%
  filter(DeadheadInd != 1)
# I think we use FactSegmentAdherence, and I think
# we divide DistanceFeetSchedule by 5280 to get miles, and then
# divide that by DriveTimeActualSecs / 3600,
# to get miles per hour

FactSegmentAdherence_sample$MPH_Schedule <- (FactSegmentAdherence_sample$DistanceFeetSchedule/5280) /
  (FactSegmentAdherence_sample$DriveTimeScheduleSecs / 3600)

FactSegmentAdherence_sample$MPH_Actual <- (FactSegmentAdherence_sample$DistanceFeetSchedule/5280) /
  (FactSegmentAdherence_sample$DriveTimeActualSecs / 3600)

FactSegmentAdherence_sample$MPH_Variance <- FactSegmentAdherence_sample$MPH_Schedule - FactSegmentAdherence_sample$MPH_Actual

FactSegmentAdherence_sample$MPH_Variance_Percent <- FactSegmentAdherence_sample$MPH_Actual / FactSegmentAdherence_sample$MPH_Schedule

# clean up data some

FactSegmentAdherence_sample_clean <- FactSegmentAdherence_sample %>%
  filter(MPH_Variance_Percent >= 0, is.finite(MPH_Variance_Percent)) %>%
  left_join(select(DimTime, TimeKey, Time24Desc), by = c("ArriveTimeKey" = "TimeKey")) %>%
  mutate(Peak = ifelse(Time24Desc >= "06:00" & Time24Desc < "09:00", "AM", 
                       ifelse(Time24Desc >= "15:00" & Time24Desc < "18:00", "PM", "Off-Peak"))) 


# now many arrive/stop pairs?

FactSegmentAdherence_sample_clean %>%
  group_by(ArriveStopKey, DepartStopKey) %>%
  summarise(n = n()) # a lot.

FactSegmentAdherence_sample_clean %>%
  group_by(RouteSegmentKey) %>%
  summarise(n = n()) # ah ha! So this is unique. Neat!

# review directions, time periods..

FactSegmentAdherence_sample_clean %>%
  group_by(PatternDirectionDesc) %>%
  summarise(n = n())

FactSegmentAdherence_sample_clean %>%
  group_by(PatternDirectionDesc) %>%
  summarise(n = n())

FactSegmentAdherence_sample_clean %>%
  group_by(Peak) %>%
  summarise(n = n())

# get median values (also consider adding values for AM and PM peaks)

# Median_Segment_MPH <- FactSegmentAdherence_sample_clean %>%
#   group_by(RouteSegmentKey, PatternDirectionDesc, Peak) %>%
#   summarise(Median_MPH = median(MPH_Actual)) 

# let's just do segment by peak period, for now (leave out direction)

Median_Segment_MPH <- FactSegmentAdherence_sample_clean %>% # consider removing off-peak...
  group_by(RouteSegmentKey, PatternDirectionDesc, Peak) %>%
  summarise(Median_MPH = median(MPH_Actual)) # step 1

FactSegmentAdherence_sample_clean %>% setDT()

####################### THIS IS A PROBLEM WITH THIS DATASET
FactSegmentAdherence_sample_clean[order(MPH_Actual)]


# will also need to explore direction with Route Segments...
# also consider having AM/PM peaks as layers...

### now get route segment data ###

library(leaflet)
library(tidyverse)
library(sf)

# db conncetion #

con3 <- RODBC::odbcDriverConnect('driver={SQL Server Native Client 11.0};server=AVAILDWHP01VW;database=DW_IndyGo;Trusted_Connection=yes;')

# collect

DimRouteSegment <- RODBC::sqlQuery(con3, 'DECLARE @Geom varchar(max) ; SELECT RouteSegmentKey, Geom.STAsText() as ShapeWKT FROM DW_IndyGo.dbo.DimRouteSegment ;')

DimRS_length <- RODBC::sqlQuery(con3, 'SELECT MAX(CHAR_LENGTH(Geom.STAsText()) FROM DW_IndyGo.dbo.DimRouteSegment ;')

# alternative collect

DimRouteSegment_alt <- RODBC::sqlQuery(con3, # nah
                                       "select RouteSegmentKey, Geom.STY as Lat, Geom.STX as Lon FROM DW_IndyGo.dbo.DimRouteSegment")

DimRouteSegment_alt_2 <- RODBC::sqlQuery(con3, 
                                         "select RouteSegmentKey, CoordinateList.STY as Lat, CoordinateList.STX as Lon FROM DW_IndyGo.dbo.DimRouteSegment")


DimRouteSegment_alt_3 <- RODBC::sqlQuery(con3, 'SELECT RouteSegmentKey, CAST(Geom.STAsText() AS ntext) as ShapeWKT FROM DW_IndyGo.dbo.DimRouteSegment ;') # looks good!

DimRouteSegment_alt_3_5 <- data.frame(DimRouteSegment_alt_3 ) # looks good!

# review to see if truncated

DimRouteSegment_alt_3_5[DimRouteSegment_alt_3_5$RouteSegmentKey == 5294, 2] # this looks good!!!

# not truncated, by the looks of it! now test all the previous methods
# to see what sticks...



# try alternative connection/driver

con2 <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "AVAILDWHP01VW", 
                       Database = "DW_IndyGo", Port = 1433)

DimRouteSegment_con2 <- DBI::dbGetQuery(con2, 'SELECT RouteSegmentKey, Geom.STAsText() as ShapeWKT FROM DW_IndyGo.dbo.DimRouteSegment ;')

# now join summary data

Median_Segment_MPH_joined <- Median_Segment_MPH %>%
  left_join(DimRouteSegment_alt_3_5, by = "RouteSegmentKey") # steop 2

# clean data (looks like one segment is coded as as point...)

Median_Segment_MPH_joined <- filter(Median_Segment_MPH_joined, RouteSegmentKey != 11311) %>%
  ungroup() %>%
  mutate(id = row_number()) # stop 3 (maybe unnecessary)

Median_Segment_MPH_joined$ShapeWKT <- as.character(Median_Segment_MPH_joined$ShapeWKT) # step 4

# Looks like still have some very high MPH...

View(Median_Segment_MPH_joined)
ggplot(Median_Segment_MPH_joined, aes(Median_MPH))+ geom_density()

quantile(Median_Segment_MPH_joined$Median_MPH, .95)

# maybe its worth to just get coordinates and box out unnecessary segments?

# review to see if truncated

DimRouteSegment_alt_3_5[DimRouteSegment_alt_3_5$RouteSegmentKey == 5294, 2] # still truncated..

max(nchar(Median_Segment_MPH_joined$ShapeWKT)) # ok! not truncated!

# now convert data

# ogr_test <- readOGR(Median_Segment_MPH_joined$ShapeWKT)
# 
# library(sf)
# 
# sf_test <- st_as_sfc(Median_Segment_MPH_joined )
# 
# sf_test <- st_as_sf(Median_Segment_MPH_joined, wkt = 3, crs = "EPSG3857")
# 
# write.csv(Median_Segment_MPH_joined, "Median_Segment_MPH_joined.csv", row.names = FALSE)
# 
# st_test <- st_read(dsn = "Median_Segment_MPH_joined.csv", geometry_column = 3)
# 
# sf_test <- st_as_sf(dsn = "Median_Segment_MPH_joined.csv", wkt = 3, crs = "EPSG3857")
# 
# sfc_test <- st_as_sfc(Median_Segment_MPH_joined, crs = NA_integer_, GeoJSON = FALSE) 
# 
# st_test <- st_linestring(Median_Segment_MPH_joined[,3])
# 
# str = st_as_text(x)
# x # this kind of works....
# st_as_sfc(str)

library(rgeos)

lines.sl <- SpatialLinesDataFrame(readWKT(Median_Segment_MPH_joined[1,5]),  # works... but need to add acutal MPH data***!!!
                                  data=data.frame(RouteSegmentKey=Median_Segment_MPH_joined[1,1],
                                                  Peak=Median_Segment_MPH_joined[1,3],
                                                  Median_MPH=Median_Segment_MPH_joined[1,4],
                                                  id=Median_Segment_MPH_joined[1,6],
                                                  PatternDirectionDesc=Median_Segment_MPH_joined[1,2])) # stop 5

for (n in 2:length(Median_Segment_MPH_joined$id)) {
  lines.sl <- rbind(lines.sl, 
                    SpatialLinesDataFrame(readWKT(Median_Segment_MPH_joined$ShapeWKT[n]), 
                                          data=data.frame(RouteSegmentKey=Median_Segment_MPH_joined$RouteSegmentKey[n],
                                                          Peak=Median_Segment_MPH_joined$Peak[n],
                                                          Median_MPH=Median_Segment_MPH_joined$Median_MPH[n],
                                                          id=Median_Segment_MPH_joined$id[n],
                                                          PatternDirectionDesc=Median_Segment_MPH_joined$PatternDirectionDesc[n]))) # step 6
} # well... this worked? got following error:

# Error in slot(sl, "lines") : 
#   no slot of name "lines" for this object of class "SpatialPoints"

# now convert this to sf to plot in leaflet..

lines.sl_sf <- st_as_sf(lines.sl) # stop 7

ggplot(lines.sl_sf ) +
  geom_sf()

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = lines.sl_sf$Median_MPH
)

# create three groups...
lines.sl_sf
map <- leaflet(data = lines.sl_sf ) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolylines(data = lines.sl_sf,
               color = ~pal(Median_MPH))%>%
  addLegend("bottomright", pal = pal, values = ~Median_MPH,
            title = "Speed of Average Vehicle",
            labFormat = labelFormat(suffix = " MPH"),
            opacity = 1
  )

map 

# let's try removing some segments:

# st_bbox(lines.sl_sf)

lines.sl_sf_testing <- lines.sl_sf

lines.sl_sf_testing  <- st_crop(lines.sl_sf, 
                                xmin = -86.175, xmax = -86.125, 
                                ymax = 39.8, ymin = 39.75)

ggplot(lines.sl_sf_testing ) +
  geom_sf()

pal_t <- colorNumeric(
  palette = "YlOrRd",
  domain = lines.sl_sf_testing$Median_MPH
)
map_t <- leaflet(data = lines.sl_sf_testing ) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolylines(data = lines.sl_sf_testing,
               color = ~pal_t(Median_MPH))%>%
  addLegend("bottomright", pal = pal_t, values = ~Median_MPH,
            title = "Speed of Average Vehicle",
            labFormat = labelFormat(suffix = " MPH"),
            opacity = 1
  )

map_t 

# so this is better but... still kind of hard to see...
# maybe consider rounding and then making categorical?

# let's make map with layers, as derived from map_t

### the following is a grouping method but.. it is less than ideal ###

# add numeric grouping label

lines.sl_sf_testing <- lines.sl_sf_testing %>%
  mutate(groups = case_when(
    Peak == "AM" ~ 1,
    Peak == "PM" ~ 2,
    Peak == "Off-Peak" ~ 3
  ))

#function to plot a map with layer selection 
map_layers <- function() {
  
  #number of groups
  k <- n_distinct(lines.sl_sf_testing$groups)
  
  #base map
  map <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner)
  
  #loop through all groups and add a layer one at a time
  for (i in 1:k) {
    map <- map %>% 
      addPolylines(
        data = lines.sl_sf_testing %>% filter(groups == i), group = as.character(i),
        color = ~pal_t(Median_MPH)
      )
  }
  
  #create layer control + legend
  map %>% 
    addLayersControl(
      overlayGroups = c(1:k),
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(as.character(c(2:k))) %>% #hide all groups except the 1st one
    addLegend("bottomright", pal = pal_t, values = lines.sl_sf_testing$Median_MPH,
              title = "Speed of Average Vehicle",
              labFormat = labelFormat(suffix = " MPH"),
              opacity = 1)
}

#plot the map
map_layers()

### here is brute-force method ###

# get rid of bogus data

quantile(lines.sl_sf_testing$Median_MPH, .95)

lines.sl_sf_testing <- filter(lines.sl_sf_testing, Median_MPH < 40)

# pal_t <- colorNumeric(
#   palette = colorRamps::matlab.like2(5),
#   domain = lines.sl_sf_testing$Median_MPH
# )

# let's try color bins, instead (still need to work on overlapping issue...)

pal3<-colorBin(palette=colorRamps::matlab.like2(5), 
               domain=c(round(min(lines.sl_sf_testing$Median_MPH), digits = 1), 
                        round(max(lines.sl_sf_testing$Median_MPH), digits = 1)), 
               bins = 5, na.color = NULL, pretty=FALSE, alpha = TRUE)

map_t_2 <- leaflet() %>% # so these popups don't look right... try splitting first.
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolylines(data = filter(lines.sl_sf_testing, Peak == "AM"),
               color = ~pal3(Median_MPH),
               group = "AM",
               popup = paste(round(lines.sl_sf_testing$Median_MPH, digits = 1)))%>%
  addPolylines(data = filter(lines.sl_sf_testing, Peak == "PM"),
               color = ~pal3(Median_MPH),
               group = "PM",
               popup = paste(round(lines.sl_sf_testing$Median_MPH, digits = 1)))%>%
  addPolylines(data = filter(lines.sl_sf_testing, Peak == "Off-Peak"),
               color = ~pal3(Median_MPH),
               group = "Off-Peak",
               popup = paste(round(lines.sl_sf_testing$Median_MPH, digits = 1)))%>%
  addLayersControl(
    overlayGroups =c("AM", "PM", "Off-Peak"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  addLegend("bottomright", pal = pal3, 
            values = round(lines.sl_sf_testing$Median_MPH, digits = 1),
            title = "Speed of Average Vehicle",
            labFormat = labelFormat(suffix = " MPH", digits = 0),
            opacity = .9
  )

map_t_2 

# try splitting first to resolve pop-up issue

lines.sl_sf_testing_am <- filter(lines.sl_sf_testing, Peak == "AM")
lines.sl_sf_testing_pm <- filter(lines.sl_sf_testing, Peak == "PM")
lines.sl_sf_testing_offpeak <- filter(lines.sl_sf_testing, Peak == "Off-Peak")

map_t_3 <- leaflet() %>% # so these popups don't look right... try splitting first.
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolylines(data = filter(lines.sl_sf_testing_am, Peak == "AM"),
               color = ~pal3(Median_MPH),
               group = "AM",
               popup = paste(round(lines.sl_sf_testing_am$Median_MPH, digits = 1)))%>%
  addPolylines(data = filter(lines.sl_sf_testing_pm, Peak == "PM"),
               color = ~pal3(Median_MPH),
               group = "PM",
               popup = paste(round(lines.sl_sf_testing_pm$Median_MPH, digits = 1)))%>%
  addPolylines(data = filter(lines.sl_sf_testing_offpeak, Peak == "Off-Peak"),
               color = ~pal3(Median_MPH),
               group = "Off-Peak",
               popup = paste(round(lines.sl_sf_testing_offpeak$Median_MPH, digits = 1)))%>%
  addLayersControl(
    overlayGroups =c("AM", "PM", "Off-Peak"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  addLegend("bottomright", pal = pal3, 
            values = round(lines.sl_sf_testing$Median_MPH, digits = 1),
            title = "Speed of Average Vehicle",
            labFormat = labelFormat(suffix = " MPH", digits = 0),
            opacity = .9
  )

map_t_3 
# alternative method
# m <- leaflet(lines.sl_sf) %>% 
#   addProviderTiles(providers$Stamen.Toner) 
# 
# for (i in unique(lines.sl_sf$RouteSegmentKey)) {
#   m <- m %>% 
#     addPolylines(data = lines.sl_sf[lines.sl_sf$RouteSegmentKey == i, ])
# }
# 
# m # hm...looks the same. maybe we just have some deadhead segments?
# we could, alternatively, remove those trips that have a starting 
# point at the garage??

# let's plot the line's via base or ggplot, see if issue still persists.

plot(lines.sl_sf) # yep... still issue..

ggplot(lines.sl_sf)+
  geom_sf() # yep... still issue..

# # old stuff below
# 
# pure_linestring <- Median_Segment_MPH_joined[,3]
# 
# lines.sl <- SpatialLinesDataFrame(readWKT(pure_linestring[1]))
# 
# lines.sl <- SpatialLinesDataFrame(readWKT(Median_Segment_MPH_joined$ShapeWKT[1]), 
#                                   data=data.frame(RouteSegmentKey=Median_Segment_MPH_joined$RouteSegmentKey[1], 
#                                                   Median_MPH=df$Median_MPH[1]))
# 
# 
# library(sp)
# 
# Median_Segment_MPH_joined <- as(Median_Segment_MPH_joined, Class = "SpatialLines")
# 
# ogr_test <- readOGR(dsn = "Median_Segment_MPH_joined.csv", layer = "ShapeWKT")
# 
# sl_test <- SpatialLinesDataFrame(Median_Segment_MPH_joined)
# 
# library(rgdal)
# 
# st_write(Median_Segment_MPH_joined, "Median_Segment_MPH_joined.csv", 
#          layer_options = "GEOMETRY=AS_WKT",
#          fid_column_name = "RouteSegmentKey")
# 
# x <- st_read("Median_Segment_MPH_joined.csv", 
#              options = "GEOM_POSSIBLE_NAMES=ShapeWKT") # this gets somewhere.. 
# 
# # WKT_test <- rgeos::readWKT(objects_1[2,1]) # this works! (for one...)
# 
# Median_Segment_MPH_joined_SL <- Median_Segment_MPH_joined %>%
#   mutate(ShapeWKT = rgeos::readWKT(ShapeWKT)) # does not work. need to loop.
# 
# Median_Segment_MPH_joined_sf <- st_read(Median_Segment_MPH_joined, stringsAsFactors = FALSE)
# 
# WKT_test_sf <- st_as_sf(WKT_test)
# 
# plot(WKT_test_sf)
# 
# map <- leaflet(data = WKT_test_sf) %>%
#   addProviderTiles(providers$Stamen.Toner) %>%
#   addPolylines(data = WKT_test_sf)
# 
# map # A+++!!!
# 
# # now map summary data
# 
# # ### 7/10/20 Update ### TESTING ###
# # 
# # con3 <- RODBC::odbcDriverConnect('driver={SQL Server Native Client 11.0};server=AVAILDWHP01VW;database=DW_IndyGo;Trusted_Connection=yes;')
# # 
# # DimRouteSegment_alt_3_5 <- RODBC::sqlQuery(con3, "SELECT * FROM DW_IndyGo.dbo.DimRouteSegment_alt_3_5")
# # 
# # library(rgdal)
# # ogrListLayers(DimRouteSegment_alt_3_5$Geom)
# # 
# # library(rgeos)
# # 
# # test_rgeos <- rgeos::readWKT(DimRouteSegment_alt_3_5$Geom[1])
# # 
# # objects_1 <- RODBC::sqlQuery(con3, 'SELECT Geom.STAsText() as ShapeWKT FROM DW_IndyGo.dbo.DimRouteSegment_alt_3_5 ;')
# # 
# # objects_1 # looking good!
# # 
# # objects_1_converted <- dplyr::mutate(objects_1, ShapeWKT = as.character(ShapeWKT))
# # 
# # # objects_1_converted <- na.omit(objects_1_converted) # consider applying this to objects_1
# # 
# # WKT_test <- rgeos::readWKT(objects_1[2,1]) # this works!
# # 
# # # WKT_test <- rgeos::readWKT(objects_1_converted)
# # 
# # plot(WKT_test )
# # 
# # library(leaflet)
# # library(tidyverse)
# # library(sf)
# # 
# # WKT_test_sf <- st_as_sf(WKT_test)
# # 
# # plot(WKT_test_sf)
# # 
# # map <- leaflet(data = WKT_test_sf) %>%
# #   addProviderTiles(providers$Stamen.Toner) %>%
# #   addPolylines(data = WKT_test_sf)
# #   
# # map # A+++!!!
# # 
# # # will likely need to consider the extent to which route segments overlap...
# # 
# # # things <- vector("list", 1)
# # # 
# # # z = 0
# # # for(line in DimRouteSegment_alt_3_5$Geom)
# # # {
# # #   {
# # #     things[[z+1]]<-readWKT(line)
# # #   }
# # #   z = z + 1
# # # }
# # # 
# # # test_case <- as.character(objects_1[2,1])
# # # 
# # # WKT_test <- readWKT(test_case)
# # # 
# # # plot(things[[1]]) 
# # # 
# # # dev.off()