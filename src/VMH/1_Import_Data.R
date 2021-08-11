library(DBI)
library(data.table)
library(lubridate)
library(stringr)
library(dplyr)

con2 <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "REPSQLP01VW", 
                       Database = "TransitAuthority_IndyGo_Reporting", 
                       Port = 1433)

VMH_Query <- tbl(con2, sql(paste0("select a.Time
,a.Route
,Boards
,Alights
,Stop_Dwell_Time
,Trip
,Speed
,Previous_Stop_Id
,Vehicle_ID
,Stop_Name
,Stop_Id
,Inbound_Outbound
,Departure_Time
,Latitude
,Longitude
,GPSStatus
,StationaryStatus
,StationaryDuration
from avl.Vehicle_Message_History a (nolock)
left join avl.Vehicle_Avl_History b
on a.Avl_History_Id = b.Avl_History_Id
where a.Route like '90%'
and a.Time > '20191001'
and a.Time < DATEADD(day,1,'20200229')
and GPSStatus = 2")))

# read in VMH data --------------------------------------------------------
VMH_Raw <- VMH_Query %>% collect() %>% setDT()

# VMH_Raw[,.N,Stop_Dwell_Time]
# do transit_day
transit_day_cutoff <- as.ITime("03:30:00")

VMH_Raw[
  , DateTest := fifelse(data.table::as.ITime(Time) < transit_day_cutoff
                        , 1
                        , 0
                        ) #end fifelse()
  ][
    , Transit_Day := fifelse(DateTest == 1
                             ,data.table::as.IDate(Time)-1
                             ,data.table::as.IDate(Time)
                             )
    ]

VMH_Raw <- VMH_Raw[Transit_Day >= "2019-10-01" & 
                     Transit_Day <= "2020-02-29"]

fwrite(VMH_Raw,"data//processed//VMH_Raw.csv")
