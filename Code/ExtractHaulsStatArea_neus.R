
library(rgdal)
library(sp)
library(data.table)
library(geosphere)

### Load stat area look up I compiled
### Mostly derived from https://www.nefsc.noaa.gov/sos/
stock_stat <- as.data.table(read.csv("Data/StatusFishResStatAreas.csv"))

### 4 species where stocks overlap in latitude in some section
### Yellowtail flounder, winter flounder, windowpane flounder, lobster
stock_stat[DoubleLat=="y", unique(RAM_stockid)] 

### Load statistical areas
stat_areas <- readOGR("GIS_Data/NEFSC_statistical_areas/", "Statistical_Areas")
stat_areas_meta <- stat_areas@data
stat_areas_meta$area <- areaPolygon(stat_areas)/(1000*1000) #area in square km
stat_areas@data <- stat_areas_meta

### Load haul locations from Jim
hauls <- readRDS("Data/hauls_jm.rds")
hauls.neus <- hauls[region=="NEFSC_NEUS"]
hauls.orig <- copy(hauls.neus)

coordinates(hauls.neus) <- ~cbind(hauls.neus$lon, hauls.neus$lat)
proj4string(hauls.neus) <- proj4string(stat_areas)



### Extract haul locations from stock stat areas
hauls.stock2 <- stock_stat[,j={
  print(RAM_stockid)
  t.dt <- .SD
  sub_stat_areas <- subset(stat_areas, Id %in% t.dt$StatArea)
  #hauls_st <- over(sub_stat_areas, hauls, returnList=T)
  #hauls_st_all <- rbindlist(hauls_st)
  hauls_st <- over(hauls.neus, sub_stat_areas)
  hauls_stock <- copy(hauls.orig)
  hauls_stock[,"StatArea":=hauls_st$Id]
  hauls_stock[,"area":=hauls_st$area]
  hauls_keep <- hauls_stock[!(is.na(StatArea))]
  list(haulid=hauls_keep$haulid, region=hauls_keep$region, lat=hauls_keep$lat, lon=hauls_keep$lon, year=hauls_keep$year,
       StatArea=hauls_keep$StatArea, area=hauls_keep$area)
}, by=list(RAM_stockid, spp)]





saveRDS(hauls.stock2, "Output/hauls_ram_neus.rds")



