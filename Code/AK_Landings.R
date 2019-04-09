
library(data.table)
library(rgdal)

### Observer (1996-2017) and Fish Ticket (1991-2016) data from Alan Haynie, shared with me 10 August 2017
obs <- as.data.table(read.csv("Data/SpatialLand/Haynie_AKData/OBS081017.csv"))
ft <- as.data.table(read.csv("Data/SpatialLand/Haynie_AKData/FT081017.csv"))

#### Load species table (where spp matched to OBS IDs, RAM spp names, and FT)
spp <- readRDS("Data/AK_spp_obs_RAM.rds")

obs_spp <- merge(obs, spp, by.x="OBS_SPECIE_CODE", by.y="OBS_SPECIES_NO")

ft_spp <- merge(ft, spp, by=c("ADFG_I_SPECIES_CODE", "AKFIN_SPECIES_CODE"))
                              


### ADFG Statistical Area shapefiles
stat6 <- readOGR("GIS_Data/AK_Groundfish_Statistical_Areas_2001/", "Groundfish_Statistical_Areas_2001")

stat6.dat <- as.data.table(stat6@data)

stat6.dat$lon <- coordinates(stat6)[,1]
stat6.dat$lat <- coordinates(stat6)[,2]

plot(lat ~ lon, stat6.dat, xlim=c(-180, -100), cex=0.25)


# ### Average lat lon from observer data for each stat area (a way to add in some missing stat areas)
### But there are none that aren't in both
# obs_stat_loc <- obs[,list(lon=mean(AvgOfRETRIEVAL_LONGITUDE_DD),
#                            lat=mean(AvgOfRETRIEVAL_LATITUDE_DD)),
#                      by=list(ADFG_STAT_AREA_CODE)]
# setorder(obs_stat_loc, ADFG_STAT_AREA_CODE)



ft_stat <- merge(ft_spp, stat6.dat, by.x="ADFG_I_STAT_AREA", by.y="STAT_AREA")

ft_nonstat <- ft_spp[, 
                     list(land_all=as.numeric(sum(WHOLE_LBS, na.rm=T)),
                          land_nonstat=sum(WHOLE_LBS[!(ADFG_I_STAT_AREA %in% stat6.dat$STAT_AREA)])),
                     by=list(scientificname, AKFIN_YEAR)]
ft_nonstat[,"frac.nonstat":=land_nonstat/land_all]
### Only a few species where landings in non-stat6 areas represents > 10%

ft_nonstat_area <- ft_spp[!(ADFG_I_STAT_AREA %in% stat6.dat$STAT_AREA),
                           list(lbs=sum(WHOLE_LBS, na.rm=T)),
                           by=list(ADFG_I_STAT_AREA)]
setorder(ft_nonstat_area, ADFG_I_STAT_AREA)

#write.csv(ft_nonstat_area, "Data/nonstat_area.csv")

### Get geographic information for old statistical areas based on union of new shapefiles
old_stat <- read.csv("Data/nonstat_area_complete.csv")
