
library(data.table)
library(rgdal)

### Observer (1996-2017) and Fish Ticket (1991-2016) data from Alan Haynie, shared with me 10 August 2017
obs <- as.data.table(read.csv("Data/SpatialLand/Haynie_AKData/OBS081017.csv"))
ft <- as.data.table(read.csv("Data/SpatialLand/Haynie_AKData/FT081017.csv"))

#### Load species table (where spp matched to OBS IDs, RAM spp names, and FT)
spp <- as.data.table(read.csv("Data/SpatialLand/Haynie_AKData/ak.master.spp.full.csv"))

obs_spp <- merge(obs, spp, by.x="OBS_SPECIE_CODE", by.y="OBS_SPECIES_NO")

ft_spp <- merge(ft, spp, by=c("ADFG_I_SPECIES_CODE", "AKFIN_SPECIES_CODE"))
                              


# ADFG Statistical Areas --------------------------------------------------
### ADFG Statistical Area shapefiles since 2001
stat6 <- readOGR("GIS_Data/AK_Groundfish_Statistical_Areas_2001/", "Groundfish_Statistical_Areas_2001")

stat6.dat <- as.data.table(stat6@data)

stat6.dat$lon <- coordinates(stat6)[,1]
stat6.dat$lat <- coordinates(stat6)[,2]

plot(lat ~ lon, stat6.dat, xlim=c(-180, -100), cex=0.25)

## historical stat areas (Geodatabase)
# The input file geodatabase
fgdb <- "GIS_data/Groundfish_Statistical_Areas_1985-2000.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
stat6_old <- readOGR(dsn=fgdb,layer="PVG_Statewide_1986_2000_GCS_WGS1984")
stat6_old.dat <- as.data.table(stat6_old@data)

stat6_old.dat$lon <- coordinates(stat6_old)[,1]
stat6_old.dat$lat <- coordinates(stat6_old)[,2]

stat6_old_only <- stat6_old.dat[!(STAT_AREA %in% stat6.dat$STAT_AREA)]

### Combine into one stat_area_loc file
stat_loc_keep <- c("STAT_AREA", "lon", "lat")

stat6_area_loc <- rbind(stat6.dat[,stat_loc_keep, with=F], stat6_old_only[,stat_loc_keep, with=F])
plot(lat ~ lon, stat6_area_loc, xlim=c(-180, -100), cex=0.25)
points(lat ~ lon, stat6_old_only, col="red")


### Prince William Sound Salmon areas




# Merge location information into fish tickets ----------------------------
ft_stat <- merge(ft_spp, stat6_area_loc, by.x="ADFG_I_STAT_AREA", by.y="STAT_AREA")

ft_nonstat <- ft_spp[, 
                     list(land_all=as.numeric(sum(WHOLE_LBS, na.rm=T)),
                          land_nonstat=sum(WHOLE_LBS[!(ADFG_I_STAT_AREA %in% stat6_area_loc$STAT_AREA)])),
                     by=list(spp, AKFIN_YEAR)]
ft_nonstat[,"frac.nonstat":=land_nonstat/land_all]
### Only a few species where landings in non-stat6 areas represents > 10%

ft_nonstat_area <- ft_spp[!(ADFG_I_STAT_AREA %in% stat6_area_loc$STAT_AREA),
                           list(lbs=sum(WHOLE_LBS, na.rm=T)),
                           by=list(ADFG_I_STAT_AREA)]
setorder(ft_nonstat_area, ADFG_I_STAT_AREA)

#write.csv(ft_nonstat_area, "Data/nonstat_area.csv")

### Species with majority of years with landings not in stat6 areas (most all 26 yrs)
# Herring (Clupea harengus pallasi)
# Salmon (Oncorhynchus spp)
# Shrimp (Panopea generosa, Pandalus borealis, Pandalus hypsinotis, Pandalus platyceros)
# Urchin (S. franciscanus), sea cucumber (Parastichopus californicus)
# Crab (Cancer magister)
spp_nonstat <-  ft_nonstat[frac.nonstat>0.5, list(num.years=length(unique(AKFIN_YEAR))), by=list(spp)]
spp_exclude <- spp_nonstat[num.years>20]$spp


### Most landings for shrimp in 5 digit STAT_AREAs: FMP_AREA "INSD" FMP_SUBAREA "SEI"
# Inside/Southeast (INSD)
# These must be southeast Alaska shellfish stat areas 
# see maps in GIS_Data/SEAK_SalmonShellfish that have numbers matching these 101-11 in Chart5b through 192-60 in Chart 5e
ft_spp[spp=="Pandalus borealis", list(n_ft=sum(N_FT)), by=list(ADFG_I_STAT_AREA, FMP_AREA, FMP_SUBAREA)]


