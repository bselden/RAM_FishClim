### Examine effect of climate and fisheries during VTR period where also have effort data

library(data.table)
library(stringr)
library(rgdal)
library(raster)

vtr <- readRDS("Data/VTR/CaS.commercial_all_port_land_wvessel_info.rds")
vtr.spp <- as.data.table(read.csv("Data/VTR_sppcodes_compiled_NEW.csv"))

### Summarize landings by species, year and cnemarea
### Landings in pounds
land_statarea <- vtr[,list(land=sum(qtykept, na.rm=T),
                           declat=mean(declat, na.rm=T),
                           declon=mean(declon, na.rm=T)),
                     by=list(sppcode, year, cnemarea)]

### Lucey landings database by statarea
load("Data/SpatialLand/NEFSC/comland_by_statarea.RData")
load("Data/SpatialLand/NEFSC/Species_codes_NEW.RData")

spp.land.dt <- as.data.table(spp.land)
spp <- as.data.table(spp)


### Biomass and environmental data from Jim
load("Data/hauls_catch_Dec2017.RData", verbose=T)

### Species catch by haul
catch.dt <- as.data.table(dat)
haul.dt <- as.data.table(hauls)
haul.neus <- haul.dt[regionfact=="NEFSC_NEUS"]
haul.loc <- haul.neus[,c("lat", "lon", "haulid")]
coordinates(haul.loc) <- cbind(haul.loc$lon, haul.loc$lat)

### Get statistical area for each haul
### NOTE: this will remove landings from those stat areas that changed due to the Hague line (1980?)
### Because not a current stat area
nefsc_stat <- readOGR("GIS_Data/NEFSC_statistical_areas/Statistical_Areas.shp")
proj4string(haul.loc) <- proj4string(nefsc_stat)
stat_centroid <- coordinates(nefsc_stat)

## Area of statarea (m2)
stat_area <- raster::area(nefsc_stat)



nefsc_stat_meta <- data.table(statarea=nefsc_stat@data$Id,
                              area_km2=stat_area/(1000*1000),
                              stat_lon=stat_centroid[,1],
                              stat_lat=stat_centroid[,2])

haul.loc$statarea <- over(haul.loc, nefsc_stat)$Id

haul.loc.dt <- as.data.table(as.data.frame(haul.loc))
haul.loc.dt <- merge(haul.loc.dt, nefsc_stat_meta, by=c("statarea"))



haul.neus.stat <- merge(haul.neus, haul.loc.dt, by=c("haulid", "lat", "lon"))
# Season
haul.neus.stat[, "season":=ifelse(month %in% c(9,10,11), "fall", 
                               ifelse(month %in% c(3,4,5), "spring", NA))]

# ### Environmental trends on hauls (to do properly should only include strata observed consistently)
# haul.env <- haul.neus.stat[,list(SBT=mean(SBT.seasonal, na.rm=T),
#                                  SST=mean(SST.seasonal.mean, na.rm=T)),
#                            by=list(season, year)]



catch.dt[,"spp":=str_to_sentence(gsub("_.*", "", sppocean))]


### Limit to some species
spp.lim <- c("Gadus morhua", "Centropristis striata", "Paralichthys dentatus", 
             "Urophycis chuss", "Stenotomus chrysops", "Limanda ferruginea")

spp.lookup <- data.table(spp=spp.lim)
spp.lookup <- merge(spp.lookup, vtr.spp, by=c("spp"))
spp.lookup[, "SCINAME":= toupper(spp)]
spp.lookup <- merge(spp.lookup, spp, by=c("SCINAME"))

### Trawldata for limited species
catch.lim <- catch.dt[spp %in% spp.lookup$spp]

### Landings data for limited species (sum across NESPP3)
spp.land.dt2 <- merge(spp.land.dt, spp.lookup, by=c("NESPP3"))
spp.land.lim <- spp.land.dt2[,
                            list(land_mt=sum(SPPLIVMT, na.rm=T)), 
                            by=list(YEAR, AREA, spp)]



### Add zeros for hauls conducted in which species was not observed
source("Code/add_zeros_fn.R")

catch.zeros <- add.zeros.sp.bioonly(catch.lim, haul.neus.stat)

#######################


### Limit to year > 1968
survey_stat <- catch.zeros[year>=1968,list(wtcpue=mean(wtcpue, na.rm=T),
                                 depth=mean(depth, na.rm=T),
                                 SBT.seasonal=mean(SBT.seasonal),
                                 SST.seasonal=mean(SST.seasonal.mean)),
                           by=list(year, spp, season, statarea, area_km2, stat_lon, stat_lat)]

### Merge with landings data
survey_land <- merge(survey_stat, spp.land.lim, by.x=c("year", "statarea", "spp"), by.y=c("YEAR", "AREA", "spp"),
                     all.x=T) #keep all records from survey, remove where both not present
survey_land[,"land_mt":=ifelse(is.na(land_mt), 0, land_mt)]


#### Convert survey biomass to kg per km2
# swept area of the bottom trawl gear during a standard R/V Albatross tow (0.0384 km2)
swept <- 0.0384
survey_land[,"kg_km2":=wtcpue/swept]
survey_land[,"bio_mt":=area_km2 * kg_km2/1000] # convert to metric tons (1mt=1000kg) 


#######################
### Analyze fall and spring separately
### Generate lag variables
survey_land[,"bio_next":=data.table::shift(bio_mt, n=1, type="lead"), by=list(statarea, spp, season)]
survey_land[,"SBT_next":=data.table::shift(SBT.seasonal, n=1, type="lead"), by=list(statarea, spp, season)]
survey_land[,"SST_next":=data.table::shift(SST.seasonal, n=1, type="lead"), by=list(statarea, spp, season)]

survey_land[,"bio_diff":=bio_next - bio_mt]
survey_land[,"SBT_diff":=SBT_next - SBT.seasonal]
survey_land[,"SST_diff":=SBT_next - SBT.seasonal]




### Uhler (1979)
### Least squares regression of Schaefer
### Puts in terms of per capita growth
### (X_t-1 - X_t)/X_t = a + bX_t -cE + error
### H=cEX, estimate cE as H/X =(land/wtcpue)
### Assume SBT_diff acts on "a"
survey_land[,"percap_diff":=bio_diff/bio_mt]

### per capita difference The most negative it can be is -1 (100% loss) so add 1.0001 and log 
survey_land[,"percap_plus1":=percap_diff + 1.0001] 
survey_land[,"percap_log":=log(percap_plus1)]
hist(survey_land$percap_log)

### Legitimate when landings are 0, but problematic for logging, set to small number
survey_land[,"land_pos":=ifelse(land_mt==0, 0.0001, land_mt)]
survey_land[,"L_B":=land_pos/bio_mt] 
survey_land[,"logLB":=log(L_B)]


### If biomass is 0, set to be a high number
## OR Exclude those??
survey_land_lim <- survey_land[bio_mt>0]


#survey_land_lim[,"logLB":=log(L_B)] #problem because can have 0 values --> zero inflated something?

hist(survey_land_lim$logLB)
hist(survey_land_lim$SBT_diff)
hist(survey_land_lim$percap_log)

plot(percap_log ~ SBT_diff, survey_land_lim[spp=="Centropristis striata" & season=="fall"])
plot(percap_log ~ logLB, survey_land_lim[spp=="Centropristis striata" & season=="fall"])
plot(percap_log ~ bio_mt, survey_land_lim[spp=="Centropristis striata" & season=="fall"])


bsb_mod <- lm(percap_log ~ SBT_diff + bio_mt + logLB -1, #no intercept model
              survey_land_lim[spp=="Centropristis striata" & season=="fall"])

fluke_mod <- lm(percap_log ~ SBT_diff + bio_mt + logLB -1, #no intercept model
              survey_land_lim[spp=="Paralichthys dentatus" & season=="spring"])

cod_mod <- lm(percap_log ~ SBT_diff + bio_mt + logLB -1, #no intercept model
                survey_land_lim[spp=="Gadus morhua" & season=="spring"])

scup_mod <- lm(percap_log ~ SBT_diff + bio_mt + logLB -1, #no intercept model
              survey_land_lim[spp=="Stenotomus chrysops" & season=="fall"])
plot(cod_mod)

plot(percap_log ~ SBT_diff, 
     survey_land_lim[spp=="Gadus morhua" & season=="spring" & abs(percap_log)<5])
