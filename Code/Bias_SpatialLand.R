### Objective: Evaluate effect of fishing on bias between observed and predicted latitude shift

library(data.table)
library(lmerTest)


load("Output/RAM_U_out.RData", verbose=T)
load("Output/cent_out.RData", verbose=T)



uspp.yrs[,"SCINAME":=toupper(scientificname)]
cent_master_lim[,"SCINAME":=toupper(spp)]



#### Fishing intensity on bias only
cent_u <- merge(cent_master_lim, useries3, 
                by.x=c("spp", "year", "subarea"), 
                by.y=c("scientificname", "year", "subarea"))

mod_u <- lmer(annual.bias ~ UdivUmsypref+ (1|spp) + (1|subarea), cent_u)
summary(mod_u)


### SPATIAL LANDINGS ########
### Landings database corrected for missing southern state landings in Lucey catch database
### output from SpatialLandings_StatArea_Stock.R
spp.land.dt <- readRDS("Output/total.land.latbin.stock.rds")
spp.land.dt[,"lat":=as.numeric(as.character(lat.bin))]

### Merge with centroid information (in order to calculate whether landings N or S of centroid)
spp.land.cent <- merge(spp.land.dt, cent_master_lim[subarea=="US East Coast"], 
                       by=c("year", "spp"))

### Classify if landings were N or S of yearly centroid
spp.land.cent[,"landNS":=ifelse(lat >=lat.cent, "N", "S")]

land_spatial <- spp.land.cent[,list(landN=sum(tl_mt[landNS=="N"],na.rm=T),
                                    landS=sum(tl_mt[landNS=="S"], na.rm=T)),
                              by=list(spp, year, lat.cent, lat.cent.cm, 
                                      num.obs, nyrs.obs, annual.bias, sign.pred)]

# relative distribution of landings (log(landN/landS))
land_spatial[,"pos_landN":=ifelse(landN==0, 0.00001, landN)]
land_spatial[,"pos_landS":=ifelse(landS==0, 0.00001, landS)]
land_spatial[,"log_landNS":=log(pos_landN/pos_landS)]

# We assume whether N or S is on the leading or lagging edge of the range
# depends on the direction predicted for the range to move in that year
# e.g. if predicted to move South, then landings in South are on the "leading" edge of that range shift
# So multiply the distribution of landings by the sign of the predicted shift
land_spatial[,"log_landedge":=log_landNS*sign.pred]

hist(land_spatial$log_landNS)
hist(land_spatial$log_landedge)


### Model just with the distribution of landings
mod_spatialland <- lmer(annual.bias ~ log_landedge + (1|spp), land_spatial)




### Model with both intensity and distribution of landings
land_spatial_U <- merge(land_spatial, useries3, by.x=c("spp", "year"), by.y=c("scientificname", "year"))

mod_spatialU <- lmer(annual.bias ~ UdivUmsypref + log_landedge + (1|spp), land_spatial_U)
summary(mod_spatialU)
