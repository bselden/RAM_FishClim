### Examine observe and predicted changes within stock boundaries
library(data.table)
library(raster)

### Load data
load("Data/hauls_catch_Dec2017.RData", verbose=T)

### Species catch by haul
catch.dt <- as.data.table(dat)

haul.dt <- as.data.table(hauls)

haul.stratum.obs <- haul.dt[,list(num.yrs.obs=length(unique(year))), by=list(stratum, regionfact, ocean, surveyfact)]
yrs.obs.region <- haul.dt[,list(num.yrs.surv=length(unique(year))), by=list(regionfact, ocean, surveyfact)]
haul.stratum.obs2 <- merge(haul.stratum.obs, yrs.obs.region, by=c("regionfact", "ocean", "surveyfact"))
haul.stratum.obs2[,"frac.yrs":=num.yrs.obs/num.yrs.surv]

### Limit to strata observed in >=90% of years of a survey
haul.dt2 <- haul.dt[stratum %in% haul.stratum.obs2[frac.yrs>=0.9]$stratum]
catch.dt2 <- catch.dt[haulid %in% haul.dt2$haulid]

### Merge with haul location
catch.wloc <- merge(catch.dt2, 
                    haul.dt2[,list(haulid=haulid, lat=lat, lon=lon, regionfact=regionfact, year=year, month=month)], 
                    by=c("haulid"))
 


### Prediction from JM Climate models (GAM fits applied to historical conditions)
cm <- readRDS("Output/pred.dt.rds")

#######################
### Compare prediction and observed for a few species in each region
cent.obs <- catch.wloc[,list(lat.cent=weighted.mean(lat, w=wtcpue),
                             lat.cent.pres=weighted.mean(lat, w=presfit)), 
                       by=list(sppocean, regionfact, year)]

cent.cm <- cm[,list(lat.cent.cm =weighted.mean(lat, w=pred.total),
                    lat.cent.pres.cm=weighted.mean(lat, w=pred.pres)),
                    by=list(sppocean, regionfact, year)]
setorder(cent.cm, year)

plot(lat.cent.pres ~ year, 
     cent.obs[year >=1968 & sppocean=="centropristis striata_Atl" & regionfact=="NEFSC_NEUS"], 
     type="o")
points(lat.cent.pres.cm ~ year,
       cent.cm[year>=1968 & sppocean=="centropristis striata_Atl" & regionfact=="NEFSC_NEUS"],
       type="o", col="blue")

plot(lat.cent ~ year, 
     cent.obs[year >=1968 & sppocean=="urophycis chuss_Atl" & regionfact=="NEFSC_NEUS"], 
     type="o")
points(lat.cent.cm ~ year,
       cent.cm[year>=1968 & sppocean=="urophycis chuss_Atl" & regionfact=="NEFSC_NEUS"],
       type="o", col="blue")

# #### Ocean Adapt Projections
# load("Data/CEproj/Centropristis striata_Atlantic_OceanAd_projection.RData", verbose=T)

#######################
### 
### Hauls within stock boundaries
hauls_ram <- readRDS("Output/hauls_ram.rds")
