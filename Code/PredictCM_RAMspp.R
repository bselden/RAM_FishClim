
library(data.table)
library(raster)
library(Hmisc)

load("Data/hauls_catch_Dec2017.RData", verbose=T)

### Species catch by haul
catch.dt <- as.data.table(dat)
haul.dt <- as.data.table(hauls)

### Classify hauls into RAM regions
haul.dt[,"subarea":=ifelse(regionfact=="AFSC_Aleutians", "BSAI",
                           ifelse(regionfact=="AFSC_EBS", "BSAI",
                              ifelse(regionfact=="AFSC_GOA", "GOA",
                                     ifelse(regionfact=="AFSC_WCTri", "US West Coast",
                                            ifelse(regionfact=="NEFSC_NEUS", "US East Coast",
                                                   ifelse(regionfact=="NWFSC_WCAnn", "US West Coast",
                                                          ifelse(regionfact=="SEFSC_GOMex", "US Southeast and Gulf",
                                                                 ifelse(regionfact=="SCDNR_SEUS", "US Southeast and Gulf", 
                                                                        ifelse(regionfact=="DFO_Newfoundland", "Canada East Coast",
                                                                               ifelse(regionfact=="DFO_SoGulf", "Canada East Coast",
                                                                                      ifelse(regionfact=="DFO_ScotianShelf", "Canada East Coast", NA)))))))))))]

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


### Clim Fits
### From Jim Morley 
### Downloaded from Amphiprion nicheMods_PlosOne2018
mod_list <- list.files("Data/CEmods", ".RData")
sppocean_list <- gsub("CEmods_Nov2017_fitallreg_2017_", "", gsub(".RData", "", mod_list))

spp_lookup <- data.table(sppocean=sppocean_list)
spp_lookup[,"spp":=capitalize(gsub("_.*$", "", sppocean))]

pred_list <- vector("list", length(sppocean_list))

# ### Black Sea Bass only
# mod_list <- c("CEmods_Nov2017_fitallreg_2017_centropristis striata_Atl.RData")
# sppocean_list <- gsub("CEmods_Nov2017_fitallreg_2017_", "", gsub(".RData", "", mod_list))

pred_list <- vector("list", length(sppocean_list))

for(i in 1:length(mod_list)){
  print(sppocean_list[i])
  load(paste0("Data/CEmods/", mod_list[i]), verbose=T)
  
  dt.fit <- as.data.table(mods$mygam1$model)
  dt.fit.bio <- as.data.table(mods$mygam2$model)
  region.orig <- levels(dt.fit$regionfact) #original fit only had some regions
  
  cols.dt.fit <- colnames(dt.fit)
  haul.dt.lim <- merge(haul.dt2, dt.fit, by=cols.dt.fit[cols.dt.fit != "presfit"])

  pred.fit <- predict(mods$mygam1, newdata=haul.dt.lim, type="response")
  pred.fit.bio <- predict(mods$mygam2, newdata=haul.dt.lim, type="response")
  
  ### Combined prediction (pred.pres * exp(pred.log.bio))
  pred.total<- pred.fit * exp(pred.fit.bio)
  
  pred_list[[i]] <- data.table(haulid=haul.dt.lim$haulid,
                               stratum=haul.dt.lim$stratum,
                               year=haul.dt.lim$year,
                               month=haul.dt.lim$month,
                               regionfact=haul.dt.lim$regionfact,
                               lat=haul.dt.lim$lat,
                               lon=haul.dt.lim$lon,
                               depth=haul.dt.lim$depth,
                               bottemp=haul.dt.lim$bottemp,
                               surftemp=haul.dt.lim$surftemp,
                               SST.seasonal.mean=haul.dt.lim$SST.seasonal.mean,
                               SBT.seasonal=haul.dt.lim$SBT.seasonal,
                               presfit=haul.dt.lim$presfit,
                               pred.pres=pred.fit, 
                               pred.log.bio=pred.fit.bio,
                               pred.total=pred.total,
                               sppocean=sppocean_list[i])
}

#pred.dt <- rbindlist(pred_list)
#saveRDS(pred.dt, "Output/pred.dt.rds")

#saveRDS(pred.dt, "Output/yellowtail.pred.rds")

pred.dt <- readRDS("Output/pred.dt.rds")


### Compare prediction and observed for BSB
cent.obs <- catch.wloc[,list(lat.cent=weighted.mean(lat, w=wtcpue),
                             lat.cent.pres=weighted.mean(lat, w=presfit)), 
                       by=list(sppocean, regionfact, year)]

cent.cm <- pred.dt[,list(lat.cent.cm =weighted.mean(lat, w=pred.total),
                    lat.cent.pres.cm=weighted.mean(lat, w=pred.pres)),
              by=list(sppocean, regionfact, year)]
setorder(cent.cm, year)

plot(lat.cent ~ year, 
     cent.obs[year >=1968 & sppocean=="centropristis striata_Atl" & regionfact=="NEFSC_NEUS"], 
     type="o", ylab="Latitude (w=wtcpue)")
points(lat.cent.cm ~ year,
       cent.cm[year>=1968 & sppocean=="centropristis striata_Atl" & regionfact=="NEFSC_NEUS"],
       type="o", col="blue", ylab="Latitude (w=predicted wtcpue)")







