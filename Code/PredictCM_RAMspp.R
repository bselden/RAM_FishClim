
library(data.table)
library(Hmisc)

load("Data/hauls_catch_Dec2017.RData", verbose=T)

ram.stocks <- readRDS("Data/ram.stock.us.rds")
setorder(ram.stocks, scientificname)

### Species catch by haul
catch.dt <- as.data.table(dat)
haul.dt <- as.data.table(hauls)

### Classify hauls into RAM regions
haul.dt[,"subarea":=ifelse(regionfact=="AFSC_EBS", "EBS",
                           ifelse(regionfact=="AFSC_Aleutians", "AI", 
                              ifelse(regionfact=="AFSC_GOA", "GOA",
                                     ifelse(regionfact=="AFSC_WCTri", "US West Coast",
                                            ifelse(regionfact=="NEFSC_NEUS", "US East Coast",
                                                   ifelse(regionfact=="NWFSC_WCAnn", "US West Coast",
                                                          ifelse(regionfact=="SEFSC_GOMex", "GMex",
                                                                 ifelse(regionfact=="SCDNR_SEUS", "SEUS", 
                                                                        ifelse(regionfact=="DFO_Newfoundland", "Canada East Coast",
                                                                               ifelse(regionfact=="DFO_SoGulf", "Canada East Coast",
                                                                                      ifelse(regionfact=="DFO_ScotianShelf", "Canada East Coast", NA)))))))))))]

haul.stratum.obs <- haul.dt[,list(num.yrs.obs=length(unique(year))), by=list(stratum, regionfact, subarea, ocean, surveyfact)]
yrs.obs.region <- haul.dt[,list(num.yrs.surv=length(unique(year))), by=list(regionfact, subarea, ocean, surveyfact)]
haul.stratum.obs2 <- merge(haul.stratum.obs, yrs.obs.region, by=c("regionfact", "ocean", "subarea", "surveyfact"))
haul.stratum.obs2[,"frac.yrs":=num.yrs.obs/num.yrs.surv]

### Limit to strata observed in >=90% of years of a survey & year >=1968 (so both spring and fall combined for NEUS)
haul.dt2 <- haul.dt[stratum %in% haul.stratum.obs2[frac.yrs>=0.9]$stratum & year>=1968]
catch.dt2 <- catch.dt[haulid %in% haul.dt2$haulid]
catch.dt2[,"spp":=capitalize(gsub("_.*$", "", sppocean))]


### Limit to just RAM species
catch.dt.ram <- catch.dt2[spp %in% ram.stocks$scientificname]


### Merge with haul location
catch.wloc <- merge(catch.dt.ram, 
                    haul.dt2[,list(haulid=haulid, regionfact=regionfact)], 
                    by=c("haulid"))

### Create master sheet where every species
haul.master <- catch.wloc[,j={
  t.dt <- .SD
  temp <- CJ(haulid=unique(t.dt$haulid), sppocean=unique(t.dt$sppocean))
}, by=list(regionfact)]
haul.master[,"spp":=capitalize(gsub("_.*$", "", sppocean))]

### Merge back into haul.dt2 to get other data
haul.master.meta <- merge(haul.master, haul.dt2, by=c("haulid", "regionfact"))

### Keep only the variables I want for all hauls
keep.cols <- c("haulid", "regionfact", "subarea", "spp", "sppocean", "lat", "lon", "stratum",
               "region", "year", "month", "depth", "rugosity", "GRAINSIZE",
               "SBT.actual", "SST.actual", "SBT.seasonal", "SST.seasonal.mean", 
               "SBT.min", "SBT.max", "SST.min", "SST.max")
haul.master.meta <- haul.master.meta[,keep.cols, with=F]

## This confirms that every species had all hauls in a region represented
with(haul.master.meta[regionfact=="NEFSC_NEUS" & spp %in% c("Gadus morhua", "Centropristis striata", "Urophycis chuss")], 
     table(year, spp))

### Merge with catch data, and add zeros where not observed
catch.w.zeros <- merge(haul.master.meta, catch.wloc, by=c("haulid", "spp", "sppocean", "regionfact"), all.x=T)
catch.w.zeros[,"wtcpue":=ifelse(is.na(wtcpue), 0, wtcpue)]
catch.w.zeros[,"logwtcpue":=ifelse(is.na(logwtcpue), -10, logwtcpue)]
catch.w.zeros[,"presfit":=ifelse(is.na(presfit), FALSE, presfit)]
setorder(catch.w.zeros, sppocean)

### Clim Fits 
### From Jim Morley
### Downloaded from Amphiprion nicheMods_PlosOne2018
mod_list <- list.files("Data/CEmods", ".RData")
sppocean_list <- gsub("CEmods_Nov2017_fitallreg_2017_", "", gsub(".RData", "", mod_list))

spp_lookup <- data.table(sppocean=sppocean_list)
spp_lookup[,"spp":=capitalize(gsub("_.*$", "", sppocean))]



pred.dt <- catch.w.zeros[sppocean %in% spp_lookup$sppocean,j={
  print(sppocean)
  t.dt <- .SD
  spp <- which(sppocean_list==eval(sppocean))
  
  load(paste0("Data/CEmods/", mod_list[spp]), verbose=T)
  
  dt.fit <- as.data.table(mods$mygam1$model)
  dt.fit.bio <- as.data.table(mods$mygam2$model)
  region.orig <- levels(dt.fit$regionfact) #original fit only had some regions
  
  # some didn't have regionfact in there (potentially for spp in single regions?)
  if(!(is.null(region.orig))){
    sub <- t.dt[regionfact %in% region.orig]
  } else{sub <- copy(t.dt)}
  
  
  sub[,"pred.pres" := predict(mods$mygam1, newdata=sub, type="response")]
  sub[,"pred.log.bio":=predict(mods$mygam2, newdata=sub, type="response")]
  
  ### Combined prediction (pred.pres * exp(pred.log.bio))
  sub[,"pred.total":=pred.pres * exp(pred.log.bio)]
  

  list(haulid=sub$haulid,
       spp=sub$spp,
       regionfact=sub$regionfact,
       subarea=subarea,
       lat=sub$lat,
       lon=sub$lon,
       stratum=sub$stratum,
       region=sub$region,
       year=sub$year,
       month=sub$month,
       depth=sub$depth,
       SBT.actual=sub$SBT.actual,
       SBT.seasonal=sub$SBT.seasonal,
       SST.seasonal.mean=sub$SST.seasonal.mean,
       SBT.min=sub$SBT.min,
       SBT.max=sub$SBT.max,
       SST.min=sub$SST.min,
       SST.max=sub$SST.max,
       wtcpue=sub$wtcpue,
       logwtcpue=sub$wtcpue,
       presfit=sub$presfit,
       pred.pres=sub$pred.pres,
       pred.log.bio=sub$pred.log.bio,
       pred.total=sub$pred.total)  
  
}, by=list(sppocean)]

pred.dt[is.na(spp), unique(sppocean)]


saveRDS(pred.dt, "Output/pred.dt.rds")

#Run if want to read in existing output
pred.dt <- readRDS("Output/pred.dt.rds")




##################################
### Centroids in each year (and lagged)

### Centroid in each year, and mean environmental variables
### Averaging across fall and spring (previously limited to 1968 and later)
spp_dist <- pred.dt[complete.cases(lat, lon, depth),
                    list(obs.lat=weighted.mean(lat, wtcpue),
                          obs.lon=weighted.mean(lon, wtcpue),
                          obs.depth=weighted.mean(depth, wtcpue),
                          SBT.seasonal=mean(SBT.seasonal),
                          SST.seasonal=mean(SST.seasonal.mean),
                          SBT.min=mean(SBT.min),
                          SST.min=mean(SST.min),
                          SBT.max=mean(SBT.max),
                          SST.max=mean(SST.max),
                          wtcpue=mean(wtcpue),
                          freq.occ=mean(presfit),
                          pred.lat.pres=weighted.mean(lat, pred.pres),
                          pred.lon.pres=weighted.mean(lon, pred.pres),
                          pred.depth.pres=weighted.mean(depth, pred.pres),
                          pred.lat.bio=weighted.mean(lat, pred.total),
                          pred.lon.bio=weighted.mean(lon, pred.total),
                          pred.depth=weighted.mean(depth, pred.total)),
                    by=list(spp, sppocean, subarea, year)]
setorder(spp_dist, spp, sppocean, subarea, year)

# Matches Jim's gams in bsb_gamVSbrt_allseasons.pdf email from 10/9/2018
plot(obs.lat ~ year, spp_dist[spp=="Centropristis striata" & subarea=="US East Coast"], type="o",
     ylab="Latitude (w=wtcpue)")
points(pred.lat.bio ~ year, spp_dist[spp=="Centropristis striata" & subarea=="US East Coast"], type="o", col="blue")



### Lead variables
vars <- c("obs.lat", "obs.lon", "obs.depth", "pred.lat.bio", "pred.lon.bio", "pred.depth")

lead1cols <- paste("lead1", vars, sep=".")

cent_master_lim <- copy(spp_dist)

cent_master_lim[,(lead1cols):=shift(.SD, n=1, type="lead"), by=list(spp, sppocean, subarea), .SDcols=vars]




### Annual difference
cent_master_lim[,"lat.cent.diff":= lead1.obs.lat - obs.lat, by=list(sppocean, spp, subarea)]
cent_master_lim[,"lat.cent.cm.diff":=lead1.pred.lat.bio - pred.lat.bio, by=list(sppocean, spp, subarea)]



### Observed - predicted annual diff
cent_master_lim[,"annual.obsminuspred":=lat.cent.diff - lat.cent.cm.diff]
cent_master_lim[,"sign.pred":=sign(lat.cent.cm.diff)]
cent_master_lim[,"annual.bias":=annual.obsminuspred*sign.pred]

hist(cent_master_lim$annual.bias)

cent_master_lim[,"nyrs.obs":=length(unique(year[is.finite(obs.lat)])), by=list(sppocean, spp, subarea)]


#### Rate of change over time for spp sub area combinations with at least 5 years of observations
### 271 spp, subarea combinations
cent_lm <- cent_master_lim[nyrs.obs>5,j={
  print(paste0(spp, " ", subarea))
  t.dt <- .SD
  lm.obs <- lm(obs.lat ~ year)
  lm.pred <- lm(pred.lat.bio ~ year, t.dt)
  list(lm.obs.slope=summary(lm.obs)$coefficients[2,1], 
       lm.obs.p=summary(lm.obs)$coefficients[2,4],
       lm.obs.r2=summary(lm.obs)$r.squared,
       lm.pred.slope=summary(lm.pred)$coefficients[2,1],
       lm.pred.p=summary(lm.pred)$coefficients[2,4],
       lm.pred.r2=summary(lm.pred)$r.squared)
}, by=list(sppocean, spp, subarea, nyrs.obs)]

cent_lm[,"obsminuspred":=ifelse(lm.pred.slope>0, lm.obs.slope - lm.pred.slope, -(lm.obs.slope-lm.pred.slope))]



png("Figures/lagclim_null.png", height=5, width=5, units="in", res=300)
plot(lm.obs.slope ~ lm.pred.slope, cent_lm)
points(lm.obs.slope ~ lm.pred.slope, cent_lm[obsminuspred<0], col="blue", pch=10, cex=0.5)
abline(a=0, b=1, col="red")
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

plot(freq.occ ~ year, cent_master_lim[spp=="Centropristis striata" & subarea=="US East Coast"])
plot(freq.occ ~ year, cent_master_lim[spp=="Gadus morhua" & subarea=="US East Coast"])


save(cent_master_lim, cent_lm, file="Output/cent_out.RData")
