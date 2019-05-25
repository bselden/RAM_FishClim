
library(data.table)
library(raster)
library(Hmisc)

load("Data/hauls_catch_Dec2017.RData", verbose=T)

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

### Merge with haul location
catch.wloc <- merge(catch.dt2, 
                    haul.dt2[,list(haulid=haulid, lat=lat, lon=lon, regionfact=regionfact, subarea, ocean, year=year, month=month)], 
                    by=c("haulid"))
catch.wloc[,"spp":=capitalize(gsub("_.*$", "", sppocean))]



### Clim Fits (don't run for loop, just load output)
### From Jim Morley
### Downloaded from Amphiprion nicheMods_PlosOne2018
mod_list <- list.files("Data/CEmods", ".RData")
sppocean_list <- gsub("CEmods_Nov2017_fitallreg_2017_", "", gsub(".RData", "", mod_list))

spp_lookup <- data.table(sppocean=sppocean_list)
spp_lookup[,"spp":=capitalize(gsub("_.*$", "", sppocean))]
# 
# pred_list <- vector("list", length(sppocean_list))
# 
# 
# pred_list <- vector("list", length(sppocean_list))
# 
# for(i in 1:length(mod_list)){
#   print(sppocean_list[i])
#   load(paste0("Data/CEmods/", mod_list[i]), verbose=T)
#   
#   dt.fit <- as.data.table(mods$mygam1$model)
#   dt.fit.bio <- as.data.table(mods$mygam2$model)
#   region.orig <- levels(dt.fit$regionfact) #original fit only had some regions
#   
#   cols.dt.fit <- colnames(dt.fit)
#   haul.dt.lim <- merge(haul.dt2, dt.fit, by=cols.dt.fit[cols.dt.fit != "presfit"])
# 
#   pred.fit <- predict(mods$mygam1, newdata=haul.dt.lim, type="response")
#   pred.fit.bio <- predict(mods$mygam2, newdata=haul.dt.lim, type="response")
#   
#   ### Combined prediction (pred.pres * exp(pred.log.bio))
#   pred.total<- pred.fit * exp(pred.fit.bio)
#   
#   pred_list[[i]] <- data.table(haulid=haul.dt.lim$haulid,
#                                stratum=haul.dt.lim$stratum,
#                                year=haul.dt.lim$year,
#                                month=haul.dt.lim$month,
#                                regionfact=haul.dt.lim$regionfact,
#                                lat=haul.dt.lim$lat,
#                                lon=haul.dt.lim$lon,
#                                depth=haul.dt.lim$depth,
#                                bottemp=haul.dt.lim$bottemp,
#                                surftemp=haul.dt.lim$surftemp,
#                                SST.seasonal.mean=haul.dt.lim$SST.seasonal.mean,
#                                SBT.seasonal=haul.dt.lim$SBT.seasonal,
#                                presfit=haul.dt.lim$presfit,
#                                pred.pres=pred.fit, 
#                                pred.log.bio=pred.fit.bio,
#                                pred.total=pred.total,
#                                sppocean=sppocean_list[i])
# }
# 
# #pred.dt <- rbindlist(pred_list)
# #saveRDS(pred.dt, "Output/pred.dt.rds")
# 
# #saveRDS(pred.dt, "Output/yellowtail.pred.rds")

pred.dt <- readRDS("Output/pred.dt.rds")
pred.dt[,"spp":=capitalize(gsub("_.*$", "", sppocean))]

### Link with subarea
pred.dt.wsub <- merge(pred.dt, haul.dt2[,list(subarea=subarea, haulid=haulid)], by=c("haulid"))




### Observed centroid
cent.obs <- catch.wloc[spp %in% spp_lookup$spp,
                       list(lat.cent=weighted.mean(lat, w=wtcpue),
                            lat.cent.pres=weighted.mean(lat, w=presfit),
                            num.obs=sum(presfit)), 
                       by=list(spp, sppocean, subarea, year)]
setorder(cent.obs, year)


### Predicted centroid
cent.cm <- pred.dt.wsub[!(is.na(subarea)),list(lat.cent.cm =weighted.mean(lat, w=pred.total),
                    lat.cent.pres.cm=weighted.mean(lat, w=pred.pres)),
              by=list(spp, sppocean, subarea, year)]
setorder(cent.cm, year)




### Merge observed and predicted (more records when predicting when no observations of a species in that year)
cent_master <- merge(cent.obs, cent.cm, by=c("sppocean", "spp", "subarea", "year"), all.y=T)
cent_master[,"nyrs.obs":=length(unique(year[is.finite(num.obs)])), by=list(spp, subarea)]

# ### Centroid in previous year for obs and predicted
# cent_master[,"lat.cent.prev":=data.table::shift(lat.cent, n=1, type="lag"), by=list(sppocean, spp, subarea)]
# cent_master[,"lat.cent.cm.prev":=data.table::shift(lat.cent.cm, n=1, type="lag"), by=list(sppocean, spp, subarea)]
# 
# ### Annual difference
# cent_master[,"lat.cent.diff":= lat.cent - lat.cent.prev, by=list(sppocean, spp, subarea)]
# cent_master[,"lat.cent.cm.diff":=lat.cent.cm - lat.cent.cm.prev, by=list(sppocean, spp, subarea)]

### Centroid in next year for obs and predicted
cent_master[,"lat.cent.next":=data.table::shift(lat.cent, n=1, type="lead"), by=list(sppocean, spp, subarea)]
cent_master[,"lat.cent.cm.next":=data.table::shift(lat.cent.cm, n=1, type="lead"), by=list(sppocean, spp, subarea)]

### Annual difference
cent_master[,"lat.cent.diff":= lat.cent.next - lat.cent, by=list(sppocean, spp, subarea)]
cent_master[,"lat.cent.cm.diff":=lat.cent.cm.next - lat.cent.cm, by=list(sppocean, spp, subarea)]



### Observed - predicted annual diff
cent_master[,"annual.obsminuspred":=lat.cent.diff - lat.cent.cm.diff]
cent_master[,"sign.pred":=sign(lat.cent.cm.diff)]
cent_master[,"annual.bias":=annual.obsminuspred*sign.pred]

hist(cent_master$annual.bias)


#### Rate of change over time (only for species which were observed in at least 5 years in a subarea)
### 185 spp, subarea combinations
cent_lm <- cent_master[nyrs.obs > 5,j={
  print(paste0(spp, " ", subarea))
  t.dt <- .SD
  lm.obs <- lm(lat.cent ~ year)
  lm.pred <- lm(lat.cent.cm ~ year, t.dt)
  list(lm.obs.slope=summary(lm.obs)$coefficients[2,1], 
       lm.obs.p=summary(lm.obs)$coefficients[2,4],
       lm.obs.r2=summary(lm.obs)$r.squared,
       lm.pred.slope=summary(lm.pred)$coefficients[2,1],
       lm.pred.p=summary(lm.pred)$coefficients[2,4],
       lm.pred.r2=summary(lm.pred)$r.squared,
       nyrs.obs=sum(is.finite(t.dt$num.obs)),
       num.obs=sum(t.dt$num.obs, na.rm=T))
}, by=list(spp, subarea)]

cent_lm[,"obsminuspred":=ifelse(lm.pred.slope>0, lm.obs.slope - lm.pred.slope, -(lm.obs.slope-lm.pred.slope))]

# Limit species to those observed in more than 5 years
# Remove Canada and US Southeast (maybe keep those for just the fishing intensity analysis)
#cent_master_lim <- cent_master[nyrs.obs > 5 & !(subarea %in% c("Canada East Coast", "US Southeast and Gulf"))]
cent_master_lim <- cent_master[nyrs.obs > 5]


png("Figures/lagclim_null.png", height=5, width=5, units="in", res=300)
plot(lm.obs.slope ~ lm.pred.slope, cent_lm)
points(lm.obs.slope ~ lm.pred.slope, cent_lm[obsminuspred<0], col="blue", pch=10, cex=0.5)
abline(a=0, b=1, col="red")
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

png("Figures/BSB_obsonly.png", height=5, width=5, units="in", res=300)
plot(lat.cent ~ year, 
     cent.obs[year >=1968 & sppocean=="centropristis striata_Atl" & subarea=="US East Coast"], 
     type="o", ylab="Latitude (w=wtcpue)")
abline(h=mean(cent.obs[year >=1968 & sppocean=="centropristis striata_Atl" & subarea=="US East Coast"]$lat.cent), 
       col="darkgreen", lty=2, lwd=2)
legend("topleft", legend=c("mean centroid"), col="darkgreen", lty=2, lwd=2, bty="n")
dev.off()


png("Figures/BSB_obspred.png", height=5, width=5, units="in", res=300)
plot(lat.cent ~ year, 
     cent.obs[year >=1968 & sppocean=="centropristis striata_Atl" & subarea=="US East Coast"], 
     type="o", ylab="Latitude (w=wtcpue)")
points(lat.cent.cm ~ year,
       cent.cm[year>=1968 & sppocean=="centropristis striata_Atl" & subarea=="US East Coast"],
       type="o", col="gray", ylab="Latitude (w=predicted wtcpue)")
dev.off()

### chilepepper rockfish
png("Figures/chile_obspred.png", height=5, width=5, units="in", res=300)
plot(lat.cent.cm ~ year,
       cent.cm[year>=1968 & spp=="Sebastes goodei"],
       type="o", col="gray", ylab="Latitude (w=predicted wtcpue)")
points(lat.cent ~ year, 
     cent.obs[year >=1968 & spp=="Sebastes goodei"], 
     type="o", ylab="Latitude (w=wtcpue)")
dev.off()


save(cent_master_lim, cent_lm, file="Output/cent_out.RData")
