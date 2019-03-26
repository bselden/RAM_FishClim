### Boosted regression trees

library(data.table)
library(dismo)

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



####################
### Add zeros for each species where haul conducted but not observed



###################
### Merge with haul location
catch.wloc <- merge(catch.dt2, 
                    haul.dt2[,list(haulid=haulid, lat=lat, lon=lon, regionfact=regionfact, year=year, month=month)], 
                    by=c("haulid"))