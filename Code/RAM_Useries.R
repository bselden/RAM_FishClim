library(data.table)
library(stringr)

### RAM Legacy Stock Assessment Dabtase v4.4

load("Data/DBdata.RData", verbose=T)

### Extract U/Umsy time series 
colkeep <- c("stockid", "year", "UdivUmsypref", "FdivFmsy", "ERdivERmsy", "CPUE", "EFFORT", "TBbest", "TB", "SSB")
useries <- as.data.table(timeseries_values_views[,which(colnames(timeseries_values_views)%in%colkeep)])





### limit to just US or Canada
stock.dt <- as.data.table(stock)
stock.dt[,"area":=gsub( "-.*$", "", areaid)] #a dash, followed by any character (.) any number of times (*) until the end of the string ($)
stock.us <- stock.dt[area %in% c("USA")]
stock.us[,"ak_subarea":=ifelse(region=="US Alaska", gsub("USA-NMFS-", "", areaid), NA)]
stock.us[,"subarea":=ifelse(is.na(ak_subarea), region, 
                                 ifelse(ak_subarea=="BS", "BSAI", 
                                        ifelse(ak_subarea=="GA", "GOA",
                                               ifelse(ak_subarea=="EBSAI", "BSAI",
                                                      ifelse(ak_subarea=="EBS", "BSAI",
                                                             ifelse(ak_subarea=="BSAI", "BSAI", NA))))))]

### Years in survey
yrs.surv <- data.table(region=c("US East Coast", "US Alaska", "US West Coast", "US Southeast and Gulf"),
                       min.survyr=c(1968, 1981, 1981, 1981),
                       max.survyr=c(2016, 2016, 2016, 2016))

### Merge with stock table
useries2 <- merge(useries[!(is.na(UdivUmsypref)),], stock.us, by=c("stockid"))
useries3 <- merge(useries2, yrs.surv, by=c("region"))

### Total biomass by species (add within a stock region)
useries3[,"TB_spp":=sum(TBbest), by=list(scientificname, region, year)]
useries3[,"SSB_spp":=sum(SSB), by=list(scientificname, region, year)]
useries3[,"frac.TB":=TBbest/TB_spp]
useries3[,"frac.SSB":=SSB/SSB_spp]
useries3[,"frac.stock":=ifelse(is.finite(TB_spp), frac.TB, frac.SSB)]
useries3[,"U.Umsy.spp":=sum(UdivUmsypref*frac.stock), by=list(scientificname, region, year)]


### species names with U/Umsy ts
ustock.yrs <- useries3[year >= min.survyr,
                        list(num.yrs=length(UdivUmsypref),
                              min.yr=min(year),
                              max.yr=max(year),
                              meanU.Umsy=mean(UdivUmsypref, na.rm=T),
                              medianU.Umsy=median(UdivUmsypref, na.rm=T),
                              type_F=sum(UdivUmsypref==FdivFmsy),
                              type_ER=sum(UdivUmsypref==ERdivERmsy),
                              num.yrs.overage=sum(UdivUmsypref>1)), 
                        by=list(region, area, areaid, stockid, scientificname, commonname)]

uspp.yrs <- useries3[year >= min.survyr,
                       list(num.yrs=length(unique(year)),
                            num.stocks=length(unique(stockid)),
                            meanU.Umsy=mean(U.Umsy.spp, na.rm=T),
                            num.yrs.overage=length(unique(year[U.Umsy.spp>1]))), 
                       by=list(region, scientificname, commonname)]

ustock.yrs[,"frac.yrs.overage":=num.yrs.overage/num.yrs]
uspp.yrs[,"frac.yrs.overage":=num.yrs.overage/num.yrs]

setorder(ustock.yrs, scientificname)
setorder(uspp.yrs, scientificname)

par(mfrow=c(2,2))
hist(ustock.yrs$frac.yrs.overage)
hist(uspp.yrs$frac.yrs.overage)
hist(ustock.yrs$meanU.Umsy)
hist(uspp.yrs$meanU.Umsy)

plot(meanU.Umsy ~ frac.yrs.overage, uspp.yrs)


# metadata.dt <- as.data.table(metadata[,c("assessid", "stockid", "region")])
# metadata.nam <- metadata.dt[region %in% c("US East Coast", "US Alaska", "US West Coast", "US Southeast and Gulf")]
# #extract stock from assessid
# metadata.nam[,"stock":=str_split_fixed(assessid, "-", n=5)[,2]]
# metadata.nam[,"stock_match":=ifelse(stockid==stock,1, 0)]
# 
# useries_wmeta <- merge(useries.yrs, metadata.dt, by=c("stockid", "region"))
