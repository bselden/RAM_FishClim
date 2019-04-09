library(data.table)

### RAM Legacy Stock Assessment Dabtase v4.4

load("Data/DBdata.RData", verbose=T)

### Extract U/Umsy time series 
colkeep <- c("stockid", "year", "UdivUmsypref", "FdivFmsy", "ERdivERmsy")
useries <- as.data.table(timeseries_values_views[,which(colnames(timeseries_values_views)%in%colkeep)])


### limit to just US or Canada
stock.dt <- as.data.table(stock)
stock.dt[,"area":=gsub( "-.*$", "", areaid)] #a dash, followed by any character (.) any number of times (*) until the end of the string ($)
stock.us <- stock.dt[area %in% c("USA")]

### Years in survey
yrs.surv <- data.table(region=c("US East Coast", "US Alaska", "US West Coast", "US Southeast and Gulf"),
                       min.survyr=c(1968, 1981, 1981, 1981),
                       max.survyr=c(2016, 2016, 2016, 2016))

### Merge with stock table
useries2 <- merge(useries[!(is.na(UdivUmsypref)),], stock.us, by=c("stockid"))
useries3 <- merge(useries2, yrs.surv, by=c("region"))


### species names with U/Umsy ts
useries.yrs <- useries3[year >= min.survyr,
                        list(num.yrs=length(UdivUmsypref),
                              min.yr=min(year),
                              max.yr=max(year),
                              meanU.Umsy=mean(UdivUmsypref, na.rm=T),
                              medianU.Umsy=median(UdivUmsypref, na.rm=T),
                              type_F=sum(UdivUmsypref==FdivFmsy),
                              type_ER=sum(UdivUmsypref==ERdivERmsy),
                              num.yrs.overage=sum(UdivUmsypref>1)), 
                        by=list(region, area, areaid, stockid, scientificname, commonname)]
useries.yrs[,"frac.yrs.overage":=num.yrs.overage/num.yrs]

hist(useries.yrs$frac.yrs.overage)
hist(useries.yrs$meanU.Umsy)

plot(meanU.Umsy ~ frac.yrs.overage, useries.yrs)
