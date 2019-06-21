library(data.table)
library(stringr)

### RAM Legacy Stock Assessment Dabtase v4.4

load("Data/DBdata.RData", verbose=T)

### Extract U/Umsy time series 
colkeep <- c("stockid", "year", "UdivUmsypref", "FdivFmsy", "ERdivERmsy", "CPUE", "EFFORT", "TBbest", "TB", "SSB", "TC")
useries <- as.data.table(timeseries_values_views[,which(colnames(timeseries_values_views)%in%colkeep)])





### limit to just US or Canada
stock.dt <- as.data.table(stock)
stock.dt[,"area":=gsub( "-.*$", "", areaid)] #a dash, followed by any character (.) any number of times (*) until the end of the string ($)
stock.us <- stock.dt[area %in% c("USA")]
#stock.us[,"ak_subarea":=ifelse(region=="US Alaska", gsub("USA-NMFS-", "", areaid), NA)]
stock.us[,"ram_subarea":=ifelse(region%in% c("US Alaska", "US Southeast and Gulf"), 
                                gsub("USA-NMFS-", "", areaid), NA)]
stock.us[,"subarea":=ifelse(is.na(ram_subarea), region, 
                                 ifelse(ram_subarea=="BS", "EBS", 
                                        ifelse(ram_subarea=="AI", "AI",
                                            ifelse(ram_subarea=="GA", "GOA",
                                               ifelse(ram_subarea=="EBSAI", "EBS",
                                                      ifelse(ram_subarea=="EBS", "EBS",
                                                             ifelse(ram_subarea=="EBSAIGA", "EBS", #for sablefish
                                                                ifelse(ram_subarea=="BSAI", "EBS", 
                                                                       ifelse(ram_subarea=="SATLC", "SEUS",
                                                                              ifelse(ram_subarea=="SATL", "SEUS", 
                                                                                     ifelse(ram_subarea=="ATL", "SEUS",
                                                                                        ifelse(ram_subarea=="GM", "GMex", NA))))))))))))]

#saveRDS(stock.us, "Data/ram.stock.us.rds")

### Years in survey
yrs.surv <- data.table(region=c("US East Coast", "US Alaska", "US West Coast", "US Southeast and Gulf", "Canada East Coast"),
                       min.survyr=c(1968, 1981, 1981, 1981, 1971),
                       max.survyr=c(2016, 2016, 2016, 2016, 2016))

### If including Canada, Scotian Shelf = 1970-2011, So Gulf= 1971-2009, Newfoundland=1995-2011
### Only 4 species have U time series

### Merge with stock table
useries2 <- merge(useries, stock.us[!(is.na(subarea))], by=c("stockid"))
useries3 <- merge(useries2[!(is.na(UdivUmsypref)),], yrs.surv, by=c("region"))

### Total biomass by species (add within a stock region)
useries3[,"num.stocks":=length(unique(stockid)), by=list(scientificname, subarea, region)]
useries3[,"TB_spp":=ifelse(num.stocks>1, sum(TB, na.rm=T), TB), 
         by=list(scientificname, region, year)]
useries3[,"SSB_spp":=ifelse(num.stocks>1, sum(SSB, na.rm=T), SSB), 
         by=list(scientificname, region, year)]
useries3[,"frac.TB":=TB/TB_spp]
useries3[,"frac.SSB":=SSB/SSB_spp]
useries3[,"frac.stock":=ifelse(TB_spp==0 | is.na(TB_spp), frac.SSB, frac.TB)]

useries_spp <- useries3[,
                        list(U.Umsy.spp =ifelse(num.stocks>1,
                                                sum(UdivUmsypref*frac.stock, na.rm=T),
                                                UdivUmsypref)), 
                        by=list(scientificname, subarea, region, year, min.survyr, num.stocks)]

# 
# ### species names with U/Umsy ts
# ustock.yrs <- useries3[year >= min.survyr,
#                         list(num.yrs=length(UdivUmsypref),
#                               min.yr=min(year),
#                               max.yr=max(year),
#                               meanU.Umsy=mean(UdivUmsypref, na.rm=T),
#                               medianU.Umsy=median(UdivUmsypref, na.rm=T),
#                               type_F=sum(UdivUmsypref==FdivFmsy),
#                               type_ER=sum(UdivUmsypref==ERdivERmsy),
#                               num.yrs.overage=sum(UdivUmsypref>1)), 
#                         by=list(region, area, areaid, stockid, scientificname, commonname)]

uspp.yrs <- useries_spp[year >= min.survyr,
                       list(num.yrs=length(unique(year)),
                            meanU.Umsy=mean(U.Umsy.spp, na.rm=T),
                            num.yrs.overage=length(unique(year[U.Umsy.spp>1]))), 
                       by=list(subarea, region, scientificname, num.stocks)]

# ustock.yrs[,"frac.yrs.overage":=num.yrs.overage/num.yrs]
uspp.yrs[,"frac.yrs.overage":=num.yrs.overage/num.yrs]

# setorder(ustock.yrs, scientificname)
setorder(uspp.yrs, scientificname)

par(mfrow=c(2,2))
hist(uspp.yrs$frac.yrs.overage)
hist(uspp.yrs$meanU.Umsy)

plot(meanU.Umsy ~ frac.yrs.overage, uspp.yrs)


# metadata.dt <- as.data.table(metadata[,c("assessid", "stockid", "region")])
# metadata.nam <- metadata.dt[region %in% c("US East Coast", "US Alaska", "US West Coast", "US Southeast and Gulf")]
# #extract stock from assessid
# metadata.nam[,"stock":=str_split_fixed(assessid, "-", n=5)[,2]]
# metadata.nam[,"stock_match":=ifelse(stockid==stock,1, 0)]
# 
# useries_wmeta <- merge(useries.yrs, metadata.dt, by=c("stockid", "region"))


save(useries3, uspp.yrs, file="Output/RAM_U_out.RData")
