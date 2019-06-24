library(data.table)
library(stringr)

### RAM Legacy Stock Assessment Dabtase v4.4

load("Data/DBdata.RData", verbose=T)

### Extract U/Umsy time series 
colkeep <- c("stockid", "year", "UdivUmsypref", "FdivFmsy", "ERdivERmsy", "F", "ER", "CdivMSY",
             "CPUE", "EFFORT", "TBbest", "TB", "SSB", "TC")
useries <- as.data.table(timeseries_values_views[,which(colnames(timeseries_values_views)%in%colkeep)])





### limit to just US or Canada
stock.dt <- as.data.table(stock)
stock.dt[,"area":=gsub( "-.*$", "", areaid)] #a dash, followed by any character (.) any number of times (*) until the end of the string ($)
stock.us <- stock.dt[area %in% c("USA", "Canada")]
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

useries3 <- merge(useries2, yrs.surv, by=c("region"))

### Total biomass by species (add within a stock region)
useries3[,"num.stocks":=length(unique(stockid)), by=list(scientificname, subarea, region)]
useries3[,"TB_spp":=ifelse(num.stocks>1, sum(TB, na.rm=T), TB), 
         by=list(scientificname, region, year)]
useries3[,"SSB_spp":=ifelse(num.stocks>1, sum(SSB, na.rm=T), SSB), 
         by=list(scientificname, region, year)]
useries3[,"Bio":=ifelse(is.na(TB), SSB, TB)]
useries3[,"Bio_spp":=ifelse(TB_spp==0 & SSB_spp==0, NA, ifelse(TB_spp==0 & SSB_spp>0, SSB_spp, TB_spp))]
useries3[,"frac.stock":=Bio/Bio_spp]

### dummy variables whether no values for U/Umsy or ER in any stocks
useries3[,"U_any":=ifelse(sum(!(is.na(UdivUmsypref)))>0, 1, 0), by=list(scientificname, subarea, year)]
useries3[,"ER_any":=ifelse(sum(!(is.na(ER)))>0, 1, 0), by=list(scientificname, subarea, year)]
useries3[,"F_any":=ifelse(sum(!(is.na(F)))>0, 1, 0), by=list(scientificname, subarea, year)]



useries_spp <- useries3[year>=min.survyr & !(is.na(frac.stock)),
                        list(U.Umsy.spp =ifelse(num.stocks>1 & U_any==1,
                                                sum(UdivUmsypref*frac.stock, na.rm=T),
                                                ifelse(num.stocks>1 &U_any==0, as.numeric(NA), UdivUmsypref)),
                             F.spp =ifelse(num.stocks>1 & F_any==1,
                                                sum(F*frac.stock, na.rm=T),
                                                ifelse(num.stocks>1 &F_any==0, as.numeric(NA), F)),
                             ER.spp=ifelse(num.stocks>1 & ER_any==1,
                                                   sum(ER*frac.stock, na.rm=T),
                                                   ifelse(num.stocks>1 & ER_any==0, as.numeric(NA), ER))), 
                        by=list(scientificname, subarea, region, year, min.survyr, num.stocks, U_any, F_any, ER_any)]


useries_spp[,"overage":=ifelse(is.finite(U.Umsy.spp) & U.Umsy.spp>1, 1, 
                               ifelse(is.finite(U.Umsy.spp) & U.Umsy.spp<=1, 0, as.numeric(NA)))]

uspp.yrs <- useries_spp[,
                       list(num.yrs=length(unique(year[is.finite(U.Umsy.spp)])),
                            meanU.Umsy=mean(U.Umsy.spp, na.rm=T),
                            meanER=mean(ER.spp, na.rm=T),
                            meanF=mean(F.spp, na.rm=T),
                            num.yrs.overage=sum(overage[U_any==1])), 
                       by=list(subarea, region, scientificname, num.stocks)]

# Reassign num years overage if no U/Umsy years
uspp.yrs[,"num.yrs.overage":=ifelse(num.yrs.overage==0 & num.yrs==0, NA, num.yrs.overage)]

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


save(useries_spp, uspp.yrs, file="Output/RAM_U_out.RData")
