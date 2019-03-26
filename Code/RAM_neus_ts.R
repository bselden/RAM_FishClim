### Load libraries
library(data.table)


### Load data
### RAM stocks with trawl survey data
ramspp <- read.csv("Data/ramspp_intrawl.csv")

### RAM Time Series
ram_ts <- as.data.table(read.csv("Data/RLSADB_v3.0_AssessTS.csv"))

### Add in Mackerel and Yellowtail from GB from v2.5
ram_ts_2.5 <- as.data.table(read.csv("Data/ExtraStocksRAM2.5.csv")) 

ram_ts_all <- rbind(ram_ts, ram_ts_2.5, fill=T) #2.5 didn't have the mgttouse columns, so set fill=T

### Limit to just North American Species
ram_ts2 <- merge(ram_ts_all, ramspp, by=c("stockid"))

### Number of years each stock has total biomass,  spawning biomass, catch, U, ER
yrs.ram.bio <- ram_ts2[,list(yrs.TB=length(unique(year[!(is.na(TB))])),
                             yrs.SSB=length(unique(year[!(is.na(SSB))])),
                             yrs.C=length(unique(year[!(is.na(Ctouse))])),
                             yrs.U=length(unique(year[!(is.na(Utouse))])),
                             yrs.ER=length(unique(year[!(is.na(ER))])),
                             min.yr=min(year),
                             max.yr=max(year),
                             min.yr.TB=ifelse(sum(!(is.na(TB)))>0, min(year[!(is.na(TB))]), as.integer(NA)),
                             min.yr.SSB=ifelse(sum(!(is.na(SSB)))>0, min(year[!(is.na(SSB))]), as.integer(NA))),
                       by=list(stockid, species, region)]

### Limit to just US East Coast (45 stocks)
ram.spp.lim <- yrs.ram.bio[region%in%c("US East Coast")]

### 6 Stocks with no biomass estimate
### most recent stock assessment also does not have a biomass estimate (Silver hake, Winter flounder, ocean pout)
### Red crab is data poor, and blue crab is in Chesapeake Bay only
ram.nobio <- ram.spp.lim[yrs.TB==0 & yrs.SSB==0]

ram_ts_lim <- ram_ts2[stockid %in% ram.spp.lim$stockid]


###########################
### Load RAM stat area survey data
### Output from RAM_neus_trawlData_to_statarea.R
surv <- readRDS("Output/ram.neus.statarea.rds")


### Merge with ram ts
surv_ram <- merge(surv, ram_ts_lim[,list(stockid=stockid, TB=TB, SSB=SSB, year=year)],
                  by.x=c("RAM_stockid", "year"), by.y=c("stockid", "year"))

### Estimate absolute biomass by multiplying by TB or SSB
surv_ram[,"abs.bio":=ifelse(!(is.na(TB)), TB*frac.swept.bio, SSB*frac.swept.bio)]








###################################
### Load Catch data by lat bin
### Output from SpatialLandings_StatArea_Stock.R
catch <- readRDS("Output/total.land.latbin.stock.rds")



