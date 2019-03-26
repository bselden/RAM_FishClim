#### Objective: Summarize trawlData by 1 degree latitude bin using Jim's data with SODA attributes
#### Do separately for each stock of a species in RAM

### Load packages
library(data.table)
library(Hmisc)

### Load data from Jim
load("Data/hauls_catch_Dec2017.RData", verbose=T)

### Create new column for spp and ocean
dat.dt <- as.data.table(dat)
dat.dt[,c("spp", "ocean"):=tstrsplit(sppocean, "_")]
dat.dt[,c("spp"):=capitalize(spp)]





### Restrict hauls to just neus
hauls.neus <- as.data.table(hauls)[regionfact=="NEFSC_NEUS"]



### Restrict catch data to just neus
dat.neus <- dat.dt[haulid %in% hauls.neus$haulid]

### Create new column for year based on first 4 characters in haulid
dat.neus[,"year":=as.numeric(substr(haulid, 1,4))]

### Restrict to species observed in at least 40 years
spp.yrs <- dat.neus[,list(num.yrs=length(unique(year))), by=list(spp)]
spp.yrs.40 <- spp.yrs[num.yrs>=40]

# =======================
# = Add Zeros =
# =======================

### Add zeros where haul conducted by species not observed
source("Code/add_zeros_fn.R")
dat.neus.z <- add.zeros.sp.bioonly(bio.dt=dat.neus[spp %in% spp.yrs.40$spp], haul.dt=hauls.neus)


### Aggregate environmental and species data to latitude bin by year
lat.brks <- seq(30,46, by=1)
lat.labs <- seq(30.5, 45.5, by=1) #these correspond approximately to the statarea coordinates (particularly in MAB where regular grid)

dat.neus.z[,"lat.bin":=cut(lat, breaks=lat.brks, labels=lat.labs)]

# =======================
# = Assign to RAM Stock =
# =======================
ram.hauls <- readRDS("Output/hauls_ram_neus.rds")
ram.hauls.neus <- ram.hauls[region=="NEFSC_NEUS"]


### Merge with stock haul data
ram.hauls.neus2 <- merge(dat.neus.z, ram.hauls.neus, by=c("haulid", "region", "lat", "lon", "spp"))

# =======================
# = Summarize to Lat Bin within Stock =
# =======================
ram.latbin.fall <- ram.hauls.neus2[month %in% c(9,10,11),list(pres2=mean(pres2),
                                                         wtcpue=mean(wtcpue),
                                                         wtcpuenal=mean(wtcpuenal),
                                                         SBT.actual=mean(SBT.actual),
                                                         avg.SBT.min=mean(SBT.min),
                                                         avg.SBT.max=mean(SBT.max),
                                                         SST.actual=mean(SST.actual),
                                                         avg.SST.min=mean(SST.min),
                                                         avg.SST.max=mean(SST.max),
                                                         depth=mean(depth, na.rm=T),
                                                         num.hauls=length(unique(haulid)),
                                                         season="fall"),
                              by=list(lat.bin, RAM_stockid, spp, year)]
setorder(ram.latbin.fall, spp, year, lat.bin)

ram.latbin.spring <- ram.hauls.neus2[month %in% c(3,4,5),list(pres2=mean(pres2),
                                                         wtcpue=mean(wtcpue),
                                                         wtcpuenal=mean(wtcpuenal),
                                                         SBT.actual=mean(SBT.actual),
                                                         avg.SBT.min=mean(SBT.min),
                                                         avg.SBT.max=mean(SBT.max),
                                                         SST.actual=mean(SST.actual),
                                                         avg.SST.min=mean(SST.min),
                                                         avg.SST.max=mean(SST.max),
                                                         depth=mean(depth, na.rm=T),
                                                         num.hauls=length(unique(haulid)),
                                                         season="spring"),
                                by=list(lat.bin, RAM_stockid, spp, year)]
setorder(ram.latbin.spring, spp, year, lat.bin)


ram.latbin <- ram.hauls.neus2[year>=1968,list(pres2=mean(pres2),
                                         wtcpue=mean(wtcpue),
                                         wtcpuenal=mean(wtcpuenal),
                                         SBT.actual=mean(SBT.actual),
                                         avg.SBT.min=mean(SBT.min),
                                         avg.SBT.max=mean(SBT.max),
                                         SST.actual=mean(SST.actual),
                                         avg.SST.min=mean(SST.min),
                                         avg.SST.max=mean(SST.max),
                                         depth=mean(depth, na.rm=T),
                                         num.hauls=length(unique(haulid))),
                         by=list(lat.bin, RAM_stockid, spp, year)]
setorder(ram.latbin, spp, year, lat.bin)



#### Limit to latitude bins with > 15 hauls in a year
ram.latbin2 <- ram.latbin[num.hauls>15]


saveRDS(ram.latbin2, "Output/ram.neus.latbin2.rds")



