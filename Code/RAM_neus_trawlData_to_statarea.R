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
### Output from ExtractHaulsStatArea_neus
ram.hauls <- readRDS("Output/hauls_ram_neus.rds")
ram.hauls.neus <- ram.hauls[region=="NEFSC_NEUS"]


### Merge with stock haul data
ram.hauls.neus2 <- merge(dat.neus.z, ram.hauls.neus, by=c("haulid", "region", "lat", "lon", "spp", "year"))


# =======================
# = Get expanded biomass by statistical area =
# =======================
# Multiply mean wtcpue in a stat area by (stat area area)/area surveyed per haul

### Mean biomass by statistical area
ram.statarea <- ram.hauls.neus2[year>=1968,list(pres2=mean(pres2),
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
                                              lat=mean(lat),
                                              lon=mean(lon)),
                              by=list(StatArea, area, RAM_stockid, spp, year)]


### Area per tow in km2 (0.01 nm2)=0.0343 km2 based on google's conversion
areapertow <- 0.0343 

# Swept area biomass (mt) for statistical area (kg=0.001mt)
ram.statarea[,"swept.bio.statarea.mt":=wtcpue*(area/areapertow)*0.001]

### Get total swept bio in a year
ram.statarea[,"total.swept.bio.mt":=sum(swept.bio.statarea.mt), by=list(RAM_stockid, spp, year)]
ram.statarea[,"frac.swept.bio":=swept.bio.statarea.mt / total.swept.bio.mt]


### Offshore statareas
offshore <- c(543,542,541,534,533,
              623,627,633,637)
faroff <- c(624,628,634,638,629,639)

ram.statarea[StatArea %in% offshore & pres2>0, unique(StatArea)] #only in 623 and 627
ram.statarea[StatArea %in% faroff & pres2>0, unique(StatArea)] #no records

surv.StatAreas <- ram.statarea[,list(num.stocks=length(unique(RAM_stockid))), by=list(StatArea)]
setorder(surv.StatAreas, StatArea)


saveRDS(ram.statarea, "Output/ram.neus.statarea.rds")



