### Examine effect of climate and fisheries during VTR period where also have effort data

library(data.table)
library(stringr)
library(rgdal)
library(raster)

vtr <- readRDS("Data/VTR/CaS.commercial_all_port_land_wvessel_info.rds")
vtr.spp <- as.data.table(read.csv("Data/VTR_sppcodes_compiled_NEW.csv"))

### Summarize landings by species, year and cnemarea
### Landings in pounds
land_statarea <- vtr[,list(land=sum(qtykept, na.rm=T),
                           num.trips=length(unique(anon.id)),
                           num.vessels=length(unique(VP_anon))),
                     by=list(sppcode, year, cnemarea)]

statarea_vtr_meta <- vtr[,list(declat=mean(declat),
                               declon=mean(declon)),
                         by=list(cnemarea)]
lat.bins <- seq(34,45)
lat.labs <- seq(34.5, 44.5)
statarea_vtr_meta[,"lat.bin":=cut(declat, lat.bins, lat.labs)]
statarea_vtr_meta[,"lat.num":=as.numeric(as.character(lat.bin))]

land_statarea_loc <- merge(land_statarea, statarea_vtr_meta, by=c("cnemarea"))

land_lat <- land_statarea_loc[,list(land=sum(land),
                                    num.trips=sum(num.trips),
                                    num.vessels=sum(num.vessels)),
                              by=list(sppcode, year, lat.bin, lat.num)]
setorder(land_lat, lat.num)

yrs.vtr <- seq(1996,2015)
library(RColorBrewer)
cols.vtr <- colorRampPalette(rev(brewer.pal(n=6, "Spectral")))(length(yrs.vtr))
plot(num.trips ~ lat.num, land_lat[sppcode=="BSB"], col="white")
for(i in 1:length(yrs.vtr)){
  points(num.trips ~lat.num, land_lat[sppcode=="BSB" & year==yrs.vtr[i]], 
         col=cols.vtr[i], type="l")
}

