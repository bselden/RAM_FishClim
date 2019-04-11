
library(data.table)
library(raster)
library(Hmisc)

load("Data/hauls_catch_Dec2017.RData", verbose=T)

### Species catch by haul
catch.dt <- as.data.table(dat)
haul.dt <- as.data.table(hauls)

haul.region <- data.table(region=c("AFSC_Aleutians", "SEFSC_GOMex", "AFSC_GOA", "AFSC_WCTri", "VIMS_NEAMAP", "NEFSC_NEUS", "SCDNR_SEUS", "NWFSC_WCAnn", "AFSC_EBS"),
                          ram_area=c("AI", ""))

### RAM Stock Boundaries from Chris Free
ram_shapes <- list.files("GIS_Data/ramldb_boundaries/", pattern=".shp")

ram_shapes_dt <- data.table(shape=ram_shapes)

ram_shapes_dt[,"stockid" :=str_split_fixed(ram_shapes, "-", n=5)[,2]]

metadata.wshape <- merge(useries_wmeta, ram_shapes_dt, by="stockid", all.x=T)
