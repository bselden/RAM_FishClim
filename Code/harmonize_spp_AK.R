library(data.table)

### Species codes
spp <- as.data.table(read.csv("Data/SpatialLand/Haynie_AKData/ak.master.spp.full.csv"))

### RAM Legacy Stock Assessment Dabtase v4.4
load("Data/DBdata.RData", verbose=T)
stock.dt <- as.data.table(subset(stock, region=="US Alaska"))

### species names that have been updated
stock.dt[,"spp":=ifelse(scientificname=="Theragra chalcogramma", "Gadus chalcogrammus",
                        ifelse(scientificname=="Lepidopsetta bilineata", "Lepidopsetta spp",
                               ifelse(scientificname=="Paralithodes camtschaticus", "Atheresthes evermanni", scientificname)))]


### RAM spp
spp.ram <- merge(spp, stock.dt[, list(spp=spp, scientificname=scientificname, areaid=areaid)], 
                 by="spp", all.y=T)

saveRDS(spp.ram, "Data/AK_spp_obs_RAM.rds")
