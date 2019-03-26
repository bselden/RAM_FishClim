library(data.table)

vtr <- readRDS("Data/VTR/CaS.commercial_all_port_land_wvessel_info.rds")
vtr[,"portlnd1_state":=gsub( ".*_", "", portlnd1)]
vtr[,"dec_port_state":=gsub( ".*_", "", dec_port)]

vtr[,"state":=ifelse(portlnd1_state=="NA", dec_port_state, portlnd1_state)]


### Read in statistical areas for each stock in RAM (from https://www.nefsc.noaa.gov/sos/)
stat_stock <- as.data.table(read.csv("Data/StatusFishResStatAreas.csv"))

### VTR RAM
vtr_ram <- merge(vtr, stat_stock, by.x=c("sppcode", "cnemarea"), by.y=c("VTR_sppcode", "StatArea"))

### Landings by stock by state
vtr_stock_land <- vtr_ram[,list(qtykept=sum(qtykept, na.rm=T)), by=list(sppcode, RAM_stockid, state, year)]
vtr_stock_land[,"total.qtykept":=sum(qtykept), by=list(sppcode, state, year)]
vtr_stock_land[,"frac.stock":=qtykept/total.qtykept]

### Number of years exploiting each stock
vtr_stock_land[,"num.yrs.stock":=length(unique(year)), by=list(RAM_stockid, sppcode, state)]
vtr_stock_land[,"num.yrs.spp":=length(unique(year)), by=list(sppcode, state)]



### Those with multiple stocks: COD, FLBB, FLSD, FLYT, HADD, LOB, MONK, SHAK

### Main stock exploited by state
vtr_main_stock <- vtr_ram[,list(stock.qtykept=sum(qtykept, na.rm=T),
                                stock.yrs=length(unique(year))), by=list(spp, sppcode, RAM_stockid, state)]
vtr_main_stock[,"total.qtykept":=sum(stock.qtykept), by=list(sppcode, state)]
vtr_main_stock[,"frac.stock":=stock.qtykept/total.qtykept]
saveRDS(vtr_main_stock[frac.stock>0.15], "Output/vtr_main_stock.rds")


### Limit vtr records to main stocks
vtr_ram_lim <- merge(vtr_ram, vtr_main_stock[frac.stock>0.15], by=c("sppcode", "RAM_stockid", "state"))


#### Model distribution of landings across latitude by state within a stockarea
land.lat.state <- vtr_ram_lim[state %in% c("me", "ma", "ny", "ri", "va", "nc", "nj", "ct", "md", "nh", "de") & year >1995,j={
  print(paste0(state, "_", sppcode, "_", RAM_stockid))
  t.dt <- .SD
  sub <- t.dt[is.finite(qtykept)]
  total.qty=sum(as.numeric(sub$qtykept),na.rm=T)
  d <- density(x=sub$declat, weights=sub$qtykept/total.qty, bw=0.5, from=30.5, to=47.5) #512 values
  d2 <- density(x=sub$declon, weights=sub$qtykept/total.qty, bw=0.5, from=-76, to=-65) #512 values
  list(lat=d$x, dens.qty=d$y, lon=d2$x, lon.dens.qty=d2$y)
}, by=list(state, spp, sppcode, RAM_stockid)]


saveRDS(land.lat.state, "Output/land.lat.state.vtr.ramstock.rds")
