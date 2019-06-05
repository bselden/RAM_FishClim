library(data.table)

### Summarized survey data to lat bin for NEUS
survey_latbin <- readRDS("Data/dat.latbin2.rds")
survey_latbin[,"lat.num":=as.numeric(as.character(lat.bin))]

### Temperature trends at lat bin
temp.latbin <- survey_latbin[lat.num>34.5 & year !=2015,
                             list(SBT.actual=mean(SBT.actual, na.rm=T),
                                  SST.actual=mean(SST.actual, na.rm=T)),
                             by=list(year)]


### Catch by latitude bin
### Adjusted from original Sean Lucey database for missing southern states
land <- readRDS("Data/total.land.latbin.rds")
land[,"lat.num":=as.numeric(as.character(lat.bin))]

png("Figures/lat_bin_demo.png", height=5, width=5, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(wtcpuenal ~ lat.num, survey_latbin[spp=="Paralichthys dentatus" & year==1999],
     xlim=c(34.5, 43.5), type="o", 
     xlab="latitude bin", ylab="log(biomass) (kg/tow)", main="Summer flounder")
points(wtcpuenal ~ lat.num, survey_latbin[spp=="Paralichthys dentatus" & year==2000], 
       col="blue", type="o")
legend("topright", legend=c(1999, 2000), col=c("black", "blue"), lty=1, bty="n")

plot(wtcpuenal ~ lat.num, survey_latbin[spp=="Urophycis chuss" & year==1990],
     xlim=c(34.5, 44.5), type="o", col="blue",
     xlab="latitude bin", ylab="log(biomass) (kg/tow)", main="Red hake")
points(wtcpuenal ~ lat.num, survey_latbin[spp=="Urophycis chuss" & year==1989], 
       col="black", type="o")
legend("topleft", legend=c(1989, 1990), col=c("black", "blue"), lty=1, bty="n")


plot(tl_mt ~ lat.num, land[spp=="Paralichthys dentatus" & year==1999],
     xlim=c(34.5, 44.5), type="o", col="black",
     xlab="latitude bin", ylab="landings (mt)")
points(tl_mt  ~ lat.num, land[spp=="Paralichthys dentatus" & year==2000], 
       col="blue", type="o")

plot(tl_mt ~ lat.num, land[spp=="Urophycis chuss" & year==1989],
     xlim=c(34.5, 44.5), type="o", col="black",
     xlab="latitude bin", ylab="landings (mt)")
points(tl_mt  ~ lat.num, land[spp=="Urophycis chuss" & year==1990], 
       col="blue", type="o")
dev.off()




### Black Sea Bass Lat Bin Trends
png("Figures/BSB_Latbin.png", height=7, width=7, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(wtcpuenal ~ lat.num, survey_latbin[spp=="Centropristis striata" & year==2000],
     xlim=c(34.5, 43.5), type="o", col="blue",
     xlab="latitude bin", ylab="log(biomass) (kg/tow)", main="Biomass")
points(wtcpuenal ~ lat.num, survey_latbin[spp=="Centropristis striata" & year==1999], 
       col="black", type="o")
points(wtcpuenal ~ lat.num, survey_latbin[spp=="Centropristis striata" & year==2001], 
       col="red", type="o")
legend("topright", legend=c(1999, 2000,2001), col=c("black", "blue", "red"), lty=1, bty="n")

plot(tl_mt ~ lat.num, land[spp=="Centropristis striata" & year==1999],
     xlim=c(34.5, 44.5), type="o", col="black",
     xlab="latitude bin", ylab="landings (mt)", main="Landings")
points(tl_mt  ~ lat.num, land[spp=="Centropristis striata" & year==2000], 
       col="blue", type="o")


yrs.90s <- seq(1996, 2001)
yrs.80s <- seq(1985,1990)

library(RColorBrewer)
cols <- rev(brewer.pal(n=6, "Spectral"))

setorder(survey_latbin, lat.num)

plot(SBT.actual ~ lat.num, survey_latbin[spp=="Centropristis striata"], 
     col="white", xlim=c(35,43), main="Sea Bottom Temperature",
     xlab="latitude bin", ylab="Sea Bottom Temperature (C)")
legend("topright", legend=yrs.90s, col=cols, lty=1, bty="n")
for(i in 1:length(yrs.90s)){
  points(SBT.actual ~ lat.num, 
         survey_latbin[spp=="Centropristis striata"& year==yrs.90s[i]], 
         col=cols[i], type="l")
}

plot(SST.actual ~ lat.num, survey_latbin[spp=="Centropristis striata"], 
     col="white", xlim=c(35,43), main="Sea Surface Temperature",
     xlab="latitude bin", ylab="Sea Surface Temperature (C)")
legend("topright", legend=yrs.90s, col=cols, lty=1, bty="n")
for(i in 1:length(yrs.90s)){
  points(SST.actual ~ lat.num, 
         survey_latbin[spp=="Centropristis striata"& year==yrs.90s[i]], 
         col=cols[i], type="l")
}

dev.off()

############
### Centroid of biomass with binned latitude
### Confirm that this is annual mean for biomass, and temperature
cent_bin <- survey_latbin[wtcpue>0 & lat.num>33.5,list(cent.bin=weighted.mean(lat.num, w=wtcpue)),
                          by=list(spp, year)]

setorder(cent_bin, year)
cent_bin[spp=="Centropristis striata"]


### Black Sea Bass Lat Bin Trends with Overall Centroid Shift
png("Figures/BSB_Latbin.png", height=7, width=7, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(cent.bin ~ year, cent_bin[spp=="Centropristis striata"], type="o",
     xlab="year", ylab="Latitude", main="Centroid")
points(cent.bin ~ year, cent_bin[spp=="Centropristis striata" & year==1999], pch=19, col=cols[4])
points(cent.bin ~ year, cent_bin[spp=="Centropristis striata" & year==2000], pch=19, col=cols[5])
points(cent.bin ~ year, cent_bin[spp=="Centropristis striata" & year==2001], pch=19, col=cols[6])


plot(wtcpuenal ~ lat.num, survey_latbin[spp=="Centropristis striata" & year==2000],
     xlim=c(34.5, 43.5), type="o", col=cols[5],
     xlab="latitude bin", ylab="log(biomass) (kg/tow)", main="Biomass")
points(wtcpuenal ~ lat.num, survey_latbin[spp=="Centropristis striata" & year==1999], 
       col=cols[4], type="o")
points(wtcpuenal ~ lat.num, survey_latbin[spp=="Centropristis striata" & year==2001], 
       col=cols[6], type="o")
legend("topright", legend=c(1999, 2000,2001), col=cols[4:6], lty=1, bty="n")

plot(tl_mt ~ lat.num, land[spp=="Centropristis striata" & year==1999],
     xlim=c(34.5, 44.5), type="o", col=cols[4],
     xlab="latitude bin", ylab="landings (mt)", main="Landings")
points(tl_mt  ~ lat.num, land[spp=="Centropristis striata" & year==2000], 
       col=cols[5], type="o")
points(tl_mt  ~ lat.num, land[spp=="Centropristis striata" & year==2001], 
       col=cols[6], type="o")



plot(SBT.actual ~ lat.num, survey_latbin[spp=="Centropristis striata"], 
     col="white", xlim=c(35,43), main="Sea Bottom Temperature",
     xlab="latitude bin", ylab="Sea Bottom Temperature (C)")
legend("topright", legend=yrs.90s, col=cols, lty=1, bty="n")
for(i in 1:length(yrs.90s)){
  points(SBT.actual ~ lat.num, 
         survey_latbin[spp=="Centropristis striata"& year==yrs.90s[i]], 
         col=cols[i], type="o")
}


dev.off()

png("Figures/temb_latbin_yr.png", height=5, width=5, units="in", res=300)
plot(SBT.actual ~ year, temp.latbin, type="o", main="Annual Sea Bottom Temperature (C)")
points(SBT.actual ~ year, temp.latbin[year==1999], col=cols[4], pch=19)
points(SBT.actual ~ year, temp.latbin[year==2000], col=cols[5], pch=19)
points(SBT.actual ~ year, temp.latbin[year==2001], col=cols[6], pch=19)

dev.off()