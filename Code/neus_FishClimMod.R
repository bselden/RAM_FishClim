### Objective: Evaluate whether harvest influences presence (or biomass) of a species


### Load libraries
library(data.table)
library(mgcv)
library(Hmisc)
library(plotrix)

### Load datasets
surv.dat <- readRDS("output/dat.latbin2.rds")
land.dat <- readRDS("output/total.land.latbin.rds")

setorder(land.dat, spp, year)

par(mfrow=c(2,1))
plot(pres2 ~lat.bin, surv.dat[spp=="Gadus morhua"])
plot(wtcpuenal ~ lat.bin, surv.dat[spp=="Gadus morhua"])
# ===============================
# = Assign catch=0 for areas surveyed and no catch observed =
# ===============================
### Make sure each species has a landing record for each year and lat bin in the survey
#### Get rid of southernmost survey bins because not observed in every year
combined.dat <- merge(surv.dat[!(is.na(lat.bin))& year<2015 &!(lat.bin %in% c("30.5", "31.5", "32.5", "33.5", "34.5"))], 
                      land.dat, by=c("lat.bin", "year", "spp"), all.x=T)
setorder(combined.dat, spp, year, lat.bin)

combined.dat[,"land_mt":=ifelse(is.na(tl_mt), 0, tl_mt)]

# ===============================
# = Standardize Catch within spp=
# ===============================

### Standardize catch so mean of 0 and sd=1 within a species
### If no catch in any year, give value of 0
catch.scale <- combined.dat[,list(mean_land=mean(land_mt, na.rm=T), sd_land=sd(land_mt, na.rm=T)), by=list(spp)]
combined.dat2 <- merge(combined.dat, catch.scale, by="spp")

combined.dat2[,"land_scaled":=ifelse(mean_land==0, 0, scale(land_mt, center=T, scale=T)), by=list(spp)]

#land_yr <- combined.dat2[,list(mean_land=mean(land_mt), mean_land_scaled=mean(land_scaled)), by=list(spp, year)]

# ===============================
# = Standardize Temperature within spp =
# ===============================
#Normalize each temperature for each species range to a mean of 0 and SD of 1 across spp geo range
# Subtract its mean temperature of occurrence (or of biomass) from the observed temperature
# Then divide by standard deviation of temperature of occurrence (or of biomass)
par(mfrow=c(2,2))
weighted.hist(combined.dat2[spp=="Gadus morhua"]$SBT.actual, 
              w=combined.dat2[spp=="Gadus morhua"]$wtcpuenal, freq=F, main="Cod SBT, w=Biomass")
weighted.hist(combined.dat2[spp=="Gadus morhua"]$SBT.actual, 
              w=combined.dat2[spp=="Gadus morhua"]$pres2, freq=F, main="Cod SBT, w=Presence")
weighted.hist(combined.dat2[spp=="Centropristis striata"]$SBT.actual, 
              w=combined.dat2[spp=="Centropristis striata"]$wtcpuenal, freq=F, main="Dogfish SBT, w=Biomass")
weighted.hist(combined.dat2[spp=="Centropristis striata"]$SBT.actual, 
              w=combined.dat2[spp=="Centropristis striata"]$pres2, freq=F, main="Dogfish SBT, w=Presence")


combined.dat2[,"mean.SBTpres":=weighted.mean(SBT.actual, w=pres2), by=list(spp)]
combined.dat2[,"mean.SSTpres":=weighted.mean(SST.actual, w=pres2), by=list(spp)]

combined.dat2[,"sd.SBTpres":=sqrt(wtd.var(SBT.actual, weights=pres2)), by=list(spp)]
combined.dat2[,"sd.SSTpres":=sqrt(wtd.var(SST.actual, weights=pres2)), by=list(spp)]

combined.dat2[,"mean.SBTbio":=weighted.mean(SBT.actual, w=wtcpuenal), by=list(spp)]
combined.dat2[,"mean.SSTbio":=weighted.mean(SST.actual, w=wtcpuenal), by=list(spp)]

combined.dat2[,"sd.SBTbio":=sqrt(wtd.var(SBT.actual, weights=wtcpuenal)), by=list(spp)]
combined.dat2[,"sd.SSTbio":=sqrt(wtd.var(SST.actual, weights=wtcpuenal)), by=list(spp)]

combined.dat2[,"scaled.SBT":=(SBT.actual - mean.SBTpres)/sd.SBTpres]
combined.dat2[,"scaled.SST":=(SST.actual - mean.SSTpres)/sd.SSTpres]

plot(wtcpuenal ~ scaled.SBT, combined.dat2[spp=="Gadus morhua"])
plot(wtcpuenal ~ avg.SST.min, combined.dat2[spp=="Gadus morhua"])




# ===============================
# = Lag variables in time =
# ===============================
### Lag catch, biomass, presence, and environment by one year
vars <- c("pres2", "wtcpuenal", "land_mt", "land_scaled",
          "scaled.SBT", "avg.SBT.min", "avg.SBT.max", 
          "scaled.SST", "avg.SST.min", "avg.SST.max")
lagcols <- paste("lag", vars, sep=".")
lag.dt <- combined.dat2[,(lagcols):=shift(.SD), by=list(spp, lat.bin), .SDcols=vars]


# ## Restrict to 35.5 North, since bins further south only sampled in 80s
# lag.dtN <- lag.dt[!(lat.bin %in% c("30.5", "31.5", "32.5"))]

# ===============================
# = GAM for Log Bio=
# ===============================

### Model (log bio) ~ scaled SBT + scaled SST + depth + landings in yr-1 + presence in yr-1 + random(spp)
#### Test with cod
cod.mod.bio <- gam(wtcpuenal ~ s(scaled.SBT)+ s(scaled.SST)+ s(depth) + s(lag.land_scaled) + s(lag.pres2),
                   family=gaussian, data=lag.dt[spp%in%c("Gadus morhua")])
plot(cod.mod.bio)


# pdf("Figures/FishClimGAMOut.bio.pdf", height=7, width=5)
# mod.fish.clim.bio <- lag.dt[!(spp %in% catch.scale[mean_land<1]$spp),j={
#   print(spp)
#   t.dt <- .SD
#   num.rec.fished <- length(t.dt$land_mt>0)
#   if(num.rec.fished >5){
#     mod.b <- gam(wtcpuenal ~ s(scaled.SBT)+ s(scaled.SST)+ s(depth) + s(lag.land_scaled) + s(lag.pres2),
#                  family=gaussian, data=t.dt)
#     chi <- as.vector(summary(mod.b)$chi.sq)
#     fval <- as.vector(summary(mod.b)$s.table[,3])
#     pval <- as.vector(summary(mod.b)$s.pv)
#     vars <- names(summary(mod.b)$chi.sq)
#     
#     par(mfrow=c(3,2))
#     plot(mod.b)
#     plot(fitted(mod.b), napredict(mod.b$na.action, mod.b$y), main=spp, 
#          ylab="Observed", xlab="Fitted")
#     
#     list(vars=vars, chi=chi, fval=fval, pval=pval)
#   }
#   else{list(vars=NA, chi=NA, fval=NA, pval=NA)}
# }, by=list(spp)]
# dev.off()

# ===============================
# = Mixed Effects GAM for Log Bio=
# ===============================
### Species as random effect
### See random effects in mgcv https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html
### Model (log bio) ~ scaled SBT + scaled SST + depth + landings in yr-1 + presence in yr-1 + random(spp)


### All species single model
spplim.mod.bio <- gam(wtcpuenal ~ s(scaled.SBT)+ s(scaled.SST)+ s(depth) + s(lag.land_scaled) + s(lag.pres2)+ s(spp, bs="re"),
               family=gaussian, data=lag.dt[spp%in%c("Gadus morhua", "Squalus acanthias", "Limanda ferruginea")])

gam.vcomp(spplim.mod.bio)


# ===============================
# = Beta Regression for Presence=
# ===============================
### To use beta regression, need pres2 to be (0,1)
lag.dt[,"pres2_beta":=ifelse(pres2==0, 0.00001, ifelse(pres2==1, 0.99999, pres2))]

par(mfrow=c(2,2))
plot(avg.SBT.max ~ lat.bin, lag.dt, ylab="Average SBT Max", xlab="Latitude Bin")
plot(avg.SST.max ~ lat.bin, lag.dt, ylab="Average SST Max", xlab="Latitude Bin")
boxplot(pres2 ~ lat.bin, lag.dt[spp=="Gadus morhua"], main="Atlantic Cod",
        ylab="Mean Presence", xlab="Latitude Bin")
boxplot(pres2 ~ lat.bin, lag.dt[spp=="Limanda ferruginea"], main="Yellowtail Flounder", 
        ylab="Mean Presence", xlab="Latitude Bin")

# par(mfrow=c(3,2))
# plot(pres2 ~ avg.SST.max, lag.dtN[spp=="Gadus morhua"], main="Atlantic Cod",
#      ylab="Mean Presence", xlab="Average SST Max")
# plot(pres2 ~ avg.SST.max, lag.dtN[spp=="Limanda ferruginea"], main="Yellowtail Flounder",
#      ylab="Mean Presence", xlab="Average SST Max")
# plot(pres2 ~ lag.land_scaled, lag.dtN[spp=="Gadus morhua"], main="Atlantic Cod",
#      ylab="Mean Presence", xlab="Scaled Landings t-1")
# plot(pres2 ~ lag.land_scaled, lag.dtN[spp=="Limanda ferruginea"], main="Yellowtail Flounder",
#      ylab="Mean Presence", xlab="Scaled Landings t-1")
# plot(lag.land_scaled ~ lag.pres2, lag.dtN[spp=="Gadus morhua"], main="Atlantic Cod",
#      ylab="Scaled Landings t-1", xlab="Mean Presence t-1")
# plot(lag.land_scaled ~ lag.pres2, lag.dtN[spp=="Limanda ferruginea"], main="Yellowtail Flounder",
#      ylab="Scaled Landings t-1", xlab="Mean Presence t-1")



### Model (pres) ~ max SBT + max SST + depth + landings in yr-1 + presence in yr-1
#### Test with cod
cod.mod <- gam(pres2_beta ~ s(avg.SBT.max)+ s(avg.SST.max)+ s(depth) + s(lag.land_scaled) + s(lag.pres2),
               family=betar(link="logit"), data=lag.dt[spp=="Gadus morhua"])

#### Test with yellowtail flounder
yt.mod <- gam(pres2_beta ~ s(avg.SBT.max)+ s(avg.SST.max)+ s(depth) + s(lag.land_scaled) + s(lag.pres2),
               family=betar(link="logit"), data=lag.dt[spp=="Limanda ferruginea"])
## Try landings alone without presence
yt.mod2 <- gam(pres2_beta ~ s(avg.SBT.max)+ s(avg.SST.max)+ s(depth) + s(lag.land_scaled),
              family=betar(link="logit"), data=lag.dt[spp=="Limanda ferruginea"])
# Try presence alone without landings
yt.mod3 <- gam(pres2_beta ~ s(avg.SBT.max)+ s(avg.SST.max)+ s(depth) + s(lag.pres2),
               family=betar(link="logit"), data=lag.dt[spp=="Limanda ferruginea"])

plot(yt.mod)

# ### Remove unfished and very lightly fished species
# ### use scaled SST & SBT
# pdf("Figures/FishClimGAMOut.pdf", height=7, width=5)
# mod.fish.clim <- lag.dt[!(spp %in% catch.scale[mean_land<1]$spp),j={
#   print(spp)
#   t.dt <- .SD
#   num.rec.fished <- length(t.dt$land_mt>0)
#   if(num.rec.fished >5){
#     mod.p <- gam(pres2_beta ~ s(scaled.SBT)+ s(scaled.SST)+ s(depth) + s(lag.land_scaled) + s(lag.pres2),
#                  family=betar(link="logit"), data=t.dt)
#     chi <- as.vector(summary(mod.p)$chi.sq)
#     fval <- as.vector(summary(mod.p)$s.table[,3])
#     pval <- as.vector(summary(mod.p)$s.pv)
#     vars <- names(summary(mod.p)$chi.sq)
#     
#     par(mfrow=c(3,2))
#     plot(mod.p)
#     plot(fitted(mod.p), napredict(mod.p$na.action, mod.p$y), main=spp, 
#          ylab="Observed", xlab="Fitted")
#     
#     list(vars=vars, chi=chi, fval=fval, pval=pval)
#   }
#   else{list(vars=NA, chi=NA, fval=NA, pval=NA)}
# }, by=list(spp)]
# dev.off()
# 
# mod.fish.clim.nopres <- lag.dt[!(spp %in% catch.scale[mean_land<1]$spp),j={
#   print(spp)
#   t.dt <- .SD
#   num.rec.fished <- length(t.dt$land_mt>0)
#   if(num.rec.fished >5){
#     mod.p <- gam(pres2_beta ~ s(avg.SBT.max)+ s(avg.SST.max)+ s(depth) + s(lag.land_scaled),
#                  family=betar(link="logit"), data=t.dt)
#     chi <- as.vector(summary(mod.p)$chi.sq)
#     pval <- as.vector(summary(mod.p)$s.pv)
#     vars <- names(summary(mod.p)$chi.sq)
#     
#     par(mfrow=c(3,2))
#     plot(mod.p)
#     plot(fitted(mod.p), napredict(mod.p$na.action, mod.p$y), main=spp, 
#          ylab="Observed", xlab="Fitted")
#     qq.gam(mod.p)
#     
#     list(vars=vars, chi=chi, pval=pval)
#   }
#   else{list(vars=NA, chi=NA, pval=NA)}
# }, by=list(spp)]

sig.mod <- mod.fish.clim[,list(sig.SST=pval[vars=="s(avg.SST.max)"]<0.05,
                               sig.SBT=pval[vars=="s(avg.SBT.max)"]<0.05,
                               sig.depth=pval[vars=="s(depth)"]<0.05,
                               sig.land=pval[vars=="s(lag.land_scaled)"]<0.05,
                               sig.pres=pval[vars=="s(lag.pres2)"]<0.05),
                         by=list(spp)]
sig.mod[,sum(sig.SST)]
sig.mod[,sum(sig.SBT)]
sig.mod[,sum(sig.depth)]
sig.mod[,sum(sig.land)]


sig.mod.nopres <- mod.fish.clim.nopres[,list(sig.SST=pval[vars=="s(avg.SST.max)"]<0.05,
                               sig.SBT=pval[vars=="s(avg.SBT.max)"]<0.05,
                               sig.depth=pval[vars=="s(depth)"]<0.05,
                               sig.land=pval[vars=="s(lag.land_scaled)"]<0.05),
                         by=list(spp)]
sig.mod.nopres[,sum(sig.SST)]
sig.mod.nopres[,sum(sig.SBT)]
sig.mod.nopres[,sum(sig.depth)]
sig.mod.nopres[,sum(sig.land)]

# ===============================
# = Regression for Delta Presence=
# ===============================
lag.dt[,"pres2_delta":=pres2 - lag.pres2]
lag.dt[,"SBT.delta":=avg.SBT.max - lag.avg.SBT.max]
lag.dt[,"SST.delta":=avg.SST.max - lag.avg.SST.max]
lag.dt[,"lag.land.delta":=land_scaled - lag.land_scaled]

### Low deviance explained for delta temperatures
delta.cod.mod <- gam(pres2_delta ~ s(SBT.delta) + s(SST.delta)+ s(depth) + s(lag.land_scaled),
                     data=lag.dt[spp=="Gadus morhua"])

### Really low deviance explained for absolute temperatures
delta.cod.mod2 <- gam(pres2_delta ~ s(avg.SBT.max) + s(avg.SST.max)+ s(depth) + s(lag.land_scaled),
                     data=lag.dt[spp=="Gadus morhua"])



# ===============================
# = First Differences Econometrics Model=
# ===============================
lag.dt[,"bio_delta":=wtcpuenal - lag.wtcpuenal]
fd.bio.cod <- gam(bio_delta ~ s(SBT.delta) + s(SST.delta)+ s(lag.land.delta),
                  data=lag.dt[spp=="Gadus morhua"])

summary(fd.bio.cod)

# ===============================
# = Fixed Effects Econometrics Model=
# ===============================
### Substract off mean for each latitude bin and species

# vars2 <- c("pres2", "wtcpuenal", "land_mt", "land_scaled",
#           "SBT.actual", "avg.SBT.min", "avg.SBT.max", 
#           "SST.actual", "avg.SST.min", "avg.SST.max")
# fecols <- paste("fe", vars, sep=".")
# fe.dt <- combined.dat2[,(fecols):=mean(.SD, na.rm=T), by=list(spp, lat.bin), .SDcols=vars2]

fe.dt <- lag.dt[,list(m.pres2=mean(pres2, na.rm=T),
                             m.wtcpuenal=mean(wtcpuenal, na.rm=T),
                             m.land =mean(land_scaled, na.rm=T),
                             m.SBT=mean(SBT.actual, na.rm=T),
                             m.SST=mean(SST.actual, na.rm=T)),
                       by=list(spp, lat.bin)]

fe.dt2 <- merge(lag.dt[!(lat.bin %in% c("30.5", "31.5", "32.5", "33.5", "34.5"))],
                fe.dt, by=c("spp", "lat.bin"))

fe.dt2[,"fe.pres2":=pres2 - m.pres2]
fe.dt2[,"fe.wtcpuenal":=wtcpuenal - m.wtcpuenal]
fe.dt2[,"fe.lag.land":=lag.land_scaled - m.land]
fe.dt2[,"fe.SBT":=SBT.actual - m.SBT]
fe.dt2[,"fe.SST":=SST.actual - m.SST]

fe.mod.cod <- gam(fe.wtcpuenal ~ s(fe.lag.land) + s(fe.SBT) + s(fe.SST) + s(depth), data=fe.dt2[spp=="Gadus morhua"])
fe.mod.cod.lm <- lm(fe.wtcpuenal ~ fe.lag.land + fe.SBT + fe.SST + depth, data=fe.dt2[spp=="Gadus morhua"])

par(mfrow=c(2,2))
plot(fe.mod.cod)
# higher landings than average in the previous year associated with higher than average biomass this year 


# ===============================
# = Panel Data Approaches=
# ===============================
### Based on Ethan Addicott's code
library(plm)
df <- combined.dat2[spp=="Gadus morhua"]
setorder(df, lat.bin, year)

### Create the panel
p.df <- pdata.frame(df, index = c("lat.bin","year"))

### Lag landings by one year (in same lat.bin)
p.df$land_mt.lag.1 <- lag(p.df$land_mt, k = 1)

### Just do the FE model (as I did manually in the previous section of code) 
## Analagous to Ethan's m4
##  m.4 <- plm(wtcpue~pred.total+ER.lag.1, data = p.df, model = "within")
m.fe <- plm(wtcpuenal ~ SBT.actual + SST.actual + depth+ land_mt.lag.1, data=p.df, model="within")
summary(m.fe)
plot(m.fe)
