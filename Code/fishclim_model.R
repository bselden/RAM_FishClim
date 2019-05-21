
library(data.table)
load("Output/RAM_U_out.RData", verbose=T)
load("Output/cent_out.RData")

### Merge annual centroid data with annual fishing info (lost many observations)--> check why
### still 80 species though
annual_trend_fish <- merge(cent_master_lim, useries3, 
                           by.x=c("spp", "subarea", "year"), 
                           by.y=c("scientificname", "subarea", "year"))

library(lme4)
annual_mod <- lmer(annual.bias ~ UdivUmsypref + (1|spp), annual_trend_fish)
annual_mod2 <- lm(annual.bias ~ UdivUmsypref, annual_trend_fish)

plot(annual.bias ~ UdivUmsypref, annual_trend_fish)

## Merge centroid trends with summary information with fishing intensity
trend_cent_fish <- merge(cent_lm, uspp.yrs, by.x=c("spp", "subarea"), by.y=c("scientificname", "subarea"))
### 29 species in RAM that don't have climate models for from Jim's paper
### 93 species in both


cent.mod <- lm(lm.obs.slope ~ lm.pred.slope + meanU.Umsy, trend_cent_fish)



png("Figures/lagclim_F.png", height=5, width=5, units="in", res=300)
plot(lm.obs.slope ~ lm.pred.slope, trend_cent_fish)
points(lm.obs.slope ~ lm.pred.slope, trend_cent_fish[meanU.Umsy>1], pch=10, cex=0.5, col="darkred")
abline(a=0, b=1, col="red")
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

png("Figures/lagclim_fracover.png", height=5, width=5, units="in", res=300)
plot(lm.obs.slope ~ lm.pred.slope, trend_cent_fish)
points(lm.obs.slope ~ lm.pred.slope, trend_cent_fish[frac.yrs.overage>0.5], pch=10, cex=0.5, col="darkred")
abline(a=0, b=1, col="red")
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

### Centroid by year with fishing by year
cent_fish_yr <- merge(cent_master_lim, useries3, by.x=c("spp", "subarea", "year"), 
                      by.y=c("scientificname", "subarea", "year"))

cent_mod_yr <- cent_fish_yr[,j={
  t.dt <- .SD
  mod_yr <- lm(lat.cent ~ lat.cent.cm + UdivUmsypref, t.dt)
  list(cm.slope=summary(mod_yr)$coefficients[2,1], 
       cm.p=summary(mod_yr)$coefficients[2,4],
       fish.slope=summary(mod_yr)$coefficients[3,1],
       fish.p=summary(mod_yr)$coefficients[3,4])
}, by=list(spp, subarea)]

hist(cent_mod_yr$cm.slope)
hist(cent_mod_yr$fish.slope)

cent_mod_yr[fish.p<0.05]
