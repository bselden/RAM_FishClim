# ======================================================================
# = Prepare data for GAM =
# ======================================================================
#####################################
### Add zeros where haul but species not observed, and merge with mean biomass for each species for GAM
library(plyr)

add.zeros.sp <- function(bio.dt, haul.dt){
	bio.dt$pres <- bio.dt$year > 0
	
	### Merge bio with hauls so that every haul is repeated
	spp <- sort(unique(bio.dt$spp))
	print("Step1")

	hauls <- unique(haul.dt$haulid)

	haul.spp.master <- as.data.table(expand.grid(haulid=hauls, spp=spp))
	
	### Merge back with haul info to get those haul meta data associated with file
	haul.spp <- merge(haul.dt, haul.spp.master, all.y=T, by="haulid")
	
	
	cols.shared <- colnames(bio.dt)[colnames(bio.dt)%in%colnames(haul.spp)]
	
	# ### Merge with mean biomass to get those data associated
	# haul.spp.mean.bio <- merge(haul.spp, mean.bio.yr, by=c("spp", "year"))
	print("Step2")
	### Merge with biomass 
	bio.haul.zeros <- merge(haul.spp, bio.dt, 
		by=cols.shared, all.x=T)
	
	##
	
	### Make pres FALSE where NA
	bio.haul.zeros$pres2 <- as.logical(ifelse(is.na(bio.haul.zeros$pres), "FALSE", bio.haul.zeros$pres))
	
	### Make NAs for absences into zero biomass
	bio.haul.zeros$wtcpue <- ifelse(is.na(bio.haul.zeros$wtcpue), 0, bio.haul.zeros$wtcpue)
	bio.haul.zeros$cntcpue <- ifelse(is.na(bio.haul.zeros$cntcpue), 0, bio.haul.zeros$cntcpue)
	

	### Reassign -9999 to NA
	bio.haul.zeros$wtcpue <- ifelse(bio.haul.zeros$wtcpue == -9999, NA, bio.haul.zeros$wtcpue) 
	bio.haul.zeros$cntcpue <- ifelse(bio.haul.zeros$cntcpue == -9999, NA, bio.haul.zeros$cntcpue) 


	### If wtcpue==0, set to something close to zero
	bio.haul.zeros$wtcpuena <- ifelse(bio.haul.zeros$wtcpue == 0, 1e-4, bio.haul.zeros$wtcpue)
	bio.haul.zeros$cntcpuena <- ifelse(bio.haul.zeros$cntcpue == 0, 1e-4, bio.haul.zeros$cntcpue)
	
	### Take log of biomass
	bio.haul.zeros$wtcpuenal <- log(bio.haul.zeros$wtcpuena)
	bio.haul.zeros$cntcpuenal <- log(bio.haul.zeros$cntcpuena)
	
	print("Step3")
	mean.bio.yr <- bio.haul.zeros[,list(mean.wtcpue=meanna(wtcpue)), 
	                         by=list(spp, year, season)]
	
	# ### Stratified Mean biomass of each species in each year (including the zeros)
	# mean.bio.yr <- bio.haul.zeros[,list(mean.wtcpue=meanna(wtcpue)),
	#                          by=list(spp, year, season, stratum, stratumarea)]
	# strat.mean.bio.yr <- mean.bio.yr[,list(strat.mean.wtcpue=weighted.mean(mean.wtcpue, w=stratumarea, na.rm=T)),
	#                          by=list(spp, year, season)]
	### Merge into hauls (to include that info for GAM fitting)
	bio.haul.zeros <- merge(bio.haul.zeros, mean.bio.yr, by=c("spp", "year", "season"))						 
							 
	
	### 0.1 grid of all survey points
	bio.haul.zeros$lon.1 <- round_any(bio.haul.zeros$lon, 0.1)
	bio.haul.zeros$lat.1 <- round_any(bio.haul.zeros$lat, 0.1)
	
	return(bio.haul.zeros)
}



add.zeros.sp.bioonly <- function(bio.dt, haul.dt){
  bio.dt$pres <- bio.dt$year > 0
  
  ### Merge bio with hauls so that every haul is repeated
  spp <- sort(unique(bio.dt$spp))
  print("Step1")
  
  hauls <- unique(haul.dt$haulid)
  
  haul.spp.master <- as.data.table(expand.grid(haulid=hauls, spp=spp))
  
  ### Merge back with haul info to get those haul meta data associated with file
  haul.spp <- merge(haul.dt, haul.spp.master, all.y=T, by="haulid")
  
  
  cols.shared <- colnames(bio.dt)[colnames(bio.dt)%in%colnames(haul.spp)]
  
  # ### Merge with mean biomass to get those data associated
  # haul.spp.mean.bio <- merge(haul.spp, mean.bio.yr, by=c("spp", "year"))
  print("Step2")
  ### Merge with biomass 
  bio.haul.zeros <- merge(haul.spp, bio.dt, 
                          by=cols.shared, all.x=T)
  
  ##
  
  ### Make pres FALSE where NA
  bio.haul.zeros$pres2 <- as.logical(ifelse(is.na(bio.haul.zeros$pres), "FALSE", bio.haul.zeros$pres))
  
  ### Make NAs for absences into zero biomass
  bio.haul.zeros$wtcpue <- ifelse(is.na(bio.haul.zeros$wtcpue), 0, bio.haul.zeros$wtcpue)

  
  ### Reassign -9999 to NA
  bio.haul.zeros$wtcpue <- ifelse(bio.haul.zeros$wtcpue == -9999, NA, bio.haul.zeros$wtcpue) 

  
  ### If wtcpue==0, set to something close to zero
  bio.haul.zeros$wtcpuena <- ifelse(bio.haul.zeros$wtcpue == 0, 1e-4, bio.haul.zeros$wtcpue)

  ### Take log of biomass
  bio.haul.zeros$wtcpuenal <- log(bio.haul.zeros$wtcpuena)


  
  return(bio.haul.zeros)
}