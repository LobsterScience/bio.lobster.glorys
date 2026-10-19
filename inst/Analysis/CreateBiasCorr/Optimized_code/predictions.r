require(bio.lobster)
library(bio.utilities)
require(devtools)
library(sf)
library(sdmTMB)
library(terra)
library(dplyr)
library(lubridate)

la()
setwd(file.path(project.datadirectory("bio.lobster.glorys")))
t<-readRDS("final_model_biasCorr_m5_sept2.rds")
m4<-t[[1]]
or<-t[[2]]

years_model<-unique(or$YR)


rL <- readRDS( file.path( project.datadirectory("bio.lobster"),  "data", "maps", "LFAPolysSF.rds" )) |>
  		st_as_sf() 
ba <- readRDS("~/git/bio.lobster.data/mapping_data/bathymetrySF.rds") |>
  		st_as_sf() 

fil <- list.files( path = "Downloads", pattern = "^GLORYS.*\\.nc$", full.names = TRUE)


for(f in fil){
		  r <- rast(f)
		  d <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
		  names(d)[1:2] <- c("X","Y")
		  sfp <- st_as_sf(d,coords = c("X","Y"), crs = 4326, remove = FALSE )
		  ii <- lengths(st_within(sfp, rL)) > 0
		  sfp <- sfp[ii, ]
		  sfp <- st_transform(sfp,crs=32620)
		  nn <- st_nearest_feature(sfp, ba)
		  sfp$depth <- ba$z[nn]

		 sfp$Date <- as.Date(sfp$time)
		 doy <- yday(sfp$Date)
		sfp$sinDoy <- sin(2*pi*doy/365.25)
		sfp$cosDoy <- cos(2*pi*doy/365.25)
		sfp$YR <- year(sfp$Date)
		sfp$X1000 = st_coordinates(sfp)[,1]/1000
		sfp$Y1000 = st_coordinates(sfp)[,2]/1000
		sfp$GLORYS_TEMP = XXXXXXXXXXXXXXXX
		sfp <- st_drop_geometry(sfp)
		sfp = filter(sfp, depth>5)
		sfp$lz = log(sfp$depth)
		g<-predict(m4,newdata=sfp)
		sfp$pred=m4$family$linkinv(g$est)
		saveRDS(sfp,file=paste0("BiasCorrPredictions_",f,".rds"))
		rm(r,d,sfp,g)
		gc(reset=T)
		}

#combine the fles

	bb<-dir(pattern="^BiasCorrPredictions_.*\\.rds$")
	out<-bind_rows(
	lapply(bb,readRDS)
	)
	saveRDS(out,"Glorys1994_2025wBiasCorrColumn_doy_oct19.rds")



