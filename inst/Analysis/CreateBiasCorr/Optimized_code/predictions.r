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
t<-readRDS("final_model_biasCorr_m5_oct19.rds")
m4<-t[[1]]
or<-t[[2]]
or$YR = year(or$T_DATE)
years_model<-unique(or$YR)


rL <- readRDS( file.path( project.datadirectory("bio.lobster"),  "data", "maps", "LFAPolysSF.rds" )) |>
  		st_as_sf(crs=4326) 
ba <- readRDS("~/git/bio.lobster.data/mapping_data/bathymetrySF.rds") |>
  		st_as_sf() 

fil <- list.files( path = "Downloads", pattern = "^GLORYS.*\\.nc$", full.names = TRUE)

fy = as.numeric(sub(".*GLORYS([0-9]{4}).*", "\\1", fil))
require(parallel)

logfile = 'prediction_progress.txt'
pred_year <- function(y) {
	
		  f = fil[which(fy==y)]
		  r <- rast(f,subds='bottomT')
		  d <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
		  names(d)[1:2] <- c("X","Y")
		  sfp <- st_as_sf(d,coords = c("X","Y"), crs = 4326)
		  ii <- lengths(st_within(sfp, rL))>0
		  sfp <- sfp[ii, ]
		  sfp <- st_transform(sfp,crs=32620)
		  nn <- st_nearest_feature(sfp, ba)
		  sfp$depth <- ba$z[nn]
		  sfp = subset(sfp,depth>0 & depth < 400)
                 sfp$X1000 = st_coordinates(sfp)[,1]/1000
		 sfp$Y1000 = st_coordinates(sfp)[,2]/1000

		days <- grep("^bottomT_", names(sfp), value = TRUE)
			preds <- vector("list", length(days))
			names(preds) <- days	
			sfp$lz <- log(sfp$depth)
			sfp$YR = y
			base_nd = as_tibble(subset(sfp,select=c(X1000,Y1000,lz,YR)))
				for(i in seq_along(days)) {

				if(i %% 50 == 0){
					  cat(paste( Sys.time(), "year", y, "day", i, "of", length(days), "\n"  ),
					    file = logfile,
					    append = TRUE
						  )
						}
				  day_col <- days[i]
				  doy <- as.numeric(sub("bottomT_", "", day_col))
				  nd <- base_nd
				  nd$GLORYS_TEMP <- sfp[[day_col]]
				  nd$sinDoy <- sin(2 * pi * doy / 365.25)
				  nd$cosDoy <- cos(2 * pi * doy / 365.25)
  				p <- predict(   m4,  newdata = nd  )

			  preds[[i]] <- data.frame(
						    X = nd$X1000,
						    Y = nd$Y1000,
						    DOY = doy,
						    EST = m4$family$linkinv(p$est),
						    GLORYS_TEMP = nd$GLORYS_TEMP,
						    YR= y,
						    depth = sfp$depth )
					}

				preds <- bind_rows( preds)	 
			saveRDS(preds,file=paste0('BiasCorrPredictions_',y,'.rds'))
			rm(r,f,d,sfp,nn,preds)
			gc(reset=T)
		}





mclapply(
  years_model,
  pred_year,mc.cores=5)





#combine the fles

	bb<-dir(pattern="^BiasCorrPredictions_.*\\.rds$")
	out<-bind_rows(
	lapply(bb,readRDS)
	)
	saveRDS(out,"Glorys1994_2025wBiasCorrColumn_doy_oct19.rds")



