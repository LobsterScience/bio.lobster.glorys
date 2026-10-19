require(bio.lobster)
require(devtools)
require(satin)
require(tidyr)
require(terra)
require(dplyr)
require(sf)
load_all("~/git/bio.lobster.glorys")

#locate glorys downloads
setwd(file.path(project.datadirectory('bio.lobster.glorys')))
fil = list.files(path = "Downloads", pattern = "^GLORYS.*\\.nc$", full.names = TRUE)

##observbed temperature data sources
      da = lobster.db('temperature.data')
      da$T_DATE = format(da$T_DATE,'%Y-%m-%d')
      da$LAT_DD = round(da$LAT_DD,3)
      da$LON_DD = round(da$LON_DD,3)
      daa = aggregate(TEMP~T_DATE+LAT_DD+LON_DD,data=da,FUN=median)
      dac = lobster.db('cw.temperature.data')
      dac$T_DATE = format(dac$TempTime,'%Y-%m-%d')
      dac$LAT_DD = round(dac$LAT_DD,3)
      dac$LON_DD = round(dac$LON_DD,3)
      daca = aggregate(Temp~T_DATE+LAT_DD+LON_DD,data=dac,FUN=median)
      daca$TEMP = daca$Temp
      #daca$T_UID = paste('CW',seq(1:nrow(daca)),sep="-")
      
      dm = rbind(daca[,c('T_DATE','LON_DD','LAT_DD','TEMP')],daa[,c('T_DATE','LON_DD','LAT_DD','TEMP')])
      dm$T_DATE = as.Date(dm$T_DATE)
      dm = subset(dm,lubridate::year(T_DATE) %in% 1994:2025)
      
      daT = lobster.db('trudel.temperature.data')
      daT$T_DATE = daT$date
      daT$LON_DD = daT$DEPLOY_LON_DD
      daT$LAT_DD = daT$DEPLOY_LAT_DD
      daT$TEMP = daT$day.mean.temp
      
      dm = rbind(dm[,c('T_DATE','LON_DD','LAT_DD','TEMP')],daT[,c('T_DATE','LON_DD','LAT_DD','TEMP')])
      dm$T_DATE <- as.Date(dm$T_DATE)
      dm$year <- format(dm$T_DATE, "%Y")
      
      yr = sort(unique(dm$year))
      out <- vector("list", length(yr))
      
      # Process one GLORYS file per year
      
      for (i in yr) {
        f <- grep(i, fil)
        r <- rast(fil[f])
        dates <- as.Date(time(r))
        x <-subset(dm, year == i)
        
        pts <- vect(  x,  geom = c("LON_DD", "LAT_DD"), crs = "EPSG:4326"  )
        
        layer <- match(x$T_DATE, dates)
        x$GLORYS_TEMP <- NA
        for(j in unique(layer)) {
          idx <- which(layer == j)
          x$GLORYS_TEMP[idx] <- extract(r[[j]],  pts[idx],  method = "bilinear" )[,2]
        }
        
        out[[as.character(i)]] <- x
      }
      
      dm_glorys <- bind_rows(out)
      dm_glorys <- dm_glorys %>% 
            filter(!is.na(GLORYS_TEMP)) %>%
          select(T_DATE, LON_DD, LAT_DD, TEMP, GLORYS_TEMP)
      
      
      #add in bathy
      
      obs_sf <- st_as_sf(
        dm_glorys,
        coords = c("LON_DD", "LAT_DD"),
        crs = 4326
      )
      
      ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
      ba = ba %>% st_as_sf() 
      
      obs_sf <- st_transform(obs_sf, st_crs(ba))
      ss <- st_nearest_feature(obs_sf, ba)
      obs_sf$depth <- ba$z[ss]
      obs_sf$depth_dist <- as.numeric(st_distance(obs_sf, ba[ss, ], by_element = TRUE))
      dm = subset(obs_sf,!is.na(GLORYS_TEMP)&depth>0& depth_dist<quantile(depth_dist,0.99,na.rm=T) & TEMP< 30 & TEMP> -2)
      
      saveRDS(dm,'Data2GlorMerge_oct192026.rds')
      
      rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
      rL = st_as_sf(rL)
      st_crs(rL) <- 4326
      rll = st_bbox(rL)
      rll[3] = -62 #cropping polys
      rll = st_as_sfc(rll)
      rL = st_intersection(rL,rll)
      rL = st_transform(rL,32620) 
      
      or = st_join(dm,rL)
      or = subset(or,depth<300 & !is.na(LFA))
      
      or$X1000 = st_coordinates(or)[,1]/1000
      or$Y1000 = st_coordinates(or)[,2]/1000
      
      or$diff = or$TEMP - or$GLORYS_TEMP
      or = subset(or,abs(diff)<10)
      
      #circular year
      or$doy = lubridate::yday(or$T_DATE)
      or$sinDoy = sin(2*pi*or$doy/365)
      or$cosDoy = cos(2*pi*or$doy/365)
      saveRDS(or,'dataForsdmTMBbiasSurface_oct192026.rds')
      
      
      
      
      
      