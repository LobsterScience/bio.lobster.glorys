require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(ggplot2)
require(data.table)
la()


#deviations by year

setwd(file.path(project.datadirectory('bio.lobster.glorys')))

dam = readRDS(file='GlorysTemps2000_2025.rds')

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

dm = subset(dm,lubridate::year(T_DATE) %in% 2000:2025)

dt = unique(dm$T_DATE)
out = list()
for(i in 1:length(dt)) {
if(i %in% seq(1,length(dt),by=10)) print(paste(Sys.Date(),i))
j = subset(dm,T_DATE==dt[i])
#j$clim = NA
j$Glor = NA
j$dist = NA
k = subset(dam,Date==dt[i])

js = st_as_sf(j,coords=c('LON_DD','LAT_DD'),crs=4326)
ks = st_as_sf(k,coords=c('X','Y'),crs=4326)
	for(l in 1:nrow(j)){
	b = st_nearest_feature(js[l,],ks)
	j[l,'dist'] = st_distance(js[l,],ks[b,])
#	j[l,'clim'] = ks[b,'climT']
	j[l,'Glor'] = ks[b,'bottomT']
}
out[[i]] = j
}

oi = do.call(rbind,out)

oii = st_as_sf(oi,coords=c('LON_DD','LAT_DD'),crs=4326)
oiu = st_transform(oii,crs=32620)
st_geometry(oiu) = st_geometry(oiu)/1000
st_crs(oiu) = 32620

ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

ss = st_nearest_feature(oiu,ba)
ds = st_distance(oiu,ba[ss,],by_element=T)
st_geometry(ba) = NULL
oiu$z = ba$z[ss]
oiu$z_dist = as.numeric(ds)

oi = oiu
saveRDS(oi,'Data2GlorMerge.rds')


oi = readRDS('Data2GlorMerge.rds')
ois = subset(oi,!is.na(dist) &dist<quantile(dist,0.99,na.rm=T) & !is.na(Glor) & TEMP<30 & TEMP> -2 & z>0) #<5ish km
ois$YR = lubridate::year(ois$T_DATE)
oisS = st_as_sf(ois,crs=32620)
#ggplot(oisS)+geom_sf()


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rll = st_bbox(rL)
rll[3] = -62 #cropping polys
rll = st_as_sfc(rll)
rL = st_intersection(rL,rll)

rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620

  or = st_join(oisS, rL)
  or = subset(or,z<300 & !is.na(LFA))

or$X1000 = st_coordinates(or)[,1]
or$Y1000 = st_coordinates(or)[,2]

or$diff = or$TEMP - or$Glor
or = subset(or,abs(diff)<10)

#circular year
or$doy = lubridate::yday(or$T_DATE)
or$sinDoy = sin(2*pi*or$doy/365)
or$cosDoy = cos(2*pi*or$doy/365)
saveRDS(or,'dataForsdmTMBbiasSurface.rds')


