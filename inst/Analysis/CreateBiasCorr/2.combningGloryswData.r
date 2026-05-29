require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(ggplot2)
require(data.table)
require(RANN)
la()


#deviations by year

setwd(file.path(project.datadirectory('bio.lobster.glorys')))

dam = readRDS(file='GlorysTemps_Depth2000_2025.rds')

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
dm = subset(dm,lubridate::year(T_DATE) %in% 2000:2025)

daT = lobster.db('trudel.temperature.data')
daT$T_DATE = daT$date
daT$LON_DD = daT$DEPLOY_LON_DD
daT$LAT_DD = daT$DEPLOY_LAT_DD
daT$TEMP = daT$day.mean.temp

dm = rbind(dm[,c('T_DATE','LON_DD','LAT_DD','TEMP')],daT[,c('T_DATE','LON_DD','LAT_DD','TEMP')])

#split out years
dm$y = lubridate::year(dm$T_DATE)
dam$y = lubridate::year(dam$Date)
dy = unique(dm$y)
for(k in seq_along(dy)){
	v = subset(dm,y == dy[k])

	l = subset(dam,y==dy[k])
	saveRDS(list(v,l), file=paste0('Gl_ob',dy[k],'.rds'))
}

v = dir()
v = v[grep('Gl_ob',v)]

for(i in 1:length(v)) {
		b = readRDS(v[i])
		b1 = b[[1]]
		b2=b[[2]]

		ud =unique(b1$T_DATE)
		for(j in seq_along(ud)){

			g = subset(b1,T_DATE==ud[j])
#			g$clim = NA
			g$Glor = NA
			g$dist = NA
			k = subset(b2,Date==ud[j])
	
js = st_as_sf(g,coords=c('LON_DD','LAT_DD'),crs=4326)
ks = st_as_sf(k,crs=4326)

	for(l in 1:nrow(g)){
	b = st_nearest_feature(js[l,],ks)
	g[l,'dist'] = st_distance(js[l,],ks[b,])
#	g[l,'clim'] = ks[b,'climT']
	g[l,'Glor'] = ks[b,'bottomT']
}
saveRDS(g,file=paste0('combGL_DA',ud[j],'.rds'))
}
rm(b)
rm(b1)
rm(b2)
gc()
}


out = list()
v = dir()
v = v[grep('combGL_DA',v)]

for(i in 1:length(v)){

out[[i]] = readRDS(v[i])
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
oiu = subset(oiu,!is.na(Glor)&z>0& dist<quantile(dist,0.99,na.rm=T) & TEMP< 30 & TEMP> -2)
oi = oiu

saveRDS(oi,'Data2GlorMerge_may212026.rds')


#oi = readRDS('Data2GlorMerge.rds')
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
saveRDS(or,'dataForsdmTMBbiasSurface_may162026.rds')

#or = readRDS('dataForsdmTMBbiasSurface.rds')

