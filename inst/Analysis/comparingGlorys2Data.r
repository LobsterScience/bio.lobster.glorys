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


dam = readRDS(file='GlorysTemps2000_24_wClim.rds')

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

dm = subset(dm,lubridate::year(T_DATE) %in% 2000:2023)

dt = unique(dm$T_DATE)
out = list()
for(i in 1:length(dt)) {
if(i %in% seq(1,length(dt),by=10)) print(paste(Sys.Date(),i))
j = subset(dm,T_DATE==dt[i])
j$clim = NA
j$Glor = NA
j$dist = NA
k = subset(dam,Date==dt[i])

js = st_as_sf(j,coords=c('LON_DD','LAT_DD'),crs=4326)
ks = st_as_sf(k,coords=c('X','Y'),crs=4326)
	for(l in 1:nrow(j)){
	b = st_nearest_feature(js[l,],ks)
	j[l,'dist'] = st_distance(js[l,],ks[b,])
	j[l,'clim'] = ks[b,'climT']
	j[l,'Glor'] = ks[b,'bottomT']
}
out[[i]] = j
}

oi = do.call(rbind,out)
saveRDS(oi,'Data2GlorMerge.rds')

