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

if(redo.comps){
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
}

oi = readRDS('Data2GlorMerge.rds')
ois = subset(oi,dist<quantile(dist,0.99) & !is.na(Glor) & TEMP<30 & TEMP> -2 & z>0) #<5ish km
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

or$Q = lubridate::quarter(or$T_DATE)
or$X1000 = st_coordinates(or)[,1]
or$Y1000 = st_coordinates(or)[,2]

or$diff = or$TEMP - or$Glor
or = subset(or,abs(diff)<10)
#ggplot() + geom_point(data=subset(or,Q %in% 3),aes(colour=diff,fill=diff,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
#  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#  scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#  theme_test_adam()

#seeems to be biased low in summer and slightly high in winter. Coming up with a bias correction surface using DOY

#circular year
#or$doy = lubridate::yday(or$T_DATE)
#or$sinDoy = sin(2*pi*or$doy/365)
#or$cosDoy = cos(2*pi*or$doy/365)
or = readRDS('dataForsdmTMBbiasSurface.rds')

or$month = lubridate::month(or$T_DATE)
or$lz = log(or$z)
or$sinMon = sin(2*pi*or$month/12)
or$cosMon = cos(2*pi*or$month/12)
or$Q = lubridate::quarter(or$T_DATE)
#or$sinQ = sin(2*pi*or$Q/4)
#or$cosQ = cos(2*pi*or$Q/4)

require(sdmTMB)
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast = st_make_valid(ns_coast)
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -71, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

spde <- make_mesh(as_tibble(or), xy_cols = c("X1000", "Y1000"),
                  n_knots=500,type = "cutoff_search")
#plot(spde)

# Add on the barrier mesh component:
bspde <- sdmTMBextra::add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)

# fitBias_t = sdmTMB(diff~ s(lz,k=3)+Glor+sinMon+cosMon,
#                     data=as_tibble(or),
#                     mesh=bspde,
#                     time='YR',
#                     family=student(link='identity'),
#                     spatial='on',
#                     spatiotemporal='iid')
# 
# 
# or$resids <- residuals(fitBias_t) # randomized quantile residuals
# qqnorm(or1$resids)
# qqline(or1$resids)
# cAIC(fitBias_t) #316362
# 
# ggplot() + geom_point(data=subset(or1,Q %in% 3),aes(colour=resids,fill=resids,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
#   scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#   scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#   theme_test_adam()
# cAIC(fitBias_t) # 316362
# #####################################################################################################################################
# fitBias_t1 = sdmTMB(diff~ s(lz,k=3)+Glor,
#                        spatial_varying = ~0+sinMon+cosMon,#seasonal cycle on month
#                        data=as_tibble(or),
#                        mesh=bspde,
#                        time='YR',
#                        family=student(link='identity'),
#                        spatial='on',
#                        spatiotemporal='iid')
# or1 = subset(or,is.finite(resids))
# qqnorm(or1$resids)
# qqline(or1$resids)
# 
# cAIC(fitBias_t1) # 270964.2
# 
# v = predict(fitBias_t1,type = 'response')
# 
# ggplot() + geom_point(data=subset(or1,Q %in% 4),aes(colour=resids,fill=resids,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
#   scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#   scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#   theme_test_adam()
# 
# 
# ggplot() + geom_point(data=subset(v,Q %in% 3),aes(colour=est,fill=est,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
#   scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#   scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#   theme_test_adam()
# 
#  
# 
# or$ests = fitBias_svq_t1$family$linkinv(v$est)
# 
# plot(or$resids+or$diff,or$ests)
#####################################################################################################################################
fitBias_t1 = sdmTMB(diff~ s(lz,k=3)+Glor+sinMon+cosMon,
                    spatial_varying = ~0+sinMon+cosMon,#seasonal cycle on month
                    data=as_tibble(or),
                    mesh=bspde,
                    time='YR',
                    family=student(link='identity'),
                    spatial='on',
                    spatiotemporal='iid')
or$residuals = residuals(fitBias_t1)
qqnorm(or$residuals)
qqline(or$residuals)

cAIC(fitBias_t1) # 270876.2
ggplot() + geom_point(data=subset(or,Q %in% 1),aes(colour=residuals,fill=residuals,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
  scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
  theme_test_adam()


v = predict(fitBias_t1,type = 'response')
or$ests = fitBias_t1$family$linkinv(v$est)

plot(or$diff,or$ests)

saveRDS(or,'dataForsdmTMBbiasSurface.rds')
saveRDS(fitBias_t1,'modelForbiasSurface.rds')

or = readRDS('dataForsdmTMBbiasSurface.rds')
fb = readRDS('modelForbiasSurface.rds')


###bias corrected surface
dam = readRDS(file='GlorysTemps2000_24_wClim.rds')
da = st_as_sf(dam,coords = c('X','Y'),crs=4326)
dau = st_transform(da,crs=32620)
xx= st_coordinates(dau)/1000
dau$X1000 = xx[,1]
dau$Y1000 = xx[,2]

st_crs(dau) = 32620
st_geometry(dau) <- NULL
dam = st_as_sf(dau,coords=c('X1000','Y1000'),crs=32620)
id = unique(dam$doy)
rm(dau,da,xx)
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf()
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620
ba1 = ba
st_geometry(ba1) <- NULL
out = list()
for(i in 1:length(id)){
	b = subset(dam,doy==id[i])
	o = st_join(b,rL)
        o = subset(o,!is.na(LFA))
	ss = st_nearest_feature(o,ba)
	o$z = ba1$z[ss]
	out[[i]] = o
}

su = bind_rows(out)
rm(out, dam, ba, ba1i,b)
su$month = lubridate::month(su$Date)
su = subset(su,z>0)
su$lz = log(su$z)
su$sinMon = sin(2*pi*su$month/12)
su$cosMon = cos(2*pi*su$month/12)
su$Q = lubridate::quarter(su$Date)
su$YR = lubridate::year(su$Date)
su$X1000 = st_coordinates(su)[,1]
su$Y1000 = st_coordinates(su)[,2]
su$Glor = su$bottomT

#su = subset(su,YR %in% unique(or$YR))
saveRDS(su,file='predictionSurfaces.rds')
su = readRDS('predictionSurfaces.rds')
sun = as_tibble(su)
years <- unique(su$YR)
require(purrr)
sun$sinDoy = sin(2*pi*sun$doy/365)
sun$cosDoy = cos(2*pi*sun$doy/365)
base_subsets <- map(1:200, function(i) {
			    sun %>%
				        group_by(YR) %>%
					    slice_sample(n = 1, replace = TRUE) %>%
					        ungroup()
		    })

# Step 2: Remove those sampled rows from the original data
sampled_ids <- bind_rows(base_subsets) %>% distinct()
remaining_df <- anti_join(sun, sampled_ids)

# Step 3: Randomly distribute remaining rows across the 200 subsets
remaining_split <- split(remaining_df, rep(1:200, length.out = nrow(remaining_df)))

# Step 4: Combine base samples with remaining rows
final_subsets <- map2(base_subsets, remaining_split, bind_rows)
saveRDS(final_subsets,file='predictionSurfaces_list_doy.rds')

#####
#run for predictions

or = readRDS('dataForsdmTMBbiasSurface.rds')

final_subsets = readRDS(file='predictionSurfaces_list_doy.rds')
or$Doy = lubridate::yday(or$T_DATE)
or$lz = log(or$z)
or$sinDoy = sin(2*pi*or$Doy/365)
or$cosDoy = cos(2*pi*or$Doy/365)
#or$sinQ = sin(2*pi*or$Q/4)
#or$cosQ = cos(2*pi*or$Q/4)

require(sdmTMB)
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast = st_make_valid(ns_coast)
ns_coast <- suppressWarnings(suppressMessages(
					        st_crop(ns_coast,
							          c(xmin = -71, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

spde <- make_mesh(as_tibble(or), xy_cols = c("X1000", "Y1000"),
		                    n_knots=500,type = "cutoff_search")
#plot(spde)

# Add on the barrier mesh component:
bspde <- sdmTMBextra::add_barrier_mesh(
				         spde, ns_coast, range_fraction = 0.1,
					   proj_scaling = 1000, plot = TRUE
					 )

fitBias_t1 = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
		                        spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
					                    data=as_tibble(or),
					                    mesh=bspde,
							                        time='YR',
							                        family=student(link='identity'),
										                    spatial='on',
										                    spatiotemporal='iid')
years = unique(or$YR)
for(i in 1:length(final_subsets)) {
	fs = final_subsets[[i]]
	fs = subset(fs,!is.na(Glor) & YR %in% years)
	g = predict(fitBias_t1,newdata=fs)
	fs$pred = fitBias_t1$family$linkinv(g$est)
	final_subsets[[i]] = fs
	saveRDS(fs, file=paste0('biaspredictions',i,'.rds'))
	rm(fs,g)
}

fi = list.files()
fi = grep('biasp',fi, value=T)
o = list()
for(i in 1:length(fi)){
	o[[i]] = readRDS(file=fi[i])
}

lo = bind_rows(o)
saveRDS(lo,file='Glorys2000-2023wBiasCorrColumn_doy.rds')
