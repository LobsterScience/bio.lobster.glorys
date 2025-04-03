require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(heatwaveR)
require(ggplot2)
require(sdmTMB)
require(sdmTMBextra)
la()
sf_use_s2(FALSE) #needed for cropping
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
#ns_coast <- suppressWarnings(suppressMessages(
#  st_crop(ns_coast,
#          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))


setwd(file.path(project.datadirectory('bio.lobster.glorys'),'Analysis','Marine_Heat_Cold_Waves'))

#prediction surface from Glorys only model
x=readRDS(file='../sdmTMBBerriedModelling/BerriedPredictionSurface.rds')
st_geometry(x) = st_geometry(x)*1000 
st_crs(x) = 32620
x = st_transform(x,4326)

x <- x %>%
  mutate(location_label = as.factor(st_as_text(geometry))) %>%
  group_by(location_label) %>%
  mutate(location_id = cur_group_id()) %>%
  ungroup()
x$location_label <- NULL
x$X = st_coordinates(x)[,1]
x$Y = st_coordinates(x)[,2]
cl = x %>% distinct(location_id,X,Y)

xx = aggregate(pred~location_id+LFA,data=subset(x,LFA %in% c(34, 35,36,37,38)  & YEAR %in% 2010:2020 & Q==3),FUN=mean)

xx = st_as_sf(merge(xx,cl),coords=c('X','Y'),crs=4326)

ggplot(xx) +
  geom_hex(stat='identity',aes(x=st_coordinates(xx)[,1],y=st_coordinates(xx)[,2],fill=pred,color=pred),size=.1) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=subset(rL,LFA %in% c(35,36,37,38,34)),size=1,colour='black',fill=NA)+
  geom_sf(data=ns_coast)+
  theme_test_adam() +
  coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4]))

sx = subset(xx,pred>=.5)

alch = readRDS('cold_warm_thresholds_summarized.rds')

aal = aggregate(cbind(seas,thresh,Scope,coldthresh,coldScope)~location_id,data=subset(alch, mn %in% c('April','May','June')),FUN=mean)
aal = st_as_sf(merge(aal,locs))


nearest_indices <- st_nearest_feature(xx, aal)
aal_nearest <- aal[nearest_indices, ]
st_geometry(aal_nearest) <- NULL
mf <- cbind(xx, aal_nearest)
mf$totalScope = mf$Scope+mf$coldScope
g1 = ggplot()+
  stat_ecdf(data=mf,aes(x=thresh),colour='red',size=1)+
  stat_ecdf(data=subset(mf,pred>=0.5),aes(x=thresh),colour='blue',size=1)+
  theme_minimal()+
  labs(x='Marine Heat Wave Threshold',y='Empirical Cumulative Distribution')
  

g2 = ggplot()+
  stat_ecdf(data=mf,aes(x=seas),colour='red',size=1)+
  stat_ecdf(data=subset(mf,pred>=0.5),aes(x=seas),colour='blue',size=1)+
  theme_minimal()+
  labs(x='Climatology Bottom Temperture',y='Empirical Cumulative Distribution')


g3 = ggplot()+
  stat_ecdf(data=mf,aes(x=Scope),colour='red',size=1)+
  stat_ecdf(data=subset(mf,pred>=0.5),aes(x=Scope),colour='blue',size=1)+
  theme_minimal()+
  labs(x='Scope of Marine Heat Wave Threshold',y='Empirical Cumulative Distribution')

gridExtra::grid.arrange(g2,g1,g3)
ggplot(mf,aes(x=seas,y=Scope))+geom_point()+geom_smooth(method='rlm')

saveRDS(mf,'heatColdBerried.rds')

mh =readRDS('marineheatWave_summary.rds')

mfa = subset(mf,pred>0.5)
ii = unique(mfa$location_id.1)

mha = subset(mh,location_id %in% ii)
mha$yr = lubridate::year(mha$date_start)
mha = subset(mha,yr>2010)

ggplot(mha,aes(x=duration))+
  geom_histogram(bins=100)+
  theme_minimal()+
  labs(x='Duration of Marine Heat Waves',y='Frequency')
summary(mha$duration)

ggplot(mha,aes(x=intensity_mean))+
  geom_histogram(bins=100)+
  theme_minimal()+
  labs(x='Mean Intensity',y='Frequency')
summary(mha$intensity_mean)

ggplot(mha,aes(x=intensity_max))+
  geom_histogram(bins=100)+
  theme_minimal()+
  labs(x='Max Intensity',y='Frequency')
summary(mha$intensity_max)



ggplot(mha,aes(x=rate_onset))+
  geom_histogram(bins=100)+
  theme_minimal()+
  labs(x='Rate Onset C/day',y='Frequency')
summary(mha$rate_onset)
