require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(heatwaveR)
require(ggplot2)
la()
sf_use_s2(FALSE) #needed for cropping
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
#ns_coast <- suppressWarnings(suppressMessages(
#  st_crop(ns_coast,
#          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))


setwd(file.path(project.datadirectory('bio.lobster.glorys'),'Analysis','Marine_Heat_Cold_Waves'))

smhw = readRDS(file='marineheatWave_summary.rds')
smhw$Mon = lubridate::month(smhw$date_peak)
smhw$Yr = lubridate::year(smhw$date_peak)

#location Data
    locs = smhw %>% distinct(location_id,X,Y)
    locs = st_as_sf(locs,coords=c('X','Y'),crs=4326)
    
    #constrain within LFAs
    rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
    rL = st_as_sf(rL)
    st_crs(rL) <- 4326
    lol = st_as_sf(locs,coords=c('X','Y'),crs=4326)
    fl = st_join(lol,rL,join=st_within)
    fl = subset(fl,!is.na(LFA))
    
    #add bathy
    
    ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
    ba = ba %>% st_as_sf() 
    ba = st_transform(ba,crs=4326) 
    ss = st_nearest_feature(locs,ba)
    st_geometry(ba) = NULL
    locs$z = ba$z[ss]

    ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
    ba = ba %>% st_as_sf() 
    ba = st_transform(ba,crs=4326) 
    ss = st_nearest_feature(fl,ba)
    st_geometry(ba) = NULL
    fl$z = ba$z[ss]
    
    

smhw <- smhw %>%
  mutate(season = sapply(date_peak, get_season))

ga = aggregate(cbind(duration, intensity_cumulative)~Yr+season+location_id+X+Y,data=smhw,FUN=sum)

fullgrid <- expand.grid(
  Yr = unique(ga$Yr),
  season = unique(ga$season),
  location_id = unique(ga$location_id)
)

dfe <- fullgrid %>%
  left_join(select(ga,c(Yr,season,location_id,duration,intensity_cumulative)), by = c("Yr", "season", "location_id")) %>%
  replace_na(list(duration = 0,intensity_cumulative=0))

dfe = merge(dfe,fl)

gas = st_as_sf(dfe,coords=c('X','Y'),crs=4326)

gasr <- suppressWarnings(suppressMessages(
  st_crop(gas,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47))))


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

ff = st_join(gasr,rL,join=st_within)
ff1 = subset(ff,!is.na(LFA))


ggplot()+
geom_sf(data=subset(gasr,Yr==2024),aes(fill=duration,colour=duration),size=1)+
  scale_color_gradient(low = "white", high = "blue")+
  scale_fill_gradient(low = "white", high = "blue")+
  geom_sf(data=ns_coast,colour='grey')+
  facet_wrap(~season)+
  theme_test_adam()+
  labs(title=2024)


####climatology and thresholds

a = readRDS(file='marineHeatWave_post_processing.rds')
a$yr = lubridate::year(a$t)
a = subset(a,yr==2016)
a$mn = months(a$t)
a <- a %>%
  mutate(mn = factor(mn, levels = c("January", "February", "March", "April", "May", "June", 
                                          "July", "August", "September", "October", "November", "December")))
aa = aggregate(cbind(seas,thresh)~location_id+mn,data=a,FUN=mean)

al = merge(aa,locs)
al = st_as_sf(al)
al <- suppressWarnings(suppressMessages(
  st_crop(al,
          c(xmin = -67.3334, ymin = 42.667, xmax = -63.6667, ymax = 45.75))))

bb = st_bbox(al)

al$Scope = al$thresh-al$seas
al = subset(al,z>0)

ggplot()+
  geom_hex(data=al,stat='identity',aes(x=st_coordinates(al)[,1],y=st_coordinates(al)[,2],fill=seas,color=seas),size=.1)+
  #geom_sf(data=al,aes(fill=thresh,colour=thresh),size=1)+
  scale_color_gradient(low = "white", high = "blue",)+
  scale_fill_gradient(low = "white",high = "blue")+
  geom_sf(data=ns_coast,colour='grey')+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title='Climatology')+
  coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4]))


ggplot()+
  geom_hex(data=al,stat='identity',aes(x=st_coordinates(al)[,1],y=st_coordinates(al)[,2],fill=thresh,color=thresh),size=.1)+
  scale_color_gradient(low = "white", high = "blue")+
  scale_fill_gradient(low = "white", high = "blue")+
  geom_sf(data=ns_coast,colour='grey')+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title='Upper Threshold')+
  coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4]))


ggplot()+
  geom_hex(data=al,stat='identity',aes(x=st_coordinates(al)[,1],y=st_coordinates(al)[,2],fill=Scope,color=Scope),size=.1)+
  #geom_sf(data=al,aes(fill=thresh,colour=thresh),size=1)+
  scale_color_gradient(low = "white", high = "blue",)+
  scale_fill_gradient(low = "white", high = "blue")+
  geom_sf(data=ns_coast,colour='grey')+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title='Upper Threshold - Climatology')+
  coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4]))


#join hot and cold thresholds

names(acold)[c(4,7)] = paste0('cold',names(acold)[c(4,7)])
st_geometry(acold) <- NULL
alch = merge(al,acold[,c('location_id','seas','coldthresh','coldScope')])
alch$totalScope = alch$Scope+alch$coldScope


ggplot()+
  geom_hex(data=alch,stat='identity',aes(x=st_coordinates(alch)[,1],y=st_coordinates(alch)[,2],fill=totalScope,color=totalScope),size=.1)+
  #geom_sf(data=al,aes(fill=thresh,colour=thresh),size=1)+
  scale_color_gradient(low = "white", high = "blue",)+
  scale_fill_gradient(low = "white", high = "blue")+
  geom_sf(data=ns_coast,colour='grey')+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title='Upper Threshold - Lower Threshold')+
  coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4]))


saveRDS(alch,'cold_warm_thresholds_summarized.rds')

#just within LFA



afl = st_join(al,rL,join=st_within)
afl = subset(afl,!is.na(LFA))
saveRDS(afl,'ClimatologyMHWThresolds.rds')

