require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(heatwaveR)
la()

setwd(file.path(project.datadirectory('bio.lobster.glorys')))
dir.create('Analysis')
#glorys reshape to r Object

xx = dir('Summary')
xy = xx[grep('-01-01',xx)]
#pattern = "GLORYS(199[3-9]|200[0-9]|201[0-6])-01-01\\.nc_ShelfBoF\\.rds"
#xy = xy[grep(pattern,xy)]

xl=list()
for(i in 1:length(xy)){
  xl[[i]] = readRDS(file = file.path('Summary',xy[i]))
}

xl = bind_rows(xl)
xl = st_as_sf(xl,coords = c('X','Y'),crs=4326)


#unique locations
xl <- xl %>%
  mutate(location_label = as.factor(st_as_text(geometry))) %>%
  group_by(location_label) %>%
  mutate(location_id = cur_group_id()) %>%
  ungroup()
xl$location_label <- NULL
xl$Date = as.Date(xl$Date)

saveRDS(xl,'marineHeatWave_pre_processing.rds')
xl = readRDS('marineHeatWave_pre_processing.rds')
ii = unique(xl$location_id)

ou = list()
m=1160
for(i in 6581:length(ii)){
  print(i)
    x = subset(xl,location_id==ii[i],select=c(Date,bottomT,location_id))
    xloc = st_coordinates(x[1,])
    st_geometry(x) <- NULL
    x$t = x$Date
    x$temp = x$bottomT
    if(nrow(subset(x,!is.na(temp)))>0){
    m=m+1      
    x = x[,c('t','temp','location_id')]
    g = ts2clm3(x,climatologyPeriod = c('1994-01-01','2016-12-31'),pctile=c(0.9)  )
    g1 = ts2clm3(x,climatologyPeriod = c('1994-01-01','2016-12-31'),pctile=c(0.1)  )
    g = subset(g,!is.na(temp))
    g = g[!duplicated(g)]
    g1 = subset(g1,!is.na(temp))
    g1 = g1[!duplicated(g1)]
    bb = merge(as.data.frame(g),as.data.frame(g1[,c('t','location_id','thresh')]),by=c('t','location_id'))
    bb$X = xloc[1]
    bb$Y = xloc[2]
    ou[[m]] = bb
    }
}

out2 = bind_rows(ou)

saveRDS(out2,file='marineHeatWave_post_processing.rds')
out2 = readRDS(file='marineHeatWave_post_processing.rds')
ii = unique(out2$location_id)

v = subset(out2,location_id==ii[1])
v$doy = lubridate::yday(v$t)
v$yr = lubridate::year(v$t)

require(ggplot2)
ggplot(subset(clims,yr==2023),aes(fill=Anomaly,colour=Anomaly))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title=2023)


ggplot(subset(clims,yr==2024),aes(fill=Anomaly,colour=Anomaly))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title=2024)


