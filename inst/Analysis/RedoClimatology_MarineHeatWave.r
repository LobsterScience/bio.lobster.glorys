require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(heatwaveR)
la()

setwd(file.path(project.datadirectory('bio.lobster.glorys'),'Analysis','Marine_Heat_Cold_Waves'))
if(redo){
#glorys reshape to r Object
setwd(file.path(project.datadirectory('bio.lobster.glorys')))
  
xx = dir('Summary')
xy = xx[grep('-01-01',xx)]
pattern = "GLORYS(199[3-9]|200[0-9]|201[0-6])-01-01\\.nc_ShelfBoF\\.rds"
xy = xy[grep(pattern,xy)]

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

saveRDS(xl,'marineHeatWater_pre_processing.rds')
xl = readRDS('marineHeatWave_pre_processing.rds')
ii = unique(xl$location_id)

opc = ouc = oph = ouh = list()

m=0
for(i in 59:length(ii)) {
  print(i)
  x = subset(xl,location_id==ii[i],select=c(Date,bottomT,location_id))
  xloc = st_coordinates(x[1,])
  st_geometry(x) <- NULL
  x$t = x$Date
  x$temp = x$bottomT
  if(nrow(subset(x,!is.na(temp)))>0){
    m=m+1      
    x = x[,c('t','temp','location_id')]
    g = ts2clm3(x,climatologyPeriod = c('1994-01-01','2016-12-31'),pctile=90 )
    g1 = ts2clm3(x,climatologyPeriod = c('1994-01-01','2016-12-31'),pctile=10  )
    gd = detect_event(g)
    gd_1 = gd[[1]]
    gd_1 = gd_1[!duplicated(gd_1),]
    gd_2 = gd[[2]]
    
    gd1 = detect_event(g1)
    gd1_1 = gd1[[1]]
    gd1_1 = gd1_1[!duplicated(gd1_1),]
    gd1_2 = gd1[[2]]
    
    gd_1$X = gd_2$X = gd1_1$X = gd1_2$X = xloc[1]
    gd_1$Y = gd_2$Y = gd1_1$Y = gd1_2$Y = xloc[2]
    gd_2$location_id = ii[i]
    gd1_2$location_id = ii[i]
    
    opc[[m]] = gd1_1
    oph[[m]] = gd_1
    
    ouc[[m]] = gd1_2
    ouh[[m]] = gd_2
    
  }
}

output_mcw = bind_rows(opc )
summary_mcw = bind_rows(ouc )
output_mhw = bind_rows(oph)
summary_mhw = bind_rows(ouh)

saveRDS(output_mcw,file='marinecoldWave_post_processing.rds')
saveRDS(summary_mcw,file='marinecoldWave_summary.rds')
saveRDS(output_mhw,file='marineheatWave_post_processing.rds')
saveRDS(summary_mhw,file='marineheatWave_summary.rds')
}

###climatology mins a maxs for seasonal cycle
oh = readRDS(file='marineheatWave_post_processing.rds')
oh = subset(oh,yr==1994)

oha = oh %>%
      group_by(location_id, X,Y) %>%
      summarise(
        mint = min(seas),
        maxt = max(seas)
      )
ohas = st_as_sf(oha,coords=c('X','Y'),crs=4326)
ohas$Range = ohas$maxt - ohas$mint

ggplot(ohas,aes(fill=Range,colour=Range))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  theme_test_adam()

#######################


smhw = readRDS(file='marineheatWave_summary.rds')
smhw$Mon = lubridate::month(smhw$date_peak)





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


