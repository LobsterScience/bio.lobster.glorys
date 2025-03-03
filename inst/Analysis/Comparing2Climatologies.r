#cold snap in LFA 33 and LFA 34 Spring 2023 to Winter 2024
require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
la()

setwd(file.path(project.datadirectory('bio.lobster.glorys')))
dir.create('Analysis')
#glorys reshape to r Object

xx = dir('Summary')
xy = xx[grep('-01-01',xx)]
xy = xy[grep('2023|2024',xy)]

xc = xx[grep('clim',xx)]
xl=list()
for(i in 1:length(xy)){
  xl[[i]] = readRDS(file = file.path('Summary',xy[i]))
}

xl = bind_rows(xl)
xl$mn = lubridate::month(xl$Date)
xl$yr = lubridate::year(xl$Date)

xla = aggregate(bottomT~yr+mn+X+Y,data=xl,FUN=mean)

xc = readRDS(file = file.path('Summary',xc))
xc$yr = lubridate::year(xc$Date)
xc$mn = lubridate::month(xc$Date)

m = unique(xc$mn)
xcs = st_as_sf(xc,coords = c('X','Y'),crs=4326)
xls = st_as_sf(xla,coords = c('X','Y'),crs=4326)


#unique locations

xcs <- xcs %>%
  mutate(location_label = as.factor(st_as_text(geometry))) %>%
  group_by(location_label) %>%
  mutate(location_id = cur_group_id()) %>%
  ungroup()
xcs$location_label <- NULL

xls <- xls %>%
  mutate(location_label = as.factor(st_as_text(geometry))) %>%
  group_by(location_label) %>%
  mutate(location_id = cur_group_id()) %>%
  ungroup()
xls$location_label <- NULL

ol = list()
v=0
  xcss = subset(xcs,mn==1)
  xlss = subset(xls, mn==1 & yr==2023)  
  for(j in 1:nrow(xlss)){
    v = v+1
    nn = xlss[j,]
    ou = st_nearest_feature(nn,xcss)
    ds = st_distance(nn,xcss[ou,],by_element=T)
    nn$clim = xcss$bottomT[ou]
    nn$dis = as.numeric(ds)
    nn$climId = xcss$location_id[ou]
    ol[[v]] = nn
    
  }

mn1 = bind_rows(ol)  
dc = mn1 %>% distinct(location_id, climId)

#2023 month 1 is done
out=list()
out[[1]] = subset(mn1,select=c(yr,mn, bottomT, climId,clim,location_id))
for(i in 2:length(m)){
  xcss = subset(xcs,mn==i)
  xlss = subset(xls, mn==i & yr==2023)  
  cv = merge(xlss,dc)
  st_geometry(xcss) <- NULL
  xcss$clim = xcss$bottomT
  out[[i]] =merge(cv,xcss[,c('clim','location_id')],by.x='climId',by.y='location_id')
}

c23 = bind_rows(out)

out=list()

for(i in 1:length(m)){
  xcss = subset(xcs,mn==i)
  xlss = subset(xls, mn==i & yr==2024)  
  cv = merge(xlss,dc)
  st_geometry(xcss) <- NULL
  xcss$clim = xcss$bottomT
  out[[i]] =merge(cv,xcss[,c('clim','location_id')],by.x='climId',by.y='location_id')
}

c24 = bind_rows(out)

clims = bind_rows(c23,c24)

saveRDS(clims,file='ClimatologyGlorys20232024.rds')
#clims = readRDS(file='ClimatologyGlorys20232024.rds')
clims$Anomaly = clims$bottomT - clims$clim

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


