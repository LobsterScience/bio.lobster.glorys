require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(ggplot2)
la()


#deviations by year

setwd(file.path(project.datadirectory('bio.lobster.glorys')))

# List all files
# List all files
files <- list.files(path = dir(), pattern = "GLORYS(_int)?\\d{4}-\\d{2}-\\d{2}\\.nc_ShelfBoF\\.rds", full.names =F)

# Extract the year from filenames
years <- as.numeric(sub("GLORYS(_int)?(\\d{4})-.*", "\\2", files))


# Filter files from 2005 onward
filtered_files <- files[years >= 2005]
# Read the filtered files into R
data_list <- lapply(paste('Summary',filtered_files,sep="/"), readRDS)
da = bind_rows(data_list)
da$yr = lubridate::year(da$Date)
da$woy = lubridate::week(da$Date)
save(da,file="GlorysTemps2005_2024.rds")

filtered_files <- files
# Read the filtered files into R
data_list <- lapply(paste('Summary',filtered_files,sep="/"), readRDS)
da = bind_rows(data_list)
da$yr = lubridate::year(da$Date)
da$woy = lubridate::week(da$Date)
#weekly climatology 1994-2016
daa = aggregate(bottomT~X+Y+woy,data=subset(da,yr<2017),FUN=mean)
names(daa)[4] = 'climT'
saveRDS(daa,file='WeeklyClimatology1993_2016.rds')

daa = readRDS(file='WeeklyClimatology1993_2016.rds')
das = st_as_sf(daa,coords=c('X','Y'),crs=4326)

#lobstergrids

#gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridGroupings_DepthPruned_37Split.rds'))
gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolys_DepthPruned_37Split.rds'))
gr41 = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41_grid_polys.rds')))
gr$GRID_NO = as.numeric(gr$GRID_NO)
gr41$LFA = as.character(gr41$LFA)
gtot = bind_rows(gr,gr41)
dag = st_join(das,gtot)
dags = subset(dag,!is.na(LFA) & !is.na(GRID_NO))

dagA = aggregate(climT~LFA+GRID_NO+woy,data=dags,FUN=function(x)quantile(x,c(0.025,0.25,0.5,0.75,0.975)))
grr = merge(dagA,gtot)
saveRDS(grr,file='WeeklyClimatology1993_2016_byGrid.rds')

#data by grid grouping
dass = st_as_sf(da,coords=c('X','Y'),crs=4326)
dag = st_join(dass,gtot)



ggplot(subset(ar,yr==2023),aes(fill=Anomaly,colour=Anomaly))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title=2024)


require(ggplot2)
ggplot(subset(rc2,yr==2023),aes(fill=Anomaly,colour=Anomaly))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title=2023)


ggplot(subset(rc2,yr==2024),aes(fill=Anomaly,colour=Anomaly))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  facet_wrap(~mn)+
  theme_test_adam()+
  labs(title=2024)



