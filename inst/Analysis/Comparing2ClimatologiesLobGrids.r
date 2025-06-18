#cold snap in LFA 33 and LFA 34 Spring 2023 to Winter 2024
require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
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

#weekly climatology 1994-2016
daa = aggregate(bottomT~X+Y+woy,data=subset(da,yr<2017),FUN=mean)
names(daa)[4] = 'climT'
saveRDS(daa,file='WeeklyClimatology1993_2016.rds')

#weekly climatology by grid
xx = aggregate(seas~woy+LFA+GRID_NO,data=rc2,FUN=mean)



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



