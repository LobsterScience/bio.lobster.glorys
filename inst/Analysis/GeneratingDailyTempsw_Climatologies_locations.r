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
filtered_files <- files[years >= 2000]
# Read the filtered files into R
data_list <- lapply(paste('Summary',filtered_files,sep="/"), readRDS)
da = bind_rows(data_list)
da$yr = lubridate::year(da$Date)
da$woy = lubridate::week(da$Date)
da$doy = lubridate::yday(da$Date)
saveRDS(da,file="GlorysTemps2000_2024.rds")
da = readRDS('GlorysTemps2000_2024.rds') 
das = st_as_sf(da,coords=c('X','Y'),crs=4326)

if(redo.clim){
filtered_files <- files
# Read the filtered files into R
data_list <- lapply(paste('Summary',filtered_files,sep="/"), readRDS)
da = bind_rows(data_list)
da$yr = lubridate::year(da$Date)
da$doy = lubridate::yday(da$Date)
#weekly climatology 1994-2016
daa = aggregate(bottomT~X+Y+doy,data=subset(da,yr<2017),FUN=mean)
names(daa)[4] = 'climT'
saveRDS(daa,file='DailyClimatology1993_2016.rds')
}
daa = readRDS(file='DailyClimatology1993_2016.rds')
daas = st_as_sf(daa,coords=c('X','Y'),crs=4326)


#merge data with clim
# Spatial join + filter by DOY
joined <- das %>%
  rowwise() %>%
  mutate(match = list(daas[daas$doy == doy,][which.min(st_distance(geometry, daas$geometry))])) %>%
  unnest(match)

