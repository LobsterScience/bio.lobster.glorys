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


# Filter files from 2000 onward
filtered_files <- files[years >= 2000]
# Read the filtered files into R
data_list <- lapply(paste('Summary',filtered_files,sep="/"), readRDS)
da = bind_rows(data_list)
da$yr = lubridate::year(da$Date)
#da$woy = lubridate::week(da$Date)
nda$doy = lubridate::yday(da$Date)
da = subset(da,!is.na(bottomT))
saveRDS(da,file="GlorysTemps2000_2025.rds")
da = readRDS('GlorysTemps2000_2025.rds') 
da = st_as_sf(da,coords=c('X','Y'),crs=4326)
#add bathy
gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','bathymetrySF.rds')) 
gs = st_as_sf(gr)
g3 = st_transform(gs,crs=4326)

require(nngeo)
require(dplyr)

da=  da %>%
	group_by(geometry) %>%
	mutate(location_id = cur_group_id()) %>%
	ungroup()
dai = da[,c('location_id','geometry')]
require(data.table)
ui = unique(as.data.table(dai),by='location_id')

uis = st_as_sf(ui)

nidx = st_nn(uis,g3,k=1,progress=T)
nidxx = unlist(nidx)

ui$z = g3$z[nidxx]
ui$geometry <- NULL

dau = merge(da,ui)
saveRDS(dau,'GlorysTemps_Depth2000_2025.rds')

#       



daa = readRDS(file='DailyClimatology1993_2016.rds')
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

dagA = aggregate(climT~LFA+GRID_NO+doy,data=dags,FUN=function(x)quantile(x,c(0.025,0.25,0.5,0.75,0.975)))
grr = merge(dagA,gtot)
saveRDS(grr,file='DailyClimatology1993_2016_byGrid.rds')
grr =readRDS(file='DailyClimatology1993_2016_byGrid.rds')
grr$geometry <- NULL
grr1 = grr[!duplicated(grr),]

#data by grid 
dass = st_as_sf(da,coords=c('X','Y'),crs=4326)

st_agr(dass) <- "constant"
st_agr(gtot) <- "constant"

# Break into chunks if needed
chunk_size <- 10000
n_chunks <- ceiling(nrow(dass) / chunk_size)

# Initialize result list
results <- vector("list", n_chunks)

# Loop through chunks
for (i in seq_len(n_chunks)) {
	  cat("Processing chunk", i, "of", n_chunks, "\n")
  idx <- ((i - 1) * chunk_size + 1):(min(i * chunk_size, nrow(dass)))
    chunk <- dass[idx, ]
    
    # Spatial join for the chunk
    r <- st_join(chunk, gtot, join = st_within)
    results[[i]] = subset(r,!is.na(GRID_NO))
}

# Combine results
dag <- bind_rows(results)

#dag = st_join(dass,gtot)
#daga = subset(dag,!is.na(GRID_NO))
dagaa = aggregate(bottomT~LFA+GRID_NO+doy+yr,data=dag,FUN=function(x)quantile(x,c(0.025,0.25,0.5,0.75,0.975)))
grra = merge(dagaa,gtot)
saveRDS(grra,file='DailyTemp_by_grid_00-25.rds')
grra = st_as_sf(readRDS(file='DailyTemp_by_grid_05-24.rds'))


#merge clim and data by grid

grb = merge(grra,grr1,by=c('LFA','GRID_NO','doy'))
grb$Anomaly = grb$bottomT[,3] - grb$climT[,3]
saveRDS(grb,file='DailyTemp_by_grid_00-25_withAnom.rds')



ggplot(subset(grb,yr==2023& doy %in% 10:16),aes(fill=Anomaly,colour=Anomaly))+geom_sf(size=1)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  facet_wrap(~woy)+
  theme_test_adam()+
  labs(title=2024)




