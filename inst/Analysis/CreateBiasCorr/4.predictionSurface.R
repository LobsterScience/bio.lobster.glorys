require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(ggplot2)
require(data.table)
require(sdmTMB)
require(purrr)
la()


setwd(file.path(project.datadirectory('bio.lobster.glorys')))


###bias corrected surface

dam = readRDS(file='GlorysTemps2000_25_wClim.rds')
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
su$YR = lubridate::year(su$Date)
su$X1000 = st_coordinates(su)[,1]
su$Y1000 = st_coordinates(su)[,2]
su$Glor = su$bottomT

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

years = unique(or$YR)
for(i in 1:length(final_subsets)) {
	fs = final_subsets[[i]]
	fs = subset(fs,!is.na(Glor) & YR %in% years)
	g = predict(fitBias_t2,newdata=fs)
	fs$pred = fitBias_t1$family$linkinv(g$est)
	final_subsets[[i]] = fs
	saveRDS(fs, file=paste0('biaspredictions',i,'.rds'))
	rm(fs,g)
}

##combine after running
fi = list.files()
fi = grep('biasp',fi, value=T)
o = list()
for(i in 1:length(fi)){
	o[[i]] = readRDS(file=fi[i])
}

lo = bind_rows(o)
saveRDS(lo,file='Glorys2000-2025wBiasCorrColumn_doy.rds')
