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
redoList = F
if(redoList){
dam = readRDS('GlorysTemps_Depth2000_2025.rds') 
dam = st_transform(dam,crs=32620)
xx= st_coordinates(dam)/1000
dam$X1000 = xx[,1]
dam$Y1000 = xx[,2]
st_crs(dam) = 32620
st_geometry(dam) <- NULL
dam = subset(dam,z>0)
dam$lz = log(dam$z)
dam$YR = dam$yr
dam$Glor = dam$bottomT

sun = as_tibble(dam)
years <- unique(sun$YR)

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
saveRDS(final_subsets,file='predictionSurfaces_list_doy.rds')}
final_subsets = readRDS('predictionSurfaces_list_doy.rds')
#read model outputs

t = readRDS(file=file.path(paste0('final_model_biasCorr_m4.rds')))
or = t[[2]]
m4 = t[[1]]

years = unique(or$YR)
for(i in 1:length(final_subsets)) {
	fs = final_subsets[[i]]
	fs = subset(fs,!is.na(Glor) & YR %in% years)
	g = predict(m4,newdata=fs)
	fs$pred = m4$family$linkinv(g$est)
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
