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
dam$X = xx[,1]
dam$Y = xx[,2]
st_crs(dam) = 32620

locations_sf <- dam |>
  dplyr::group_by(location_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


locations_df <- locations_sf |>
  sf::st_transform(crs=32620) 

sf::st_geometry(locations_df) =  sf::st_geometry(locations_df)/1000
coords <- sf::st_coordinates(locations_df)

locations_df <- locations_df |> 
  sf::st_drop_geometry() |>
  dplyr::mutate(
    X = coords[, 1],
    Y = coords[, 2]
  )


t=readRDS(file=file.path(paste0('final_model_biasCorr_m5_june10.rds')))

#t = readRDS(file=file.path(paste0('final_model_biasCorr_m4.rds')))
or = t[[2]]
or$X = or$X1000
or$Y = or$Y1000

obs_xy <- as.matrix(locations_df[, c("X", "Y")])

pred_xy <- as.matrix(or[, c("X", "Y")])

nn <- FNN::get.knnx(pred_xy,obs_xy, k = 1)
pred_grid <- locations_df[nn$nn.dist < 20, ]
damr = subset(dam, location_id %in% pred_grid$location_id)
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
saveRDS(final_subsets,file='predictionSurfaces_list_doy.rds')

}
final_subsets = readRDS('predictionSurfaces_list_doy.rds')
#read model outputs
t=readRDS(file=file.path(paste0('final_model_biasCorr_m5_june10.rds')))

#t = readRDS(file=file.path(paste0('final_model_biasCorr_m4.rds')))
or = t[[2]]
m4 = t[[1]]

nn <- FNN::get.knnx(or, pred_xy, k = 1)

pred_grid <- pred_grid[nn$nn.dist < 20000, ]

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
saveRDS(lo,file='Glorys2000-2025wBiasCorrColumn_doy_june15.rds')
lo = readRDS(file='Glorys2000-2025wBiasCorrColumn_doy_june15.rds')


#allocate bias corrs to grids
gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolys_DepthPruned_37Split.rds'))
gr41 = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41_grid_polys.rds')))
gr$GRID_NO = as.numeric(gr$GRID_NO)
gr41$LFA = as.character(gr41$LFA)
gtot = bind_rows(gr,gr41)
gtot = st_transform(gtot,crs=32620)
st_geometry(gtot) <- st_geometry(gtot)/1000
st_crs(gtot) <- 32620
#data by grid

dass = st_as_sf(lo, coords=c('X1000','Y1000'),crs=32620)

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
daa = subset(dag,!is.na(LFA))
saveRDS(daa,file='Glorys2000_2025wBiasCorrColumn_doy_grid_june15.rds')
daa = readRDS(file='Glorys2000_2025wBiasCorrColumn_doy_grid_june15.rds')

#or$diff = or$TEMP - or$Glor
daa$bcT = daa$Glor+daa$pred

daz = aggregate(z~LFA+GRID_NO,data=daa,FUN=function(x) c(mean(x,na.rm=T), sd(x,na.rm=T)))
daT = aggregate(bcT~LFA+GRID_NO+doy+yr+Date,data=daa,FUN=function(x)quantile(x,c(0.025,0.25,0.5,0.75,0.975)))

dazt = merge(daT,daz,all=T)
i = which(dazt$bcT[,3]< -1.5)
dazt$bcT[i,] <- NA
saveRDS(dazt,file='Glorys2000_2025wBiasCorrColumn_doy_grid_agg_june15.rds')

