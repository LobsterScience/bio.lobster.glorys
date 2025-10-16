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


daa = readRDS(file='Glorys2000-2023wBiasCorrColumn_doy.rds')
das = st_as_sf(daa,coords=c('X1000','Y1000'),crs=32620)

#lobstergrids

#gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridGroupings_DepthPruned_37Split.rds'))
gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolys_DepthPruned_37Split.rds'))
gr41 = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41_grid_polys.rds')))
gr$GRID_NO = as.numeric(gr$GRID_NO)
gr41$LFA = as.character(gr41$LFA)
gtot = bind_rows(gr,gr41)
gtot = st_transform(gtot,crs=32620)
st_geometry(gtot) = st_geometry(gtot)/1000
st_crs(gtot) = 32620

dag = st_join(das,gtot)
dags = subset(dag,!is.na(LFA.y) & !is.na(GRID_NO))
saveRDS(dags,file='Glorys2000-2023wBianCorrColum_LFAGrids_doy.rds')




