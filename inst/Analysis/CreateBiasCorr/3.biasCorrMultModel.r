require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(tidyr)
require(sf)
require(ggplot2)
require(data.table)
require(sdmTMB)

la()


setwd(file.path(project.datadirectory('bio.lobster.glorys')))

or = readRDS('dataForsdmTMBbiasSurface.rds')

or$lz = log(or$z)

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast = st_make_valid(ns_coast)
ns_coast <- suppressWarnings(suppressMessages(
					        st_crop(ns_coast,
							          c(xmin = -71, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)


spde <- make_mesh(as_tibble(or), xy_cols = c("X1000", "Y1000"),
		                    cutoff=24)
#plot(spde)

# Add on the barrier mesh component:
bspde <- sdmTMBextra::add_barrier_mesh(
				         spde, ns_coast, range_fraction = 0.1,
					   proj_scaling = 1000, plot = FALSE
					 )
or$Q = lubridate::quarter(or$T_DATE)

#ggplot()+geom_sf(data=subset(or),aes(fill=diff,colour=diff))+
#  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#    scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#    theme_test_adam()+facet_wrap(~Q)

#CV splits  
or$IDS = "I"
or1 = as_tibble(or)
or1 = cv_SpaceTimeFolds(or1,idCol = 'IDS', nfolds=5)
path=file.path('Model_outputs/models')
dir.create(path,recursive = T)
source(('~/git/Framework33_34_41/SpatialModelling/setupMultimodelTable.r'))
source(file.path('~/git/Framework_LFA33_34_41/SpatialModelling/setupMultimodelTable.r'))

models = c('m1','m2','m3','m4','m5','m6','m7')
################################################################################################################################
if('m1' %in% models){
  mod.label <- "m1" 
  
  m = sdmTMB(diff~ Glor+sinDoy+cosDoy,
                      spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
                      data=as_tibble(or1),
                      mesh=bspde,
                      time='YR',
                      family=student(link='identity'),
                      spatial='on',
                      spatiotemporal='ar1')
  
  m_cv <- sdmTMB_cv(
    diff~ Glor+sinDoy+cosDoy,
    spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
    data=as_tibble(or1),
    mesh=bspde,
    time='YR',
    family=student(link='identity'),
    spatial='on',
    spatiotemporal='ar1',
    fold_ids='fold_id',
    k_folds = 5
  )
  m1 = m
  ca <-mod.select.fn()
  mod.select <- rbind(mod.select, ca)
  saveRDS(m,file=file.path(path,paste0('biasCorr_',mod.label,'.rds')))
  saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}

################################################################################################################################
if('m2' %in% models){
  mod.label <- "m2" 
  
  m = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
             data=as_tibble(or1),
             mesh=bspde,
             time='YR',
             family=student(link='identity'),
             spatial='on',
             spatiotemporal='iid')
  
  m_cv <- sdmTMB_cv(
    diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
    data=as_tibble(or1),
    mesh=bspde,
    time='YR',
    family=student(link='identity'),
    spatial='on',
    spatiotemporal='iid',
    fold_ids='fold_id',
    k_folds = 5
  )
  m2 = m
  ca <-mod.select.fn()
  mod.select <- rbind(mod.select, ca)
  saveRDS(m,file=file.path(path,paste('biasCorr_',mod.label,'.rds')))
  saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}



################################################################################################################################
if('m3' %in% models){
  mod.label <- "m3" 
  
  m = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
             data=as_tibble(or1),
             mesh=bspde,
             family=student(link='identity'),
             spatial='on',)
             
  m_cv <- sdmTMB_cv(
    diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
    data=as_tibble(or1),
    mesh=bspde,
    family=student(link='identity'),
    spatial='on',
    fold_ids='fold_id',
    k_folds = 5
  )
  m3 = m
  ca <-mod.select.fn()
  mod.select <- rbind(mod.select, ca)
  saveRDS(m,file=file.path(path,paste('biasCorr_',mod.label,'.rds')))
  saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}
################################################################################################################################
if('m4' %in% models){
  mod.label <- "m4" 
  
  m = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
             spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
             data=as_tibble(or1),
             mesh=bspde,
             time='YR',
             family=student(link='identity'),
             spatial='on',
             spatiotemporal='ar1')
  
  m_cv <- sdmTMB_cv(
    diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
    spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
    data=as_tibble(or1),
    mesh=bspde,
    time='YR',
    family=student(link='identity'),
    spatial='on',
    spatiotemporal='ar1',
    fold_ids='fold_id',
    k_folds = 5
  )
  m4 = m
  ca <-mod.select.fn()
  mod.select <- rbind(mod.select, ca)
  saveRDS(m,file=file.path(path,paste0('biasCorr_',mod.label,'.rds')))
  saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}

################################################################################################################################
if('m5' %in% models){
  mod.label <- "m5" 
  
  m = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
             data=as_tibble(or1),
             mesh=bspde,
             time='YR',
             family=student(link='identity'),
             spatial='on',
             spatiotemporal='ar1')
  
  m_cv <- sdmTMB_cv(
    diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
    data=as_tibble(or1),
    mesh=bspde,
    time='YR',
    family=student(link='identity'),
    spatial='on',
    spatiotemporal='ar1',
    fold_ids='fold_id',
    k_folds = 5
  )
  m5 = m
  ca <-mod.select.fn()
  mod.select <- rbind(mod.select, ca)
  saveRDS(m,file=file.path(path,paste0('biasCorr_',mod.label,'.rds')))
  saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}
###############################################################################################################################
################################################################################################################################
if('m6' %in% models){
	  mod.label <- "m6"

  m = sdmTMB(diff~ s(lz,k=3)+Glor,
			               data=as_tibble(or1),
			               mesh=bspde,
				                    time='YR',
				                    family=student(link='identity'),
						                 spatial='on',
						                 spatiotemporal='ar1')
  
    m_cv <- sdmTMB_cv(
		          diff~ s(lz,k=3)+Glor,
			      data=as_tibble(or1),
			          mesh=bspde,
			          time='YR',
				      family=student(link='identity'),
				      spatial='on',
				          spatiotemporal='ar1',
				          fold_ids='fold_id',
					      k_folds = 5
					    )
    m6 = m
      ca <-mod.select.fn()
      mod.select <- rbind(mod.select, ca)
        saveRDS(m,file=file.path(path,paste0('biasCorr_',mod.label,'.rds')))
        saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}

################################################################################################################################
mod.select=readRDS(mod.select,file=file.path(path,'model_selection.rds'))
if('m7' %in% models){
	  mod.label <- "m7"

  m = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
	                  spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
			               data=as_tibble(or1),
			               mesh=bspde,
				                    time='YR',
				                    family=gaussian(link='identity'),
						                 spatial='on',
						                 spatiotemporal='ar1')
    m_cv <- sdmTMB_cv(
		          diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
			      spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
			      data=as_tibble(or1),
			          mesh=bspde,
			          time='YR',
				      family=gaussian(link='identity'),
				      spatial='on',
				          spatiotemporal='ar1',
				          fold_ids='fold_id',
					      k_folds = 5
					    )
    m7 = m
      ca <-mod.select.fn()
      mod.select <- rbind(mod.select, ca)
        saveRDS(m,file=file.path(path,paste0('biasCorr_',mod.label,'.rds')))
        saveRDS(mod.select,file=file.path(path,'model_selection.rds'))
}

