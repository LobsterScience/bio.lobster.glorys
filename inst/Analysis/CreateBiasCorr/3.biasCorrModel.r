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


#chosen from 3.biasCorrMultModel.r

  m4 = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
             spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
             data=as_tibble(or),
             mesh=bspde,
             time='YR',
             family=student(link='identity'),
             spatial='on',
             spatiotemporal='ar1')
#or$residuals = residuals(m4)
#qqnorm(or$residuals)
#qqline(or$residuals)

#ggplot() + geom_point(data=subset(or,Q %in% 1),aes(colour=residuals,fill=residuals,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
#  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#  scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#  theme_test_adam()


v = predict(m4,type = 'response')
or$ests = m4$family$linkinv(v$est)

plot(or$diff,or$ests)

saveRDS(list(m4,or),file=file.path(paste0('final_model_biasCorr_m4.rds')))
