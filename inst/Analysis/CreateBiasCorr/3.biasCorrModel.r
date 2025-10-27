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
		                    n_knots=500,type = "cutoff_search")
#plot(spde)

# Add on the barrier mesh component:
bspde <- sdmTMBextra::add_barrier_mesh(
				         spde, ns_coast, range_fraction = 0.1,
					   proj_scaling = 1000, plot = TRUE
					 )

fitBias_t1 = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
		                        spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
					                    data=as_tibble(or),
					                    mesh=bspde,
							                        time='YR',
							                        family=student(link='identity'),
										                    spatial='on',
										                    spatiotemporal='iid')
#or$residuals = residuals(fitBias_t1)
#qqnorm(or$residuals)
#qqline(or$residuals)

#cAIC(fitBias_t1) # 270876.2
#ggplot() + geom_point(data=subset(or,Q %in% 1),aes(colour=residuals,fill=residuals,x=X1000,y=Y1000),size=.2)+facet_wrap(~YR)+
#  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#  scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
#  theme_test_adam()


v = predict(fitBias_t1,type = 'response')
or$ests = fitBias_t1$family$linkinv(v$est)

plot(or$diff,or$ests)

saveRDS(or,'dataForsdmTMBbiasSurface_ann.rds')

fitBias_t2 = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
	            spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
	            data=as_tibble(or),
	            mesh=bspde,
	            time='YR',
	            family=student(link='identity'),
	            spatial='on',
	            spatiotemporal='AR1')

v = predict(fitBias_t2,type = 'response')
or$ests2 = fitBias_t2$family$linkinv(v$est)

plot(or$diff,or$ests)

saveRDS(or,'dataForsdmTMBbiasSurface_ann_2.rds')

fitBias_t3 = sdmTMB(diff~ s(lz,k=3)+Glor+sinDoy+cosDoy,
	             spatial_varying = ~0+sinDoy+cosDoy,#seasonal cycle on day
	             data=as_tibble(or),
	              mesh=bspde,
	               #time='YR',
	               family=student(link='identity'),
	               spatial='on')

v = predict(fitBias_t3,type = 'response')
or$ests3 = fitBias_t3$family$linkinv(v$est)

saveRDS(or,'dataForsdmTMBbiasSurface_ann_3.rds')


