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
fig_dir = file.path('C:/Users/cooka/OneDrive - DFO-MPO/LFA33_34_41_Framework/Documents/Figures/BiasCorrGlor')
dir.create(fig_dir,showWarnings = F)
or = readRDS('dataForsdmTMBbiasSurface.rds')
st_geometry(or) = st_geometry(or)*1000
st_crs(or) = 32620

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast = st_make_valid(ns_coast)
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -61, ymax = 46.5))))
ns_coast <- st_transform(ns_coast, crs_utm20)

ggplot()+geom_sf(data=ns_coast)+ geom_sf(data=or,pch=".")
ggsave(file.path(fig_dir,'MapOfObservations.png'))

or$Q = lubridate::quarter(or$T_DATE)

quarter_to_months <- function(q) {
  switch(as.character(q),
         "1" = c("Jan-Mar"),
         "2" = c("Apr-Jun"),
         "3" = c("Jul-Sep"),
         "4" = c("Oct-Dec"))
}
or$MN = unlist(lapply(or$Q,FUN=function(x) quarter_to_months(x)))

or$Anomaly = or$diff
hist(or$Anomaly)
ggplot()+geom_sf(data=ns_coast)+geom_sf(data=subset(or),aes(fill=Anomaly,colour=Anomaly),size=.8)+
  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
    scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
    theme_test_adam()+facet_wrap(~as.factor(MN))
ggsave(file.path(fig_dir,'MapOfAnomlaies_by_month.png'))

readRDS(file.path(project.datadirectory('bio.lobster.glorys'),'Model_outputs','models','model_selection.rds'))
#m4
m4 = readRDS(file.path(project.datadirectory('bio.lobster.glorys'),'Model_outputs','models','biasCorr_m4.rds'))
or$residuals = residuals(m4)
qqnorm(or$residuals)
qqline(or$residuals)

ggplot()+geom_sf(data=ns_coast)+geom_sf(data=subset(or),aes(fill=residuals,colour=residuals),size=1)+
  scale_fill_gradient2(low='blue',mid='white',high='red',midpoint=0) +
  scale_color_gradient2(low='blue',mid='white',high='red',midpoint=0) +
  theme_test_adam()+facet_wrap(~as.factor(MN))
ggsave(file.path(fig_dir,'MapOfm4Residuals_by_month.png'))


v = predict(m4,type = 'response')
or$ests = m4$family$linkinv(v$est)

plot(or$diff,or$ests,xlab='Observed Differences',ylab='Model Estimates')
ggsave(file.path(fig_dir,'m4Fits_by_obs.png'))



