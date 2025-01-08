
require(bio.lobster)
require(bio.lobster.glorys)
require(satin)
require(tidyr)

setwd(file.path(project.datadirectory('bio.lobster.glorys')))
fil = dir('Downloads')
y1 = read.cmems(file.path('Downloads',fil[2]))
a = y1$bottomT
image(a@lon, a@lat, t(a@data[,,300,]))

#glorys reshape to r Object

for(i in 2:length(fil)){
		g = glorysReshapeLong(glorysfile=file.path('Downloads',fil[i]))
		saveRDS(g,file = file.path('Summary',paste(fil[i],'ShelfBoF.rds',sep="_")))
		print(fil[i])
	}

##climatology
g = glorysReshape(glorysfile='cmems_mod_glo_phy_my_0.083deg-climatology_P1M-m_1733154384209.nc')
saveRDS(g,file = file.path('SummaryFiles','climatologyMonthly1993-2016_ShelfBoF.rds'))

