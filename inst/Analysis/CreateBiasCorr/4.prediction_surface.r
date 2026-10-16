require(bio.lobster)
library(bio.utilities)
require(devtools)
library(sf)
library(sdmTMB)
la()
setwd(file.path(project.datadirectory("bio.lobster.glorys")))
t<-readRDS("final_model_biasCorr_m5_sept2.rds")
m4<-t[[1]]
or<-t[[2]]

years_model<-unique(or$YR)

#annualfiles
b<-dir(pattern="^GloryTemps_Depth_y.*\\.rds$")

for(ff in b){
	cat("Processing",ff,"\n")
	damr<-readRDS(ff)
	damr<-st_transform(damr,32620)
	xy<-st_coordinates(damr)/1000
	damr$X<-xy[,1]
	damr$Y<-xy[,2]
	st_geometry(damr)<-NULL
	
	damr<-subset(damr,z>0)
	damr$lz<-log(damr$z)
	damr$YR<-damr$yr
	damr$Glor<-damr$bottomT
	damr$sinDoy=sin(2*pi*damr$doy/365)
	damr$cosDoy=cos(2*pi*damr$doy/365)

	damr<-subset(damr,!is.na(Glor)&YR%in%years_model)
	
if(nrow(damr)==0)next
	n_sets<-200
	damr$grp<-sample(rep(seq_len(n_sets),length.out=nrow(damr)))

	final_subsets<-split(damr,f="grp")

	preds<-lapply(
	final_subsets,function(fs){
			fs$X1000=fs$X
			fs$Y1000=fs$Y
			g<-predict(m4,newdata=fs)
			fs$pred=m4$family$linkinv(g$est)
			fs
		})

	lo<-dplyr::bind_rows(preds)
	yr<-unique(lo$yr)

		saveRDS(lo,file=paste0("BiasCorrPredictions_",yr,".rds"))
		rm(damr,final_subsets,preds,lo)
		gc()
	}

#read and bind
	bb<-dir(pattern="^BiasCorrPredictions_.*\\.rds$")
	out<-bind_rows(
	lapply(bb,readRDS)
	)
	saveRDS(out,"Glorys1994_2025wBiasCorrColumn_doy_sept24.rds")

#split to grids
	gr=readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolys_DepthPruned_37Split.rds'))
	gr41=st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41_grid_polys.rds')))
	gr$GRID_NO=as.numeric(gr$GRID_NO)
	gr41$LFA=as.character(gr41$LFA)
	gtot=bind_rows(gr,gr41)
	gtot=st_transform(gtot,crs=32620)
	st_geometry(gtot)<-st_geometry(gtot)/1000	
	st_crs(gtot)<-32620
	gtot$x=1
	ids = aggregate(x~GRID_NO+LFA,data=gtot,FUN=length)

for(ff in bb){
		cat("Processing",ff,"\n")
		dass<-readRDS(ff)
		dass=st_as_sf(dass,coords=c('X1000','Y1000'),crs=32620)
		st_agr(dass)<-"constant"
		st_agr(gtot)<-"constant"

		chunk_size<-1000
		n_chunks<-ceiling(nrow(dass)/chunk_size)

		results<-vector("list",n_chunks)

	for(i in seq_len(n_chunks)){
			cat("Processingchunk",i,"of",n_chunks,"\n")
			idx<-((i-1)*chunk_size+1):(min(i*chunk_size,nrow(dass)))
			chunk<-dass[idx,]
			#Spatialjoinforthechunk
			r<-st_join(chunk,gtot,join=st_within)
			results[[i]]=subset(r,!is.na(GRID_NO))
			}

#Combineresults
	results<-bind_rows(results)
	results=subset(results,!is.na(LFA))
	yr<-unique(dass$yr)
	saveRDS(results,file=paste0("grid_biasc_",yr,".rds"))
		rm(dass,results)
		gc()
	}

#READ IN AND REBUILD
	bb<-dir(pattern="^grid_biasc_.*\\.rds$")
	for(ff in bb){
		cat("Processing",ff,"\n")
		daa<-readRDS(ff)
		daa=st_as_sf(daa,coords=c('X1000','Y1000'),crs=32620)
		daa$bcT=daa$Glor+daa$pred
		daz=aggregate(z~GRID_NO,data=daa,FUN=function(x)c(mean(x,na.rm=T),sd(x,na.rm=T)))
		daT=aggregate(bcT~GRID_NO+doy+yr+Date,data=daa,FUN=function(x)quantile(x,c(0.025,0.25,0.5,0.75,0.975)))
		daa=merge(daT,daz,all=T)
		daa = merge(daa,ids)#this gives estimates for grids and then reallocates back to LFAs, so whaterever teh grid temp in 98 LFA 34 is the same as grid 98 in LFA 38
		i=which(daa$bcT[,3]< -1.5)
		daa$bcT[i,]<-NA
		yr<-unique(daa$yr)
		saveRDS(daa,file=paste0("grid_biascagg_",yr,".rds"))
		rm(daa,daz,daT)
		gc()
		}	

#READIN AND COMBINED
	bb<-dir(pattern="^grid_biascagg_.*\\.rds$")
	out<-bind_rows(lapply(bb,readRDS))
	saveRDS(out,"Glorys1994_2025wBiasCorrColumn_doy_grid_agg_sept24.rds")


#clean up folder 
bb<-dir(pattern="^BiasCorrPredictions_.*\\.rds$")  
file.remove(bb)
         bb<-dir(pattern="^grid_biasc_.*\\.rds$")
file.remove(bb)
        bb<-dir(pattern="^grid_biascagg_.*\\.rds$")
file.remove(bb)
    
