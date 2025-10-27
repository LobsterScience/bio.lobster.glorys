#' @export
glorysReshapeLong <- function(glorysfile=file.path(project.datadirectory('bio.lobster.glorys'),'Downloads','GLORYS1994-01-01'),redo=F){
				require(satin)
				require(dplyr) 
				options(stringsAsFactors=F)
				y1 = read.cmems(glorysfile)
				nL = length(y1)	
        if(nL==1){ 
          x1 = y1
        } else {
				x1 = y1[[1]]
        }
				nlats = length(x1@lat)
				nlons = length(x1@lon)
				g = expand.grid(x1@lon,x1@lat)
				names(g) = c('X','Y')
				g$EID = 1:nrow(g)
				g$lonI = rep(1:nlons,times=nlats)
				g$latI = rep(1:nlats,each=nlons)
				ds = x1@period[[1]]
				nds = length(ds)
				a1 = list()
		for(i in 1:nL){
				out = list()
				if(nL==1){ 
				  x1 = y1
				} else {
				  x = y1[[i]]
				}
				na = x@attribs$title
					for(j in 1:nrow(g)){
						u = g[j,]
						f = x@data[u$latI, u$lonI,,]
						da = data.frame(Date=as.character(x@period$tmStart),att=f,X=u$X,Y=u$Y)
						names(da)[2] = na
						out[[j]] =da 
						}									
				a1[[i]] = dplyr::bind_rows(out)		
						}
				out = a1 %>% purrr::reduce(inner_join)
				return(out)
		}
