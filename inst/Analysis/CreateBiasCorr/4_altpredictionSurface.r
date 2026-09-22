library(bio.lobster)
library(bio.utilities)
library(data.table)
library(sf)
library(sdmTMB)
library(future.apply)
library(terra)

la()

setwd(file.path(project.datadirectory("bio.lobster.glorys")))
damr <- readRDS("GlorysTemps_Depth1994_2025.rds")
damr <- st_transform(damr, 32620)
xy <- st_coordinates(damr) / 1000
damr$X <- xy[,1]
damr$Y <- xy[,2]
st_geometry(damr) <- NULL
damr <- damr[z > 0, ]
damr$lz <- log(damr$z)
damr$YR <- damr$yr
damr$Glor <- damr$bottomT


setDT(damr)
damr[, sinDoy := sin(2*pi*doy/365)]
damr[, cosDoy := cos(2*pi*doy/365)]


n_sets <- 200
base_subsets <- replicate(
  n_sets,
  damr[, .SD[sample(.N, 1, replace = TRUE)], by = YR],
  simplify = FALSE
)

sampled_rows <- rbindlist(base_subsets)

remaining_df <- fsetdiff(
  damr,
  unique(sampled_rows)
)

remaining_df[, grp := rep(
  seq_len(n_sets),
  length.out = .N
)]

remaining_split <- split(
  remaining_df,
  by = "grp",
  keep.by = FALSE
)

final_subsets <- lapply(seq_len(n_sets),
  function(i){
    rbindlist(
     list(base_subsets[[i]], remaining_split[[i]]),
      fill = TRUE
    )
  }
)

rm(base_subsets, sampled_rows, remaining_df, remaining_split)
gc()


t <- readRDS("final_model_biasCorr_m5_sept2.rds")

m4 <- t[[1]]
or <- t[[2]]

years <- unique(or$YR)


plan(
  multisession,
  workers = parallel::detectCores() - 1
)

preds <- future_lapply(
  final_subsets,
  function(fs){

    fs <- fs[
      !is.na(Glor) &
      YR %in% years
    ]

    fs[, X1000 := X]
    fs[, Y1000 := Y]

    g <- predict(
      m4,
      newdata = fs
    )

    fs[, pred := m4$family$linkinv(g$est)]

    fs
  },
  future.seed = TRUE
)

lo <- rbindlist(preds)

saveRDS(
  lo,
  "Glorys1994-2025wBiasCorrColumn_doy_sept22.rds"
)

rm(preds)
gc()

gr <- readRDS(
  file.path(
    git.repo,
    "bio.lobster.data",
    "mapping_data",
    "GridPolys_DepthPruned_37Split.rds"
  )
)

gr41 <- st_as_sf(
  readRDS(
    file.path(
      git.repo,
      "bio.lobster.data",
      "mapping_data",
      "LFA41_grid_polys.rds"
    )
  )
)

gr$GRID_NO <- as.numeric(gr$GRID_NO)
gr41$LFA <- as.character(gr41$LFA)

gtot <- rbind(
  gr,
  gr41
)

gtot <- st_transform(
  gtot,
  32620
)

st_geometry(gtot) <- st_geometry(gtot) / 1000
st_crs(gtot) <- 32620


pts <- vect(
  lo,
  geom = c("X1000","Y1000"),
  crs = "EPSG:32620"
)

polys <- vect(gtot)

rm(lo)
gc()

joined <- terra::extract(
  polys,
  pts,
  bind = TRUE
)

joined <- as.data.frame(joined)

setDT(joined)

joined <- joined[
  !is.na(GRID_NO) &
  !is.na(LFA)
]


joined[
  ,
  bcT := Glor + pred
]


daz <- joined[
  ,
  .(
    mean_depth = mean(z, na.rm = TRUE),
    sd_depth   = sd(z, na.rm = TRUE)
  ),
  by = .(
    LFA,
    GRID_NO
  )
]

daT <- joined[
  ,
  .(
    q025 = quantile(
      bcT,
      .025,
      na.rm = TRUE
    ),
    q25 = quantile(
      bcT,
      .25,
      na.rm = TRUE
    ),
    q50 = quantile(
      bcT,
      .50,
      na.rm = TRUE
    ),
    q75 = quantile(
      bcT,
      .75,
      na.rm = TRUE
    ),
    q975 = quantile(
      bcT,
      .975,
      na.rm = TRUE
    )
  ),
  by = .(
    LFA,
    GRID_NO,
    doy,
    yr,
    Date
  )
]


dazt <- merge(
  daT,
  daz,
  by = c(
    "LFA",
    "GRID_NO"
  ),
  all.x = TRUE
)

dazt[
  q50 < -1.5,
  c(
    "q025",
    "q25",
    "q50",
    "q75",
    "q975"
  ) := NA
]

saveRDS(
  dazt,
  "Glorys1994_2025wBiasCorrColumn_doy_grid_agg_sept22.rds"
)
