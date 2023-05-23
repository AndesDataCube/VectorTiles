#'  @title      ***************
#'  @project    Tiling
#'  @created   	23 de mayo, 2023
#'  @updated   	23 de mayo, 2023
#'  @revised   	23 de mayo, 2023
#'  @category  	***************
#'  @R version  R version 4.2.2 (2022-10-31)
#'  @OS system  Linux Debian Buster 10.9
#'  @author     Prudencio-Paredes Fernando
#'  @email      julio.contreras1@unmsm.edu.pe
#'              fjprudenciop@gmail.com

# 1. Initial setup ----

library(sf)
library(tidyverse)
library(raster)
library(rgee)

# Initialize GEE
ee_Initialize("julio.contreras1@unmsm.edu.pe", drive = TRUE, gcs = T)

# Parameters
elev.threshold <- 4000
size.grid <- 30000
size.buffer <- 18000

# 2. Load data ----
# Draw andes freehand
andes.sf <- mapedit::drawFeatures()
andes.ee <- sf_as_ee(andes.sf)

srtm <- ee$Image("CGIAR/SRTM90_V4")

# 3. Build mask from elevation ----
# image mask
mask <-
  srtm$
  clip(andes.ee)$
  gt(elev.threshold)

region <- mask$updateMask(mask)

# vectorize
region.ee <-
  region$
  reduceToVectors(
    geometryType = "polygon",
    scale = 90,
    eightConnected = TRUE,
    labelProperty = "elevation",
    maxPixels = 1e13
  )

region.sf <-
  ee_as_sf(
    x = region.ee,
    maxFeatures = 1e13
  ) %>%
  st_transform(crs = "EPSG:32718")

# 4. Create Grid ----
# grid over full extensions
globalGrid.sf <-
  st_make_grid(
    x = region.sf,
    cellsize = size.grid,
    what = "polygons"
  ) %>%
  st_as_sf()

# grid over countries
index <-
  st_intersects(
    x = globalGrid.sf,
    y = st_union(region.sf),
    sparse = FALSE
  ) %>%
  as.logical()

grid.sf <-
  mutate(globalGrid.sf, status = index) %>%
  dplyr::filter(status == TRUE) %>%
  st_centroid() %>%
  st_buffer(
    dist = size.buffer,
    endCapStyle = "SQUARE"
  ) %>%
  st_transform(crs = "EPSG:4326")

mapview::mapview(grid.sf)

st_write(
  obj = grid.sf, 
  dsn = "data/vector/dbase.gpkg",
  layer = "tiling"
)
