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

# Grid over countries
index <-
  st_intersects(
    x = globalGrid.sf,
    y = st_union(region.sf),
    sparse = FALSE
  ) %>%
  as.logical()

# Create and manipulate grid spatial dataframe
grid.sf <- mutate(globalGrid.sf, status = index) %>%
           dplyr::filter(status == TRUE) %>%
           st_centroid() %>%
           st_buffer(
             dist = size.buffer,
             endCapStyle = "SQUARE"
           ) %>%
           st_transform(crs = "EPSG:4326")


# Write grid to geopackage as layer "tiling"
st_write(
  obj = grid.sf, 
  dsn = "data/vector/dbase.gpkg",
  layer = "tiling"
)

# Read countries from geopackage
Countries <- st_read(
  dsn = "data/vector/dbase.gpkg",
  layer = "countries"
)

# Read tiles from geopackage
Tiles <- st_read(
  dsn = "data/vector/dbase.gpkg",
  layer = "tiling"
)

# Calculate centroids and join with countries
Tiles_centroidsT <- st_centroid(Tiles) %>%
                    st_join(Countries, join = st_within) %>%
                    st_transform(crs = 32718)

# Calculate IDs for tiles based on coordinates
TilesC <- Tiles %>%
          mutate(
                 Country = Tiles_centroidsT$CNTR_ID,
                 Row = round(((CoordT[,2] - min(CoordT[,2])) / size.grid) + 1),
                 Path = round(((CoordT[,1] - min(CoordT[,1])) / size.grid) + 1)) %>%
          unite("ID", Country, Row, Path, sep = "") %>%
          st_transform(crs = "EPSG:4326")
       
# Display grid on interactive map
mapview::mapview(TilesC)   

# Write tiles with IDs to geopackage as layer "tiling_id"
st_write(
  obj = TilesC, 
  dsn = "data/vector/dbase.gpkg",
  layer = "tiling_id"
)
