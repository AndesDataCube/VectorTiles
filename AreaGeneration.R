library(reticulate)
library(rgee)
library(googledrive)
library(sf)
library(geojsonio) # Convertir de geojson a un objetivo ee
library(leaflet)
library(mapview)

# ee_install()
ee_Initialize("julio.contreras1@unmsm.edu.pe", drive = TRUE)

# Inicialización de sesión en GEE
rgee::ee_Authenticate()

ZE <- st_read("D:/AndesDataCube/Image/Papers/Yuri/InnerTropics.geojson")
ZECentroid <- st_centroid(ZE)
coords <- as.vector(st_coordinates(ZECentroid$geometry))

# Convertir la geometría de sf a ee.Geometry
ee_geometry <- sf_as_ee(ZE)

# Cargar la imagen SRTM
srtmCollection <- ee$Image("CGIAR/SRTM90_V4")

# Recortar la imagen utilizando la geometría
srtmClipped <- srtmCollection$clip(ee_geometry)
srtmClipped$projection()$getInfo()

mask <- srtmClipped$gt(3800)
srtmMasked <- srtmClipped$updateMask(mask)


# # Definir la nueva resolución deseada
# new_resolution <- 1
# 
# # Resamplear la imagen utilizando la función reduceResolution()
# srtmResampled <- srtmClipped$reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 65536)
# srtmResampled <- srtmResampled$updateMask(srtmResampled$mask())
# 
# # Escalar la imagen a la nueva resolución
# scale_factor <- ee$Number(10000)$divide(90)
# srtmResampled_scaled <- srtmResampled$resample('bicubic')$multiply(scale_factor)
# 
# # Crear una máscara con los píxeles que sean mayores a 3500 metros
# mask <- srtmResampled_scaled$gt(3500)
# srtmMasked <- srtmResampled_scaled$updateMask(mask)

# mask <- srtmResampled_scaled$gt(3500)
# srtmMasked <- srtmResampled_scaled$updateMask(mask)


Map$setCenter(coords[1], coords[2], 5)
Map$addLayer(srtmClipped, name = "elev")
Map$addLayer(srtmMasked, name = "elev")
# Map$addLayer(srtmResampled_scaled, name = "elev")



# Exportar la imagen a Google Drive
task_img <- ee_image_to_drive(image = srtmClipped,
                              description = "srtmClipped",
                              # region = ee_geometry,
                              # fileFormat = "GEO_TIFF",
                              folder = "SRTMAndesDataCube",
                              scale = 5000)





task_img$start()
ee_monitoring()
ee_check_task_status(task_img)

ee$batch$Export$image$toDrive(description = "srtm_masked",
                              fileFormat = "GEO_TIFF",
                              image = srtmMasked,
                              folder = "SRTMAndesDataCube",
                              scale = 1000)



# Generar Grillas y centroides

# Leer el archivo GeoJSON
Cuad <- st_read("D:/AndesDataCube/Image/Papers/Yuri/InnerTropicsCuadriculas.geojson")

# Definir la proyección de destino (EPSG:32718)
proj <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"

# Proyectar los datos a la proyección EPSG:32718
Cuad_proj <- st_transform(Cuad, crs = proj)

# Disolver los polígonos en un polígono único
Cuad_dissolved <- st_union(Cuad_proj)

# Descomponer el polígono único en polígonos individuales
Cuad_exploded <- st_cast(Cuad_dissolved, "POLYGON")

# Definir el tamaño de la celda en metros
cellsize <- 30000

#---------
# centroids_list <- list()  # Lista vacía para almacenar los centroides
# buffers_list <- list()    # Lista vacía para almacenar los buffers
# 
# for (x in 1:length(Cuad_exploded)) {
#   grid <- st_make_grid(Cuad_exploded[x], cellsize = cellsize, what = "polygons")
#   centroids <- st_centroid(grid)
#   buffers <- st_buffer(centroids, dist = 36000, endCapStyle="SQUARE")
#   centroids_list[[x]] <- centroids  # Agregar los centroides a la lista
#   buffers_list[[x]] <- buffers      # Agregar los buffers a la lista
# }
# # st_union(centroids_list[[1]])
# centroids_multipoint <- do.call(rbind, centroids_list)  # Convertir la lista de centroides en un objeto sf
# # Crear una matriz de coordenadas
# coords <- matrix(unlist(centroids_multipoint), ncol = 2, byrow = TRUE)
# # Convertir la matriz en un objeto sf
# centroids_sf <- st_as_sf(data.frame(coords), coords = c("X1", "X2"), crs = st_crs(proj)) # Google earth
# buffers <- st_buffer(centroids_sf, dist = 36000, endCapStyle="SQUARE")
# 
# st_write(buffers, "D:/AndesDataCube/Image/Papers/Yuri/Borrador/buffers.shp")
#---------

# Definir el tamaño de la celda en metros
cellsize <- 30000

# Generar la cuadrícula
grid <- st_make_grid(Cuad_exploded, cellsize = cellsize, what = "polygons")

grid_sf <- st_as_sf(grid)
plot(grid_sf)


grid_sf_exploded <- st_cast(grid_sf, "POLYGON")

# Determinar qué cuadrículas se superponen con Cuad_exploded
overlap <- st_intersects(grid_sf_exploded, Cuad_dissolved, sparse = FALSE)[, 1]
# overlap_logical <- as.logical(overlap)
# Filtrar el conjunto de datos grid_sf_exploded utilizando el resultado de superposición
selected_grid <- grid_sf_exploded[overlap,]
centroids <- st_centroid(selected_grid)
buffers <- st_buffer(centroids, dist = 18000, endCapStyle="SQUARE")
plot(buffers, add = TRUE)



st_write(buffers, "D:/AndesDataCube/Image/Papers/Yuri/Borrador/buffers.shp")
