library(sf)

get_combination <- function(n=3) {
    result <- list()
    for (i in 1:n) {
        result <- c(result, combn(n, i, simplify = FALSE))
    }
    return(result)
}

get_grids <- function(tile, grid, geom_intersect, combs) {
    container <- list()
    for (index in 1:length(combs)) {
        # get intersecting polygons
        intersects <- grid[geom_intersect[combs[[index]]],]
        
        # merge polygons
        intersects <- st_union(intersects)
        
        # intersect
        intersects <- suppressWarnings(
            st_intersection(tile, intersects)
        )
        
        # get area
        area <- st_area(intersects)/as.numeric(st_area(tile))
        
        # add to container
        container[[index]] <- area
    }
    geom_intersect[unlist(combs[which.max(unlist(container))])]
}

get_s2_grids <- function(tile_centroid, s2grid) {
    andes_tile <- st_transform(tile_centroid, tile_centroid$EPSG) %>% 
        st_buffer(18000, endCapStyle = "SQUARE") %>%
        st_transform(4326)
    geom_intersect <- st_intersects(andes_tile, s2grid)[[1]]
    nintersects <- length(geom_intersect)

    if (nintersects > 1) {
        possible_combinations <- get_combination(n=nintersects)
        intersect_grids <- get_grids(
            tile=andes_tile,
            grid=s2grid,
            geom_intersect=geom_intersect,
            combs=possible_combinations
        )
        s2grids <- s2grid[intersect_grids,]$Name
        
        if(length(s2grids) > 1) {
            s2grids <- paste(s2grids, collapse = ",")
        }
    } else {
        s2grids <- s2grid[geom_intersect,]$Name
    }
    s2grids
}



get_l8_grids <- function(tile_centroid, l8grid) {
    andes_tile <- st_transform(tile_centroid, tile_centroid$EPSG) %>% 
        st_buffer(18000, endCapStyle = "SQUARE") %>%
        st_transform(4326)
    geom_intersect <- st_intersects(andes_tile, l8grid)[[1]]
    nintersects <- length(geom_intersect)

    if (nintersects > 1) {
        possible_combinations <- get_combination(n=nintersects)
        intersect_grids <- get_grids(
            tile=andes_tile,
            grid=l8grid,
            geom_intersect=geom_intersect,
            combs=possible_combinations
        )
        l8grids <- l8grid[intersect_grids,]
        l8path <- l8grids$PATH
        l8row <- l8grids$ROW
        
        if(nrow(l8grids) > 1) {
            l8path <- paste(l8path, collapse = ",")
            l8row <- paste(l8row, collapse = ",")
        }

    } else {
        l8path <- l8grid[geom_intersect,]$PATH
        l8row <- l8grid[geom_intersect,]$ROW
    }
    c(l8path, l8row)
}

andes_cube <- st_read("data/vector/andesdatacube_wgs84_4.geojson")
s2grid <- st_read("data/vector/s2grid/sentinel_2_index_shapefile.shp")
s2grid <- st_zm(s2grid, drop = TRUE)
l4grid <- st_read("data/vector/l8grid/WRS2_descending.shp")
l1grid <- st_read("data/vector/l8grid/WRS1_descending.shp")



andes_cube[c("S2TILE", "WRS2PATH", "WRS2ROW", "WRS1PATH", "WRS1ROW")] <- NA

for (index in 1:nrow(andes_cube)) {
    print(index)
    tile_centroid <- andes_cube[index,]
    
    # Get S2 grids
    s2grids <- get_s2_grids(tile_centroid, s2grid)
    andes_cube[index,]$S2TILE <- s2grids

    # Get L4-L9 grids
    l8grids <- get_l8_grids(tile_centroid, l4grid)
    andes_cube[index,]$WRS2PATH <- l8grids[1]
    andes_cube[index,]$WRS2ROW <- l8grids[2]
    
    # Get L1-L3 grids
    l1grids <- get_l8_grids(tile_centroid, l1grid)
    andes_cube[index,]$WRS1PATH <- l1grids[1]
    andes_cube[index,]$WRS1ROW <- l1grids[2]
}

write_sf(andes_cube, "andesdatacube_tiles.geojson")
```


