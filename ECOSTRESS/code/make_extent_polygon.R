library(here)
library(raster)
library(sf)
library(sp)

r <- raster(here("ECOSTRESS", "data", "Example_Eco.tif")) 
e <- extent(r)
p <- as(e, 'SpatialPolygons') 
s <- st_as_sf(p)
st_crs(s) <- 4326

st_write(s, here("ECOSTRESS", "data", "Extent_polygon.shp"))
