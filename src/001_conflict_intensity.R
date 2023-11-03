# setup
library(sf)
library(dplyr)
library(spatstat)
library(rnaturalearth)
library(tidyr)
library(terra)

indir <- file.path("data")
dir.create(indir, showWarnings = FALSE)
outdir <- file.path("output")
dir.create(outdir, showWarnings = FALSE)

# process UCDP GED data to GPKG
ged_org <- file.path(indir, "ged_23_1.gpkg")
if (!file.exists(ged_org)){
  url <- "/vsizip/vsicurl/https://ucdp.uu.se/downloads/ged/ged231-csv.zip"
  gdal_utils(
    util = "vectortranslate",
    source = url,
    destination = ged_org,
    options = c(
      "-a_srs", "EPSG:4326",
      "-oo", "GEOM_POSSIBLE_NAMES=geom_wkt",
      "-overwrite"
    )
  )
}

# get Western Africa region
africa <- ne_countries(continent = "Africa", returnclass = "sf")
west_africa <- africa[africa$subregion == "Western Africa", ]
st_write(west_africa, file.path(outdir, "west_africa.gpkg"), delete_dsn = TRUE)

# filter and transform GED
ged <- read_sf(ged_org,
               wkt_filter = st_as_text(st_as_sfc(st_bbox(west_africa)))) %>%
  filter(date_start >= as.Date("2018-01-01"),
         where_prec <= 3,
         as.vector(st_within(., st_union(west_africa), sparse = FALSE))) %>%
  mutate(across(starts_with("death"), ~as.numeric(.x)),
         deaths_total = rowSums(across(starts_with("death")))) %>%
  select(id, date_start, year, country, starts_with("death")) %>%
  summarise(across(starts_with("death"), ~sum(.x)), .by = c(year, geom))


# calculate a planar Equal Area Projection
aoi <- st_union(west_africa)
center <- st_coordinates(st_centroid(aoi))
laea <- paste0(
  "+proj=laea +lat_0=", center[2],
  " +lon_0=", center[1], " +x_0=0 +y_0=0"
)

ged <- st_transform(ged, laea)
aoi <- st_transform(st_union(aoi), laea)

# create point pattern object
ppp <- as.ppp(c(aoi, st_as_sfc(ged)))
marks(ppp) <- ged$deaths_total

# estimate density
res <- 10 * 1000 # resolution of output rasters in meters
sigma <- 25 * 1000 # bandwidth of the kernel in meters
conflict_dens <- density(ppp, sigma = sigma, eps = res, edge = TRUE, diggle = TRUE)
fatalities_dens <- density(ppp, weights = marks(ppp), sigma = sigma, eps = res, edge = TRUE, diggle = TRUE)

# convert to SpatRaster
to_spatRaster <- function(r, res, crs){
  r <- rast(r) * res ^ 2
  crs(r) <- crs
  r
}

conflict_dens <- to_spatRaster(conflict_dens, res, crs(laea))
fatalities_dens <- to_spatRaster(fatalities_dens, res, crs(laea))

# classify raster based on percentiles
classify_density <- function(r, qs, levels){
  vals <- global(r, fun = function(x) quantile(x, qs, na.rm = TRUE))
  r <- classify(r, as.numeric(vals), include.lowest = TRUE, right = TRUE, brackets = TRUE)
  set.cats(r, value = data.frame(value = 0:(length(vals)-2), category = levels))
  r
}

qs <- c(0, 0.5, 0.85, 0.95, 0.99, 1)
cats <- c("low", "moderate", "elevated", "high", "very high")
conflict_class <- classify_density(conflict_dens, qs, cats)
fatalities_class <- classify_density(fatalities_dens, qs, cats)

# transform to Lat/Lon and write data to disk
to_disk <- function(r, method, filename){
  r <- project(r, "EPSG:4326", method = method)
  names(r) <- tools::file_path_sans_ext(basename(filename))
  writeRaster(r, filename, overwrite = TRUE)
}

to_disk(conflict_dens, "bilinear", file.path(outdir, "conflict_density.tif"))
to_disk(conflict_class, "near", file.path(outdir, "conflict_class.tif"))
to_disk(fatalities_dens, "bilinear", file.path(outdir, "fatalitites_density.tif"))
to_disk(fatalities_class, "near", file.path(outdir, "fatalitites_class.tif"))

ged <- st_transform(ged, "EPSG:4326")
st_write(ged, dsn = file.path(outdir, "ged_west_africa.gpkg"), delete_dsn = TRUE)
