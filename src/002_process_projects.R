# setup
library(sf)
library(terra)
library(dplyr)
library(tibble)
library(readxl)
library(franc)
require(lwgeom)

indir <- file.path("data")
dir.create(indir, showWarnings = FALSE)
outdir <- file.path("output")
dir.create(outdir, showWarnings = FALSE)

# list files of projects locations
xlsxs <- list.files(indir, ".xlsx$", full.names = TRUE, recursive = TRUE)
xlsxs <- grep("final", xlsxs, value = TRUE)

col_names <- lapply(xlsxs, function(xlsx){
  data <- read_xlsx(xlsx, skip = 1, sheet = 1)
  if(length(unique(sapply(data, class))) == 1) data <- read_xlsx(xlsx, skip = 3, sheet = 1)
  col_names <- names(data)
  lng <- table(sapply(col_names, franc, whitelist = c("fra", "eng")))
  lng <- names(which.max(lng))
  tibble(lng = lng, names = col_names)
})


# manually correct miss-matches
col_names <- do.call(rbind, col_names)
col_names$names[col_names$names == "Country of loaction"] <- "Country of location"
col_names$names[col_names$names == "Géo-coordonnées supplémentaires soumises en .kmz, .shp or similar (lignes/polygones)"] <- "Géo-coordonnées supplémentaires soumises en KML (lignes/polygones)"
col_names <- distinct(col_names)

fra <- filter(col_names, lng == "fra")
eng <- filter(col_names, lng == "eng")

common <- unique(fra$names[which(fra$names %in% eng$names)])
fra <- filter(fra, !names %in% common)
eng <- filter(eng, !names %in% common)
fra <- filter(fra, !names %in% c("Colonne1", "Colonne2"))

col_dict <- data.frame(
  eng = c(common, eng$names),
  fra = c(common, fra$names),
  abbrv = c("unique_id", "latitude", "longitude", "gadm_gid", "country_or_beneficiary_code",
            "kfw_proj_no", "abbrv_proj_name", "bmz_proj_no", "dac_5", "proj_loc_identifier",
            "country", "location_name", "author", "restrictions", "date_of_collection",
            "activity_status", "start_date", "activity_description", "add_description", "iati_type",
            "alt_location_type", "geo_exactness", "rel_community", "add_geo_data", "gid_level",
            "gadm_v")
)

locs <- lapply(xlsxs, function(xlsx){

  data <- read_xlsx(xlsx, skip = 1, sheet = 1)
  if(length(unique(sapply(data, class))) == 1) data <- read_xlsx(xlsx, skip = 3, sheet = 1)

  if ("Country of loaction" %in% names(data)){
    names(data)[names(data) == "Country of loaction"] = "Country of location"
  }

  if ("Géo-coordonnées supplémentaires soumises en .kmz, .shp or similar (lignes/polygones)" %in% names(data)) {
    names(data)[names(data) == "Géo-coordonnées supplémentaires soumises en .kmz, .shp or similar (lignes/polygones)"] = "Géo-coordonnées supplémentaires soumises en KML (lignes/polygones)"
  }

  lng <- table(sapply(names(data), franc, whitelist = c("fra", "eng")))
  lng <- names(which.max(lng))

  if (lng == "fra") {
    data <- select(data, any_of(col_dict$fra)) %>%
      rename(any_of(setNames(col_dict$fra, col_dict$abbrv)))
  } else {
    data <- select(data, col_dict$eng) %>%
      rename(any_of(setNames(col_dict$eng, col_dict$abbrv)))
  }

  cols <- rep(NA, nrow(col_dict))
  names(cols) <- col_dict$abbrv

  data <- add_column(data, !!!cols[setdiff(col_dict$abbrv, names(data))])

  unique_locs <- unique(data$longitude)
  if (length(unique_locs) == 1) {
    if(is.na(unique_locs)) {
      return(NULL)
    }
  }

  data <- data %>%
    mutate(longitude = as.numeric(trimws(longitude, whitespace = "[\\h\\v]")),
           latitude = as.numeric(trimws(latitude, whitespace = "[\\h\\v]"))) %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326), remove = FALSE)

  data$file <- basename(xlsx)

  if(!inherits(data[["start_date"]], "POSIXct")){
    data[["start_date"]] <- NA
  }

  if(!inherits(data[["date_of_collection"]], "POSIXct")){
    data[["date_of_collection"]] <- NA
  }
  data
})

locs <- do.call(rbind, locs) %>%
  filter(as.numeric(latitude) >= -90, as.numeric(latitude) <= 90,
         !is.na("Planned or actual start date of activity at the location"))

# get Western Africa region
west_africa <- st_read(file.path(outdir, "west_africa.gpkg"))
locs <- locs %>% filter(as.vector(st_within(., st_union(west_africa), sparse = FALSE)))

# prepare conflict density tifs
tifs <- list.files(outdir, ".tif$", full.names = TRUE)
tifs <- lapply(tifs, rast)
tifs <- do.call(c, tifs)

# extract density estimates at project locations
data <- extract(tifs, locs, ID = FALSE)
locs <- cbind(locs, data)

# download DAC codes
dac_codes <- read.csv("https://raw.githubusercontent.com/datasets/dac-and-crs-code-lists/main/data/sectors.csv")
index <- match(locs$dac_5, dac_codes$code)
locs$dac_5_name <- dac_codes$name_en[index]
locs <- cbind(locs, west_africa[["name_en"]][as.numeric(st_within(locs, west_africa))])
names(locs)[33] <- "country_name"
locs$count <- 1

# write to disk
st_write(locs, file.path(outdir, "project_locations.gpkg"), delete_dsn = TRUE)
# locs <- st_read(file.path(outdir, "project_locations.gpkg"))

# simulate data
if (!file.exists(file.path(outdir, "sim_project_locations.gpkg"))) {
  locs_buffer <- st_buffer(locs, dist = 20000)
  locs_sim <- st_as_sf(st_sample(locs_buffer, size = rep(1, nrow(locs))))
} else {
  locs_sim <- st_read(file.path(outdir, "sim_project_locations.gpkg"))
  locs_sim <- locs_sim[ , "geom"]
}

data <- extract(tifs, locs_sim, ID = FALSE)
locs_sim <- cbind(locs_sim, data)
locs_sim <- cbind(locs_sim, west_africa[["name_en"]][as.numeric(st_within(locs_sim, west_africa))])
names(locs_sim)[5] <- "country_name"
locs_sim$count <- 1

# sample DAC5 Codes
unique_dacs <- unique(locs[["dac_5_name"]])
locs_sim$dac_5_name <- sample(unique_dacs, size = nrow(locs), replace = TRUE)
# sample IATI type
unique_iati <- unique(locs[["iati_type"]])
locs_sim$iati_type <- sample(unique_iati, size = nrow(locs), replace = TRUE)

locs_sim <- locs_sim[ ,c("country_name", "dac_5_name", "iati_type", "conflict_class",
                         "conflict_density", "fatalitites_class",
                         "fatalitites_density", "count")]

st_write(locs_sim, file.path(outdir, "sim_project_locations.gpkg"), delete_dsn = TRUE)
