# setup
library(sf)
library(terra)
library(dplyr)
library(readxl)
require(lwgeom)

indir <- file.path("data")
dir.create(indir, showWarnings = FALSE)
outdir <- file.path("output")
dir.create(outdir, showWarnings = FALSE)

# list files of projects locations
xlsxs <- list.files(indir, ".xlsx$", full.names = TRUE, recursive = TRUE)
xlsxs <- grep("final", xlsxs, value = TRUE)

# read layers with spatial features
col_names <- c(names(read_xlsx(xlsxs[2], skip = 1, sheet = 1)), "geometry")

locs <- lapply(xlsxs, function(xlsx){

  data <- read_xlsx(xlsx, skip = 1, sheet = 1)
  if(length(unique(sapply(data, class))) == 1) data <- read_xlsx(xlsx, skip = 3, sheet = 1)

  data <- data %>%
    mutate(Longitude = as.numeric(trimws(Longitude, whitespace = "[\\h\\v]")),
           Latitude = as.numeric(trimws(Latitude, whitespace = "[\\h\\v]"))) %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326), remove = FALSE)

  if (all(c("Colonne1", "Colonne2") %in% names(data))) data <- select(data, -Colonne1, -Colonne2)
  names(data) <- col_names
  data$file <- basename(xlsx)

  if(!inherits(data$`Date of data collection or latest update`, "POSIXct")){
    data$`Date of data collection or latest update` <- NA
  }

  if(!inherits(data$`Planned or actual start date of activity at the location`, "POSIXct")){
    data$`Planned or actual start date of activity at the location` <- NA
  }
  data
})

locs <- do.call(rbind, locs) %>%
  filter(as.numeric(Latitude) >= -90, as.numeric(Latitude) <= 90,
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
index <- match(locs$DAC.5.Purpose.Code, dac_codes$code)
locs$DAC.5.Name <- dac_codes$name_en[index]
locs <- cbind(locs, west_africa[["name_en"]][as.numeric(st_within(locs, west_africa))])
names(locs)[33] <- "country"
locs$count <- 1

# write to disk
st_write(locs, file.path(outdir, "project_locations.gpkg"), delete_dsn = TRUE)
# locs <- st_read(file.path(outdir, "project_locations.gpkg"))

# simulate data
locs_buffer <- st_buffer(locs, dist = 20000)
locs_sim <- st_as_sf(st_sample(locs_buffer, size = rep(1, nrow(locs))))

data <- extract(tifs, locs_sim, ID = FALSE)
locs_sim <- cbind(locs_sim, data)
locs_sim <- cbind(locs_sim, west_africa[["name_en"]][as.numeric(st_within(locs_sim, west_africa))])
names(locs_sim)[5] <- "country"
locs_sim$count <- 1

# sample DAC5 Codes
unique_dacs <- unique(locs[["DAC.5.Name"]])
locs_sim$DAC5 <- sample(unique_dacs, size = nrow(locs), replace = TRUE)
# sample IATI type
unique_iati <- unique(locs[["Location.Type.IATI"]])
locs_sim$IATI <- sample(unique_iati, size = nrow(locs), replace = TRUE)

locs_sim <- locs_sim[ ,c("country", "DAC5", "IATI", "conflict_class",
                         "conflict_density", "fatalitites_class",
                         "fatalitites_density", "count")]

st_write(locs_sim, file.path(outdir, "sim_project_locations.gpkg"), delete_dsn = TRUE)
