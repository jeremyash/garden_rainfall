library(terra)
library(tidyverse)
library(lubridate)

GARDEN_NAME <- "Garden"
GARDEN_LAT  <- 35.611622
GARDEN_LON  <- -82.371369
GARDEN_TZ   <- "America/New_York"

RAINFALL_RADIUS_MILES <- 5
FORECAST_MAP_RADIUS_MULTIPLIER <- 2

NDFD_QPF_URL <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.qpf.bin"

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

cache_rds <- "cache/forecast_cache.rds"
cache_tif <- "cache/forecast_qpf_ll.tif"

tmp <- tempfile(fileext = ".bin")

download.file(
  NDFD_QPF_URL,
  tmp,
  mode = "wb",
  quiet = FALSE
)

qpf <- terra::rast(tmp)

garden_ll <- terra::vect(
  data.frame(
    lon = GARDEN_LON,
    lat = GARDEN_LAT
  ),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

garden_qpf <- terra::project(garden_ll, terra::crs(qpf))

vals <- terra::extract(qpf, garden_qpf)
qpf_values <- as.numeric(vals[1, -1])

valid_times <- terra::time(qpf)

if (is.null(valid_times) || all(is.na(valid_times))) {
  valid_times <- seq(
    from = floor_date(with_tz(Sys.time(), "UTC"), "6 hours"),
    by = "6 hours",
    length.out = terra::nlyr(qpf)
  )
}

valid_times_local <- with_tz(valid_times, GARDEN_TZ)

qpf_table <- tibble(
  layer = seq_along(qpf_values),
  valid_time_utc = valid_times,
  valid_time_local = valid_times_local,
  rainfall_in = qpf_values,
  cumulative_rainfall_in = cumsum(replace_na(qpf_values, 0)),
  slider_label = format(valid_times_local, "%a %b %d, %H:%M")
)

garden_buffer <- terra::buffer(
  terra::project(garden_ll, "EPSG:3857"),
  width = RAINFALL_RADIUS_MILES * FORECAST_MAP_RADIUS_MULTIPLIER * 1609.34
) |>
  terra::project(terra::crs(qpf))

qpf_crop <- terra::crop(qpf, garden_buffer)
qpf_mask <- terra::mask(qpf_crop, garden_buffer)

qpf_ll <- terra::project(qpf_mask, "EPSG:4326")

terra::writeRaster(
  qpf_ll,
  cache_tif,
  overwrite = TRUE
)

forecast_cache <- list(
  qpf_file = cache_tif,
  table = qpf_table,
  garden = list(
    name = GARDEN_NAME,
    lat = GARDEN_LAT,
    lon = GARDEN_LON,
    timezone = GARDEN_TZ
  ),
  rainfall_radius_miles = RAINFALL_RADIUS_MILES,
  forecast_map_radius_multiplier = FORECAST_MAP_RADIUS_MULTIPLIER,
  last_refresh = Sys.time()
)

saveRDS(forecast_cache, cache_rds)

message("Saved forecast cache RDS: ", cache_rds)
message("Saved forecast raster: ", cache_tif)