library(terra)
library(tidyverse)
library(lubridate)

# -----------------------------
# Garden settings
# -----------------------------

GARDEN_NAME <- "Garden"
GARDEN_LAT  <- 35.611622
GARDEN_LON  <- -82.371369
GARDEN_TZ   <- "America/New_York"

RAINFALL_RADIUS_MILES <- 5
FORECAST_MAP_RADIUS_MULTIPLIER <- 2

NDFD_QPF_URL <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.qpf.bin"

# -----------------------------
# Output
# -----------------------------

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

cache_rds <- "cache/forecast_cache.rds"
cache_tif <- "cache/forecast_qpf_ll.tif"

# -----------------------------
# Download NDFD QPF
# -----------------------------

tmp <- tempfile(fileext = ".bin")

download.file(
  NDFD_QPF_URL,
  tmp,
  mode = "wb",
  quiet = FALSE
)

qpf <- terra::rast(tmp)

qpf <- qpf / 25.4
terra::units(qpf) <- "in"

message("NDFD QPF layers: ", terra::nlyr(qpf))

# -----------------------------
# Garden point
# -----------------------------

garden_ll <- terra::vect(
  data.frame(
    lon = GARDEN_LON,
    lat = GARDEN_LAT
  ),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

garden_qpf <- terra::project(
  garden_ll,
  terra::crs(qpf)
)

# -----------------------------
# Extract rainfall values
# -----------------------------

vals <- terra::extract(qpf, garden_qpf)

qpf_values <- as.numeric(vals[1, -1])

# -----------------------------
# Forecast valid times
# -----------------------------

valid_times_raw <- terra::time(qpf)

bad_times <- is.null(valid_times_raw) ||
  all(is.na(valid_times_raw)) ||
  length(unique(valid_times_raw)) <= 1 ||
  any(lubridate::year(valid_times_raw) < 2020, na.rm = TRUE)

if (bad_times) {
  
  message("Bad or missing GRIB valid times detected. Using generated 6-hour forecast times.")
  
  valid_times <- seq(
    from = lubridate::ceiling_date(
      lubridate::with_tz(Sys.time(), "UTC"),
      "6 hours"
    ),
    by = "6 hours",
    length.out = terra::nlyr(qpf)
  )
  
} else {
  
  valid_times <- valid_times_raw
}

valid_times_local <- lubridate::with_tz(
  valid_times,
  GARDEN_TZ
)

# -----------------------------
# Forecast table
# -----------------------------

qpf_table <- tibble(
  layer = seq_along(qpf_values),
  
  slider_value = as.character(seq_along(qpf_values)),
  
  valid_time_utc = valid_times,
  
  valid_time_local = valid_times_local,
  
  rainfall_in = qpf_values,
  
  cumulative_rainfall_in = cumsum(
    replace_na(qpf_values, 0)
  ),
  
  slider_label = format(
    valid_times_local,
    "%a %b %d %H:%M"
  )
)

# -----------------------------
# Crop around garden
# -----------------------------

garden_buffer <- terra::buffer(
  
  terra::project(
    garden_ll,
    "EPSG:3857"
  ),
  
  width =
    RAINFALL_RADIUS_MILES *
    FORECAST_MAP_RADIUS_MULTIPLIER *
    1609.34
  
) |>
  
  terra::project(
    terra::crs(qpf)
  )

qpf_crop <- terra::crop(
  qpf,
  garden_buffer
)

qpf_mask <- terra::mask(
  qpf_crop,
  garden_buffer
)

# -----------------------------
# Project once for Leaflet
# -----------------------------

qpf_ll <- terra::project(
  qpf_mask,
  "EPSG:4326"
)

# -----------------------------
# Save raster cache
# -----------------------------

terra::writeRaster(
  qpf_ll,
  cache_tif,
  overwrite = TRUE
)

# -----------------------------
# Save metadata cache
# -----------------------------

forecast_cache <- list(
  
  qpf_file = cache_tif,
  
  table = qpf_table,
  
  garden = list(
    name = GARDEN_NAME,
    lat = GARDEN_LAT,
    lon = GARDEN_LON,
    timezone = GARDEN_TZ
  ),
  
  rainfall_radius_miles =
    RAINFALL_RADIUS_MILES,
  
  forecast_map_radius_multiplier =
    FORECAST_MAP_RADIUS_MULTIPLIER,
  
  last_refresh = Sys.time()
)

saveRDS(
  forecast_cache,
  cache_rds
)

message("Saved forecast cache:")
message(cache_rds)

message("Saved forecast raster:")
message(cache_tif)

message("Last refresh:")
message(forecast_cache$last_refresh)

# -----------------------------
# Observed rainfall: MRMS 24-hour QPE
# -----------------------------

observed_cache_rds <- "cache/observed_cache.rds"
observed_cache_tif <- "cache/observed_qpe_24h_ll.tif"

MRMS_BASE_URL <- "https://mrms.ncep.noaa.gov/data/2D/MultiSensor_QPE_24H_Pass2"

download_mrms_latest <- function(hours_back = 12) {
  
  now_utc <- lubridate::floor_date(
    lubridate::with_tz(Sys.time(), "UTC"),
    "hour"
  )
  
  candidate_times <- now_utc - lubridate::hours(2:hours_back)
  
  for (i in seq_along(candidate_times)) {
    
    t <- candidate_times[i]
    
    stamp <- format(
      t,
      format = "%Y%m%d-%H0000",
      tz = "UTC"
    )
    
    url <- paste0(
      MRMS_BASE_URL,
      "/MRMS_MultiSensor_QPE_24H_Pass2_00.00_",
      stamp,
      ".grib2.gz"
    )
    
    gz_tmp <- tempfile(fileext = ".grib2.gz")
    grib_tmp <- tempfile(fileext = ".grib2")
    
    message("Trying MRMS: ", url)
    
    ok <- tryCatch({
      download.file(url, gz_tmp, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    if (ok && file.exists(gz_tmp) && file.info(gz_tmp)$size > 0) {
      
      R.utils::gunzip(
        gz_tmp,
        destname = grib_tmp,
        remove = FALSE,
        overwrite = TRUE
      )
      
      if (file.exists(grib_tmp) && file.info(grib_tmp)$size > 0) {
        return(list(
          file = grib_tmp,
          valid_time_utc = t,
          url = url
        ))
      }
    }
  }
  
  stop("No recent MRMS 24-hour QPE file found.")
}

mrms_dl <- download_mrms_latest()

qpe <- terra::rast(mrms_dl$file)

message("MRMS QPE layers: ", terra::nlyr(qpe))
message("MRMS units: ", paste(terra::units(qpe), collapse = ", "))

# MRMS QPE is generally millimeters. Convert to inches.
qpe <- qpe / 25.4
terra::units(qpe) <- "in"

# -----------------------------
# Extract observed rainfall at garden
# -----------------------------

garden_qpe <- terra::project(
  garden_ll,
  terra::crs(qpe)
)

observed_val <- terra::extract(
  qpe,
  garden_qpe
)

observed_rainfall_in <- as.numeric(observed_val[1, 2])

# -----------------------------
# Crop/mask around garden
# -----------------------------

qpe_buffer <- terra::buffer(
  
  terra::project(
    garden_ll,
    "EPSG:3857"
  ),
  
  width =
    RAINFALL_RADIUS_MILES *
    FORECAST_MAP_RADIUS_MULTIPLIER *
    1609.34
  
) |>
  terra::project(
    terra::crs(qpe)
  )

qpe_crop <- terra::crop(
  qpe,
  qpe_buffer
)

qpe_mask <- terra::mask(
  qpe_crop,
  qpe_buffer
)

qpe_ll <- terra::project(
  qpe_mask,
  "EPSG:4326"
)

terra::writeRaster(
  qpe_ll,
  observed_cache_tif,
  overwrite = TRUE
)

observed_cache <- list(
  
  qpe_file = observed_cache_tif,
  
  table = tibble(
    product = "MRMS 24-hour QPE",
    valid_time_utc = mrms_dl$valid_time_utc,
    valid_time_local = lubridate::with_tz(mrms_dl$valid_time_utc, GARDEN_TZ),
    rainfall_in = observed_rainfall_in,
    label = format(
      lubridate::with_tz(mrms_dl$valid_time_utc, GARDEN_TZ),
      "%a %b %d %H:%M"
    ),
    source_url = mrms_dl$url
  ),
  
  garden = list(
    name = GARDEN_NAME,
    lat = GARDEN_LAT,
    lon = GARDEN_LON,
    timezone = GARDEN_TZ
  ),
  
  rainfall_radius_miles = RAINFALL_RADIUS_MILES,
  
  observed_map_radius_multiplier = FORECAST_MAP_RADIUS_MULTIPLIER,
  
  last_refresh = Sys.time()
)

saveRDS(
  observed_cache,
  observed_cache_rds
)

message("Saved observed rainfall cache:")
message(observed_cache_rds)

message("Saved observed rainfall raster:")
message(observed_cache_tif)

message("Observed 24-hour rainfall at garden:")
message(round(observed_rainfall_in, 2), " in")