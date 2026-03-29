# ==============================================================================
# 01_data_engineering.R
# Prepares UK Tram GTFS data along with specific bus routes in Leeds 
# for which aspirations exist for future Tram Routes
# 1. Calculates full network stats (stops/routes) before spatial filtering.
# 2. Extracts continuous 7am-7pm hourly segments for detailed dashboard charts.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tidyverse)
  library(gtfstools)
  library(geosphere)
  library(purrr)
  library(stringr)
})

# --- 1. CONFIGURATION ---
ANALYSIS_DATE <- as.Date("2026-01-19") 
USE_TIMEPOINTS_ONLY <- TRUE

# Continuous 12-hour window (7am to 7pm)
ANALYSIS_START <- 7L * 3600L  
ANALYSIS_END   <- 19L * 3600L 

MIN_SPEED_KPH  <- 1
MAX_SPEED_KPH  <- 85

# Path to UK GTFS file downloaded from DfT BODS (adjust as needed)
GTFS_PATH <- "C://Data/itm_all_gtfs.zip"

agency_lookup <- tibble(
  city = c("Manchester", "Sheffield", "Nottingham", "Edinburgh", "Croydon", "Birmingham", "Blackpool", "Leeds", "Leeds"),
  public_name = c("Manchester Metrolink", "Sheffield Supertram", "NET", "Edinburgh Trams", 
                  "Croydon Tramlink", "West Midlands Metro", "Blackpool Tramway", "First Bus", "First Bus"),
  bods_agency_name = c("Metrolink", "South Yorkshire Future Tram", "Nottingham Express Transit (Tram)", 
                       "Edinburgh Trams", "London Tramlink", "West Midlands Metro", "Blackpool Transport", 
                       "First Leeds", "First Bradford")
)

# The 4 networks known to have valid shapefiles
proven_four <- c("Metrolink", "South Yorkshire Future Tram", "Edinburgh Trams", "Blackpool Transport")
proven_four_lookup <- agency_lookup %>% filter(bods_agency_name %in% proven_four)

# route short names of the relevant leeds bus routes (some are run by First Bradford)
leeds_bus_routes <- data.frame(route_short_name = c("72", "X11", "X6", "51", "52"),
                               agency_name = c("First Bradford", "First Bradford", "First Bradford", "First Leeds", "First Leeds")
                               )

# --- 2. DATA LOADING & PRE-PROCESSING ---
cat("\n[1/6] Loading and Filtering GTFS...\n")
gtfs_full <- read_gtfs(GTFS_PATH)

# Get the tram GTFS 
valid_agencies <- proven_four_lookup$bods_agency_name
gtfs_1 <- gtfs_full %>%
  filter_by_agency_id(gtfs_full$agency[agency_name %in% valid_agencies, agency_id]) %>%
  filter_by_route_type(0)

# Get the specific Leeds Bus GTFS 
gtfs_2 <- gtfs_full %>%
  filter_by_agency_id(gtfs_full$agency[agency_name %in% leeds_bus_routes$agency_name, agency_id]) %>%
  filter_by_route_id(gtfs_full$routes[route_short_name %in% leeds_bus_routes$route_short_name, route_id])
  
# merge GTFS files (tram + bus)
gtfs <- gtfstools::merge_gtfs(gtfs_1, gtfs_2)

# clean
rm(gtfs_1, gtfs_2, gtfs_full)

# Active Dates
day_col <- tolower(format(ANALYSIS_DATE, "%A"))
active_ids <- gtfs$calendar[start_date <= ANALYSIS_DATE & end_date >= ANALYSIS_DATE & get(day_col) == 1, service_id]

if ("calendar_dates" %in% names(gtfs)) {
  adds <- gtfs$calendar_dates[date == ANALYSIS_DATE & exception_type == 1, service_id]
  rems <- gtfs$calendar_dates[date == ANALYSIS_DATE & exception_type == 2, service_id]
  active_ids <- unique(c(setdiff(active_ids, rems), adds))
}

gtfs <- filter_by_service_id(gtfs, active_ids)
gtfs <- filter_by_trip_id(gtfs, gtfs$trips[!is.na(shape_id) & shape_id != "", trip_id])


# --- 2A. COMPILE DESCRIPTIVE STATS FOR DASHBOARD ---
cat("\n[2/6] Compiling exact Network Statistics...\n")

active_trips <- as.data.table(gtfs$trips)
active_stop_times <- as.data.table(gtfs$stop_times)

# Map stops to their city
stop_city_map <- active_stop_times %>%
  left_join(gtfs$stops[, .(stop_id, stop_name)], by = "stop_id") %>%
  left_join(active_trips[, .(trip_id, route_id)], by = "trip_id") %>%
  left_join(gtfs$routes[, .(route_id, agency_id)], by = "route_id") %>%
  left_join(gtfs$agency[, .(agency_id, agency_name)], by = "agency_id") %>%
  left_join(agency_lookup, by = c("agency_name" = "bods_agency_name")) %>%
  mutate(
    stop_name = stop_name %>%
      gsub("\\s*\\(.*?\\)", "", .) %>%          # remove bracketed content
      gsub("\\s+From\\s+.*$", "", .,            # remove " From ..." suffix
           ignore.case = TRUE) %>%
      gsub("\\s+To\\s+.*$", "", .,            # remove " To ..." suffix
           ignore.case = TRUE)) %>%   
  distinct(city, stop_name)

# Calculate Unique Stops
stops_count <- stop_city_map %>%
  group_by(city) %>%
  summarise(total_stops = n(), .groups = "drop")

# # Calculate Unique Routes
routes_count <- active_trips %>%
  left_join(gtfs$routes[, .(route_id, route_short_name, agency_id)], by = "route_id") %>%
  left_join(gtfs$agency[, .(agency_id, agency_name)], by = "agency_id") %>%
  left_join(agency_lookup, by = c("agency_name" = "bods_agency_name")) %>%
  left_join(gtfs$stop_times[, .(trip_id, stop_sequence, stop_id)], by = "trip_id") %>%
  left_join(gtfs$stops[, .(stop_id, stop_name)], by = "stop_id")  %>%
  filter(stop_sequence == 0) %>% 
  
  group_by(city) %>%
  summarise(
    # Manchester/Sheffield/Edinburgh use distinct route_short_name
    # Blackpool uses distinct first stop / trip_headsign combos (halved because of bidirectional travel)
    total_routes = if_else(
      first(city) == "Blackpool",
      ceiling(n_distinct(trip_headsign, stop_name) / 2),
      n_distinct(route_short_name)
    ),
    .groups = "drop"
  )

# Get Cleaner route names
blackpool_routes_full_names <- active_trips %>%
  left_join(gtfs$routes[, .(route_id, agency_id)], by = "route_id") %>%
  left_join(gtfs$agency[, .(agency_id, agency_name)], by = "agency_id") %>%
  filter(agency_name == "Blackpool Transport" ) %>%
  left_join(agency_lookup, by = c("agency_name" = "bods_agency_name")) %>%
  left_join(gtfs$stop_times[, .(trip_id, stop_sequence, stop_id)], by = "trip_id") %>%
  left_join(gtfs$stops[, .(stop_id, stop_name)], by = "stop_id")  %>%
  # filter for first stops only 
  filter(stop_sequence == 0) %>% 
  select(trip_headsign, stop_name) %>% 
  rename(first_stop = stop_name) %>%
  unique() %>%
  # Clean stop_name and trip_headsign:
  #   1. Remove anything in brackets e.g. "City Centre (Stop A)" -> "City Centre"
  #   2. For "X From Y" patterns (Sheffield-style), keep only X e.g. "Cathedral From City Hall" -> "Cathedral"
  #   3. For "X To Y" patterns, keep only X e.g. "Malin Bridge To City" -> "Malin Bridge"
  mutate(
    first_stop = first_stop %>%
      gsub("\\s*\\(.*?\\)", "", .) %>%          # remove bracketed content
      gsub("\\s+From\\s+.*$", "", .,            # remove " From ..." suffix
           ignore.case = TRUE) %>%
      gsub("\\s+To\\s+.*$", "", .,            # remove " To ..." suffix
           ignore.case = TRUE)  %>%
      trimws(),
    trip_headsign = trip_headsign %>%
      gsub("\\s*\\(.*?\\)", "", .) %>%
      gsub("\\s+From\\s+.*$", "", .,
           ignore.case = TRUE) %>%
      gsub("^.*\\s+To\\s+", "", .,
           ignore.case = TRUE) %>%
      trimws(),
    full_route_name = paste0(first_stop, " to ", trip_headsign, " - ", "Blackpool Tramway"))

not_blackpool_routes_full_names <- active_trips %>%
  left_join(gtfs$routes[, .(route_id, agency_id, route_short_name)], by = "route_id") %>%
  left_join(gtfs$agency[, .(agency_id, agency_name)], by = "agency_id") %>%
  left_join(agency_lookup, by = c("agency_name" = "bods_agency_name")) %>%
  filter(agency_name != "Blackpool Transport" ) %>%
  left_join(gtfs$stop_times[, .(trip_id, stop_sequence, stop_id)], by = "trip_id") %>%
  left_join(gtfs$stops[, .(stop_id, stop_name)], by = "stop_id")  %>%
  # filter for first stops only 
  filter(stop_sequence == 0) %>% 
  select(trip_headsign, stop_name, route_short_name) %>% 
  rename(first_stop = stop_name) %>%
  unique() %>%
  # Clean stop_name and trip_headsign:
  #   1. Remove anything in brackets e.g. "City Centre (Stop A)" -> "City Centre"
  #   2. For "X From Y" patterns (Sheffield-style), keep only X e.g. "Cathedral From City Hall" -> "Cathedral"
  #   3. For "X To Y" patterns, keep only X e.g. "Malin Bridge To City" -> "Malin Bridge"
  mutate(
    first_stop = first_stop %>%
      gsub("\\s*\\(.*?\\)", "", .) %>%          # remove bracketed content
      gsub("\\s+From\\s+.*$", "", .,            # remove " From ..." suffix
           ignore.case = TRUE) %>%
      gsub("\\s+To\\s+.*$", "", .,            # remove " To ..." suffix
           ignore.case = TRUE)  %>%
      trimws(),
    trip_headsign = trip_headsign %>%
      gsub("\\s*\\(.*?\\)", "", .) %>%
      gsub("\\s+From\\s+.*$", "", .,
           ignore.case = TRUE) %>%
      gsub("^.*\\s+To\\s+", "", .,
           ignore.case = TRUE) %>%
      trimws(),
    full_route_name = paste0(first_stop, " to ", trip_headsign, " - ", route_short_name) 
  ) %>% select(-route_short_name)
  
routes_full_names <- rbind(not_blackpool_routes_full_names, blackpool_routes_full_names)
head(routes_full_names, 100)


network_stats <- stops_count %>% left_join(routes_count, by = "city")
print(network_stats)

# Export Network Stats
dir.create("data", showWarnings = FALSE)
saveRDS(network_stats, "data/network_stats.rds")


# --- 3. STATIC TOPOLOGY ENGINE ---
cat("\n[3/6] Building Static Topology (Geosphere)...\n")
trips <- as.data.table(gtfs$trips)
stop_times <- as.data.table(gtfs$stop_times)
stops <- as.data.table(gtfs$stops)
shapes <- as.data.table(gtfs$shapes)

setkey(shapes, shape_id, shape_pt_sequence)

shapes[, dist_inc := distHaversine(
  cbind(shape_pt_lon, shape_pt_lat),
  cbind(shift(shape_pt_lon, fill=first(shape_pt_lon)), shift(shape_pt_lat, fill=first(shape_pt_lat)))
), by = shape_id]
shapes[, shape_dist_traveled := cumsum(dist_inc), by = shape_id]
head(shapes,1)

rep_trips <- trips[, .SD[1], by = shape_id, .SDcols = c("trip_id", "direction_id")]
head(rep_trips, 1)

rep_stops <- stop_times[trip_id %in% rep_trips$trip_id] %>%
  left_join(trips[, .(trip_id, shape_id)], by = "trip_id") %>%
  left_join(stops[, .(stop_id, stop_lat, stop_lon, stop_name)], by = "stop_id") %>%
  as.data.table()
head(rep_stops, 1)

dist_lookup <- rep_stops[, {
  s_pts <- shapes[shape_id == .BY$shape_id]
  if(nrow(s_pts) > 0) {
    stop_mat <- cbind(.SD$stop_lon, .SD$stop_lat)
    shape_mat <- cbind(s_pts$shape_pt_lon, s_pts$shape_pt_lat)
    indices <- apply(stop_mat, 1, function(r) which.min(distHaversine(r, shape_mat)))
    
    .(stop_id = stop_id, 
      stop_sequence = stop_sequence,
      shape_dist_m = as.numeric(s_pts$shape_dist_traveled[indices]),
      shape_pt_seq = as.numeric(s_pts$shape_pt_sequence[indices])) 
  }
}, by = shape_id]

head(dist_lookup, 1)


# --- 4. DYNAMIC SCHEDULE & SPEEDS ---
cat("\n[4/6] Extracting Dynamic Hourly Segments (07:00 to 19:00)...\n")
to_sec <- function(x) {
  parts <- tstrsplit(x, ":")
  as.integer(parts[[1]])*3600L + as.integer(parts[[2]])*60L + as.integer(parts[[3]])
}

st <- as.data.table(gtfs$stop_times)
st[, dep_sec := to_sec(departure_time)]
st[, arr_sec := to_sec(arrival_time)]

# Filter continuously from 7am to 7pm
st <- st[dep_sec >= ANALYSIS_START & dep_sec <= ANALYSIS_END]

if(USE_TIMEPOINTS_ONLY && "timepoint" %in% names(st)) {
  st <- st[timepoint == 1 | stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence), .SD, by=trip_id]
}

st[, `:=`(
  next_arr_sec = shift(arr_sec, type = "lead"),
  next_stop_id = shift(stop_id, type = "lead")
), by = trip_id]

hops <- st[!is.na(next_stop_id)]
head(hops,1)

# Automatic Hourly Bins (7, 8, 9, ... 18)
hops[, hour_bin := floor(dep_sec / 3600)]

hops <- hops[trips[, .(trip_id, shape_id, route_id, trip_headsign)], on = "trip_id", nomatch = 0]
hops[, duration_h := (next_arr_sec - dep_sec) / 3600]
dynamic_hops <- hops[duration_h > 0]
head(dynamic_hops,1)

# Compute Speeds
merged <- dynamic_hops[dist_lookup, on = .(shape_id, stop_id), nomatch = 0]
setnames(merged, c("shape_dist_m", "shape_pt_seq"), c("dist_start", "seq_start"))

merged <- merged[dist_lookup, on = .(shape_id, next_stop_id = stop_id), nomatch = 0]
setnames(merged, c("shape_dist_m", "shape_pt_seq"), c("dist_end", "seq_end"))

merged[, dist_m := dist_end - dist_start]
clean <- merged[dist_m > 0 & duration_h > 0]
clean[, speed_kph := (dist_m / 1000) / duration_h]
head(clean, 1)


# Outlier Filter
speeds <- clean[speed_kph >= MIN_SPEED_KPH & speed_kph <= MAX_SPEED_KPH]


# --- 5. BUILD SPATIAL SEGMENTS FOR DASHBOARD ---
cat("\n[5/6] Aggregating Spatial Segments for Map...\n")

routes <- as.data.table(gtfs$routes)[, .(route_id, route_short_name, agency_id)]
agencies <- as.data.table(gtfs$agency)[, .(agency_id, agency_name)]

speeds <- speeds %>%
  left_join(routes, by = "route_id") %>%
  left_join(agencies, by = "agency_id") %>%
  left_join(agency_lookup, by = c("agency_name" = "bods_agency_name")) %>%
  left_join(stops[, .(stop_id, prev_stop_name = stop_name)], by = "stop_id") %>%
  left_join(stops[, .(stop_id, stop_name = stop_name)], by = c("next_stop_id" = "stop_id")) %>%
  as.data.table()

head(speeds, 100)


# --- Add the First and Last stops ---
cat("      Generating trip direction metadata...\n")

# Identify the first and last stop for every trip
trip_endpoints <- stop_times[, .(
  first_stop_id = stop_id[which.min(stop_sequence)],
  last_stop_id = stop_id[which.max(stop_sequence)]
), by = trip_id]

# Join with stop names and agency/city info
trip_first_last <- trip_endpoints %>%
  left_join(stops[, .(stop_id, first_stop_raw = stop_name)], by = c("first_stop_id" = "stop_id")) %>%
  left_join(stops[, .(stop_id, last_stop_raw = stop_name)], by = c("last_stop_id" = "stop_id")) %>%
  left_join(trips[, .(trip_id, route_id)], by = "trip_id") %>%
  left_join(routes[, .(route_id, agency_id)], by = "route_id") %>%
  left_join(agencies[, .(agency_id, agency_name)], by = "agency_id") %>%
  left_join(agency_lookup, by = c("agency_name" = "bods_agency_name")) %>%
  as.data.table()

# Clean the stop names using the same logic used elsewhere in the script
clean_stop_name <- function(x) {
  x %>%
    gsub("\\s*\\(.*?\\)", "", .) %>%
    gsub("\\s+From\\s+.*$", "", ., ignore.case = TRUE) %>%
    gsub("\\s+To\\s+.*$", "", ., ignore.case = TRUE) %>%
    trimws()
}

trip_first_last[, first_stop := clean_stop_name(first_stop_raw)]
trip_first_last[, last_stop := clean_stop_name(last_stop_raw)]


# ------------------------------------------------------------------------------
# Add trip counts per direction (first_stop, last_stop) directly from trips
# ------------------------------------------------------------------------------

# 1. Identify trips that are in the analysis window 
window_trips <- unique(dynamic_hops$trip_id)

# 2. Add an in_window flag to trip_first_last 
trip_first_last[, in_window := trip_id %in% window_trips]

# 3. Count trips per direction (city, first_stop, last_stop) that are in window
trip_counts <- trip_first_last[in_window == TRUE, .(trip_count_7to19 = .N), 
                               by = .(city, first_stop, last_stop)]

# 4. Merge counts back to trip_first_last (so each trip knows its direction's count)
trip_first_last <- merge(trip_first_last, trip_counts, 
                         by = c("city", "first_stop", "last_stop"), 
                         all.x = TRUE)
trip_first_last[is.na(trip_count_7to19), trip_count_7to19 := 0]   # zero trips in window

# 5. Join the trip count onto the speeds table by trip_id
speeds <- merge(speeds, 
                trip_first_last[, .(trip_id, trip_count_7to19)], 
                by = "trip_id", 
                all.x = TRUE)

glimpse(speeds)

# 6. Filter speeds to keep only trips from directions with at least, say, 5 trips
MIN_TRIPS <- 20
speeds_filtered <- speeds[trip_count_7to19 >= MIN_TRIPS]

# Aggregate by hour_bin and physical segment
segment_summary <- speeds_filtered[, .(
  avg_speed_kph = mean(speed_kph),
  dist_m = mean(dist_m),
  trip_count = .N,
  seq_start = first(seq_start),
  seq_end = first(seq_end)
), by = .(city, agency_name, route_short_name, trip_headsign, 
          prev_stop_name, stop_name, hour_bin, shape_id)]

# filter this down to trams running 7-7
segment_summary <- segment_summary %>%
  group_by(city, agency_name, route_short_name, trip_headsign, 
           prev_stop_name, stop_name, shape_id) %>%
  filter(all(7:18 %in% hour_bin)) %>%
  ungroup()

create_linestring <- function(s_id, start_seq, end_seq) {
  s_min <- min(start_seq, end_seq, na.rm = TRUE)
  s_max <- max(start_seq, end_seq, na.rm = TRUE)
  pts <- shapes[shape_id == s_id & shape_pt_sequence >= s_min & shape_pt_sequence <= s_max]
  if(nrow(pts) < 2) return(st_linestring()) 
  coords <- as.matrix(pts[, .(shape_pt_lon, shape_pt_lat)])
  st_linestring(coords) 
}

cat("      Generating spatial lines...\n")
segment_sf <- segment_summary %>%
  mutate(
    geometry = st_sfc(pmap(list(shape_id, seq_start, seq_end), create_linestring), crs = 4326)
  ) %>%
  st_as_sf()

segment_sf <- segment_sf[!st_is_empty(segment_sf), ]

segment_sf <- segment_sf %>%
  mutate(
    trip_headsign = str_remove(trip_headsign, "\\s*\\(.*\\)"),
    prev_stop_name = str_remove(prev_stop_name, "\\s*\\(.*\\)"),
    stop_name = str_remove(stop_name, "\\s*\\(.*\\)")
  )

# --- 6. EXPORT ---
cat("\n[6/6] Saving final data structure...\n")
saveRDS(segment_sf, "data/processed_tram_segments.rds")

# --- 7. METADATA EXPORT ---
cat("\n[7/7] Saving metadata...\n")
metadata <- list(
  analysis_date = ANALYSIS_DATE,
  processed_timestamp = Sys.time(),
  data_source = "DfT Bus Open Data Service (BODS)"
)
saveRDS(metadata, "data/metadata.rds")

cat("=== DATA ENGINEERING COMPLETE ===\n")
