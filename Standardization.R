
# 0. Initialize

library(tidyverse)
library(lubridate)

# Set WD if not working in an R project (you should be!)
# setwd("C:/Users/TBRUSH/R/SPCS_R")

# Set project name
proj_name <- "SPCS"


# 1. Standardize camera deployments

stations <- read_csv("raw_data/SPCS/stations.csv")
camera_checks <- read_csv("raw_data/SPCS/camera_checks.csv")
images <- read_csv("raw_data/SPCS/images.csv")

deployment  <- full_join(camera_checks, stations, by = "station_id")
head(deployment)

# DELETE THE FOLLOWING STEP IF YEARS ARE CORRECT IN ORIG_FILE FIELD ->

images$orig_file <- images$orig_file %>%
  str_replace_all("2020", "2021")

#get first and last image from each station
# derive date_time from orig_file to ensure wrong timestamps don't show up

first_last <- images %>%
  separate(orig_file, c(NA, "year", "month", "day", "hour", "minute", "second"), extra = "drop") %>%
  unite(date_time, c("day":"year", "hour":"second"), sep = "-", remove = FALSE)

first_last$date_time <-
  dmy_hms(first_last$date_time, tz = "UTC")

# find first and last images (will join to eff dataset)
first_image <- first_last %>%
  group_by(station_id) %>%
  slice_min(date_time) %>%
  mutate(first_image = date_time) %>%
  select(station_id, first_image)
last_image <- first_last %>%
  group_by(station_id) %>%
  slice_max(date_time) %>%
  mutate(last_image = date_time) %>%
  select(station_id, last_image)
first_last <- first_last %>%
  distinct(station_id) %>%
  full_join(first_image, by="station_id") %>%
  full_join(last_image, by="station_id") %>%
  select(station_id, first_image, last_image)
  
deployment <- deployment %>%
  full_join(first_last, by="station_id")
deployment_short <- select(deployment, 
                           station_id,
                           treatment, 
                           latitude,
                           longitude, 
                           check_date,
                           stop_date,
                           first_image,
                           last_image,
                           media_recovered,
                           bait,
                           feature,
                           quiet_period,
                           camera_height, 
                           camera_angle,
                           camera_distance,
                           camera_status)

write.csv(deployment_short, paste0("raw_data/",proj_name,"_deployment_data.csv"), row.names = F)


# 2. Standardize station data

# Add treatment column from deployments
treatment <- select(deployment, station_id, treatment)

# Select only first occurrence, since we're pulling from all cam checks
treatment <- treatment %>%
  group_by(station_id) %>%
  filter(station_id == min(station_id)) %>%
  slice(1) %>%
  ungroup()

stations <- left_join(stations, treatment, by = "station_id")

# Trim down
stations <- select(stations, -station_tbl_id)

# Export to .csv
write.csv(stations, paste0("raw_data/",proj_name,"_station_data.csv"), row.names = F)


# 3. Standardize identifications from database output

idents <- read.csv("raw_data/SPCS/images_idents.csv")

# DELETE THE FOLLOWING STEP IF YEARS ARE CORRECT IN ORIG_FILE FIELD ->

idents$orig_file <- idents$orig_file %>%
  str_replace_all("2020", "2021")

# resume

ud.dat <- idents



# rename to standardized columns
# run 'colnames(ud.dat)' to see column names

ud.dat <- ud.dat %>%
  mutate(species = latin_name) %>%
  rename(age = age_category,
         count = species_count)

# derive date_time from orig_file to ensure wrong timestamps don't show up in data
ud.dat <- ud.dat %>%
  separate(orig_file, c(NA, "year", "month", "day", "hour", "minute", "second"), extra = "drop") %>%
  unite(date_time, c("day":"year", "hour":"second"), sep = "-", remove = FALSE)

ud.dat$date_time <-
  dmy_hms(ud.dat$date_time, tz = "UTC")

ud.dat$month <- as.numeric(ud.dat$month)
ud.dat$day <- as.numeric(ud.dat$day)
ud.dat$hour <- as.numeric(ud.dat$hour)
ud.dat$minute <- as.numeric(ud.dat$minute)
ud.dat$second <- as.numeric(ud.dat$second)

# find first and last images (will join to eff dataset)
first_last <- ud.dat %>%
  group_by(station_id) %>%
  slice_min(date_time)

# remove misfires and staff images
# remove deleted rows
ud.dat <- filter(ud.dat, deleted == "f")
ud.dat$misfire[ud.dat$misfire=="f"] <- F
ud.dat <- filter(ud.dat, misfire == F)
ud.dat$staff[ud.dat$staff=="f"] <- F
ud.dat <- filter(ud.dat, staff == F)

# remove extraneous species column
ud.dat <- subset(ud.dat, select = -c(latin_name))

# small standardizations
ud.dat$`project_id` <- proj_name 
ud.dat$time_zone <- NA
ud.dat$time_zone <- "UTC-8" # Change if in different TZ

ud.dat$collar[ud.dat$collar=="f"] <- F
ud.dat$collar[ud.dat$collar=="t"] <- T
ud.dat <- ud.dat %>%
  mutate(group_count =
           ifelse(ud.dat$group_count == 100, NA, group_count)) %>%
  mutate(snow_cover =
           ifelse(ud.dat$snow_cover == 100, NA, snow_cover))

# pare down to desired columns only
ud.dat <- select(ud.dat,
                 project_id,
                 station_id,
                 misfire,
                 species,
                 common_names,
                 date_time,
                 time_zone,
                 count,
                 group_count,
                 age,
                 sex,
                 behaviour,
                 collar,
                 collar_tags,
                 snow_cover,
                 comments)


# Write to csv
write.csv(ud.dat, paste0("raw_data/",proj_name,"_detection_data.csv"), row.names = F)