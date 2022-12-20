
# 0. Initialize

library(tidyverse)
library(lubridate)

getwd()

# Set WD if not working in an R project (you should be!)
# setwd("C:/")

# Set project name
proj_name <- "SPCS"

# Set timezone as UTC if cameras don't account for DST, otherwise set as local tz
tz <- "UTC"

# 1. Standardize camera deployments
# Extract database output to input/raw_data

stations <- read_csv(paste0("input/raw_data/",proj_name, "/stations.csv"))
camera_checks <- read_csv(paste0("input/raw_data/",proj_name, "/camera_checks.csv"))
images <- read_csv(paste0("input/raw_data/",proj_name, "/images.csv"))
idents <- read_csv(paste0("input/raw_data/",proj_name,"/images_idents.csv"))

deployment  <- full_join(camera_checks, stations, by = "station_id")
head(deployment)


# get first and last image from each station

img_dates <- images %>%
  mutate(date_time = as.POSIXct(timestamp_pst, tz=tz))

# if needed, can derive date_time from orig_file to ensure wrong timestamps don't show up (uncomment below)

# img_dates <- images %>%
#   separate(orig_file, c(NA, "year", "month", "day", "hour", "minute", "second"), extra = "drop") %>%
#   unite(date_time, c("day":"year", "hour":"second"), sep = "-", remove = FALSE)
# 
# img_dates$date_time <-
#   dmy_hms(img_dates$date_time, tz = tz)

summary(img_dates$date_time)

# if any cameras had date set up wrong, fix below

#06 - hour 2 off
#09 - year 1 behind
#21 - jumped from 5-22 to 6-09
#48 - 1 hr fast
#54 - date


# COQ07 -> 877 days, 20 hrs behind until Oct 3
img_dates_new <- img_dates %>%
  mutate(date_time = if_else(station_id=="SPCS06", date_time + hours(2),
                             if_else(station_id=="SPCS09", date_time + years(1),
                                     if_else(station_id=="SPCS21" & date_time > as.POSIXct("2022-05-21", tz = tz), date_time - days(18)))
                             , 
                                 date_time + (((877*24)+20)*60*60),
# COQ04 -> year is 2018 instead of 2019 until Oct 3                                 
                                 if_else(station_id=="COQ04" & date_time < as.POSIXct("2019-10-03", tz=tz),
                                         as.POSIXct(str_replace(date_time, "2018", "2019"), tz = tz),
                                         date_time))
         )
# make sure it looks right
summary(img_dates$date_time)

# find first and last images (will join to eff dataset)
first_image <- img_dates %>%
  group_by(station_id) %>%
  summarize(first_image = min(date_time))
last_image <- img_dates %>%
  group_by(station_id) %>%
  summarize(last_image = max(date_time))
first_last <- full_join(first_image, last_image, by="station_id")
  
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

write.csv(deployment_short, paste0("input/raw_data/",proj_name,"_deployment_data.csv"), row.names = F)


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
write.csv(stations, paste0("input/raw_data/",proj_name,"_station_data.csv"), row.names = F)


# 3. Standardize identifications from database output

# derive correct timestamp from img_dates

ud.dat <- left_join(idents, img_dates[,c(2,30)], by="image_id")

# rename to standardized columns
# run 'colnames(ud.dat)' to see column names

ud.dat <- ud.dat %>%
  mutate(species = latin_name) %>%
  rename(age = age_category,
         count = species_count)

# remove misfires and staff images
# remove deleted rows
ud.dat <- filter(ud.dat, deleted == F)
ud.dat <- filter(ud.dat, misfire == F)
ud.dat <- filter(ud.dat, staff == F)

# remove extraneous species column
ud.dat <- subset(ud.dat, select = -c(latin_name))

# small standardizations
ud.dat$`project_id` <- proj_name 
ud.dat$time_zone <- "UTC-8" # Change if in different TZ

# ud.dat <- ud.dat %>%
#   mutate(group_count =
#            ifelse(ud.dat$group_count == 100, NA, group_count)) %>%
#   mutate(snow_cover =
#            ifelse(ud.dat$snow_cover == 100, NA, snow_cover))

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
write.csv(ud.dat, paste0("input/raw_data/",proj_name,"_detection_data.csv"), row.names = F)
