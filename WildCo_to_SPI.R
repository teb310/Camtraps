# Formatting from WildCo to SPI

#1. Prepare R ####

# Load packages

library(tidyverse)
library(lubridate)

# Set timezone (if cameras don't track daylight savings, use UTC, otherwise use local tz)

tz = "UTC" 

# Set the timespan of each data check (can add >1 check)
# E.g. If your first set of camera checks occurred in April and May of 2021, write `"2021-04"|"2021-05"`.

check_1 <- "2021-09"

# Set x  and y to choose how to measure the time your cameras were deployed: days, hours, or minutes.
# y should be the same as x but with quotes
D <- 86400
H <- 3600
M <- 60

x <- D
y <- "D"

# Read in files (replace with own filepaths)

cameras <- read_csv("raw_data/cameras.csv")
images <- read_csv("raw_data/SPCS/images.csv")
images_idents <- read_csv("raw_data/SPCS/images_idents.csv")
camera_checks <- read_csv("raw_data/SPCS/camera_checks.csv")
stations <- read_csv("raw_data/SPCS/stations.csv")



# 2. Camera information ####

## Prepare data ####

# Join existing tables and organize variables

joined_cameras <- camera_checks %>%
  filter(media_recovered == TRUE) %>%
  inner_join(cameras, by = "camera_label") %>%
  select(station_id, camera_label, camera_comments)
camera_info <- 
  inner_join(stations, joined_cameras, by = "station_id") %>%
  inner_join(images, by = "station_id") %>%
  distinct(station_id, .keep_all = TRUE) %>%
  add_column(
    study_area_name = "Sechelt Peninsula", 
    UTM_zone = NA, 
    easting = NA, 
    northing = NA, 
    parent_sample_station_label = NA,
    site_description_date = NA) %>%
  select(
    study_area_name,
    camera_label, 
    parent_sample_station_label,
    UTM_zone:northing,
    longitude:latitude, 
    make:model, 
    camera_comments, 
    site_comments,
    site_description_date)

## Format to SPI standard ####

camera_info <- camera_info %>%
  mutate(
    make = case_when(
      str_starts(make, regex("Browning", ignore_case = TRUE)) ~ "BR", 
      str_starts(make, regex("Bushnell", ignore_case = TRUE)) ~ "BU",
      str_starts(make, regex("Cabelas", ignore_case = TRUE)) ~ "CA",
      str_starts(make, regex("Covert", ignore_case = TRUE)) ~ "CO",
      str_starts(make, regex("LTL", ignore_case = TRUE)) ~ "LA",
      str_starts(make, regex("Moultrie", ignore_case = TRUE)) ~ "MO",
      str_starts(make, regex("Reconyx", ignore_case = TRUE)) ~ "RE",
      str_starts(make, regex("RidgeTec", ignore_case = TRUE)) ~ "RT",
      str_starts(make, regex("Spartan", ignore_case = TRUE)) ~ "SN",
      str_starts(make, regex("Spypoint", ignore_case = TRUE)) ~ "SP",
      str_starts(make, regex("Stealth", ignore_case = TRUE)) ~ "SC",
      str_starts(make, regex("TrailMaster", ignore_case = TRUE)) ~ "TM",
      str_starts(make, regex("Wildgame", ignore_case = TRUE)) ~ "WI")) %>%
  arrange(camera_label) 

## Write to .csv ####

write.table(camera_info, "processed_data/SPI/camera_information.csv", col.names = FALSE, row.names = FALSE, sep = ",", na = "")

# File can be found in WD

## Copy & paste ####

# Open template at https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/wildlife-wildlife-habitat/wildlife-inventory/wildlife_camera_template.xlsm?forcedownload=true

# Find sheet named "Camera Information"

# Copy & paste into sheet



# 3. Camera setup & checks ####
## Prepare data ####

# Separate deployment and check dates (WILL HAVE TO TWEAK IF >1 CHECK)
deployment <- camera_checks %>%
  arrange(check_date) %>%
  distinct(station_id, .keep_all = TRUE) %>%
  select(camera_label, check_date)
retrieval <- camera_checks %>%
  filter(str_detect(check_date, check_1)) %>%
  mutate(end_date = if_else(is.na(stop_date), check_date, stop_date)) %>%
  select(camera_label:station_id, end_date, quiet_period, sensitivity, images_per_trigger, bait)
camera_setup_checks <- 
  left_join(deployment, retrieval, by = "camera_label")

# Set date to deployment date & end date to check date (tweak if >1 check)

camera_setup_checks <- camera_setup_checks %>%
  mutate(date = check_date) %>%
  filter(!is.na(end_date)) 

# Set up total time column

camera_setup_checks <- camera_setup_checks %>%
  mutate(
    total_time = (int_length(date %--% end_date) / x))

## Format to SPI standard ####

camera_setup_checks$date <-
  format(as.Date(camera_setup_checks$date, tz = tz, "%Y-%m-%d"), "%d %b %Y")
camera_setup_checks$end_date <-
  format(as.Date(camera_setup_checks$end_date, tz = tz, "%Y-%m-%d"), "%d %b %Y")

## Organize and finalize ####

camera_setup_checks <- camera_setup_checks %>%
  add_column(
    study_area_name = "Sechelt Peninsula", 
    time = NA, 
    end_time = NA, 
    unit_total_time = y, 
    trigger_timing = NA,
    video_length = NA,
    comments = NA,
    visit_photos = NA) %>%
  select(
    study_area_name,
    camera_label, 
    date,
    time,
    end_date,
    end_time,
    total_time,
    unit_total_time,
    quiet_period,
    sensitivity,
    trigger_timing,
    images_per_trigger,
    video_length,
    bait,
    comments:visit_photos) %>%
  arrange(camera_label) %>%
  mutate(bait = na_if(bait, "None"))

## Write to .csv ####

write.table(camera_setup_checks, "processed_data/SPI/camera_setup_checks.csv", col.names = FALSE, row.names = FALSE, sep = ",", na = "")

# File can be found in WD

## Copy & paste ####

# Open template

# Find sheet named "Camera Setup and Checks"

# Copy & paste



# 4. Sequence image data ####
## Prepare data ####

# Get rid of unnecessary data and re-order variables

image_data <- images_idents %>%
  filter(deleted == FALSE, staff == FALSE, misfire == FALSE) %>%
  select(
    station_id, 
    orig_file, 
    temperature, 
    latin_name, 
    group_count, 
    age_category, 
    sex, 
    behaviour, 
    staff_name, 
    comments)

# Prepare date & time 

image_data <- image_data %>%
  separate(orig_file, c(NA, "year", "month", "day", "hour", "minute", "second"), extra = "drop") %>%
  unite(date_time, c("day":"year", "hour":"second"), sep = "-")

image_data$date_time <-
  dmy_hms(image_data$date_time, tz = tz)

## Separate independent events ####

events <- image_data %>%
  arrange(station_id, date_time) %>%
  group_by(station_id, latin_name) %>%
  mutate(duration = int_length(date_time %--% lag(date_time)), event_id = 9999)

# (Set mins to your chosen independence interval)

mins <- 30
seq <- as.numeric(1)
for(i in 2:nrow(events)) {
  events$event_id[i-1]  <- paste0(format(seq, scientific = F))
  if(is.na(events$duration[i]) | abs(events$duration[i]) > (mins * 60)){
    seq <- seq + 1}}
events$event_id <- as.numeric(events$event_id)

# Fix last row of event_id

if(events$duration[nrow(events)] < (mins * 60)|
   is.na(events$duration[nrow(events)])){
  events$event_id[nrow(events)] <- events$event_id[nrow(events)-1]
} else{
  events$event_id[nrow(events)] <- paste0("E",format(seq+1, scientific = F))
}

# Show one row per group & drop event columns

events <- events %>%
  ungroup() %>%
  distinct(event_id, .keep_all = TRUE) %>%
  select(!(duration:event_id))

## Format values to SPI standard ####

image_data <- events %>%
  add_column(study_area_name = "Sechelt Peninusla", .before = "station_id") %>%
  add_column(sequence_definition = mins, .after = "temperature") %>%
  separate(date_time, c("date", "time"), sep = " ")
image_data$date <-
  format(parse_date(image_data$date), "%d %b %Y")
image_data <- image_data %>%
  rename(
    camera_label = station_id, 
    species_code = latin_name, 
    count = group_count, 
    life_stage_code = age_category,
    sex_code = sex,
    activity_code = behaviour,
    surveyor = staff_name) %>%
  mutate(
    count = na_if(count, 100)) %>%
  #ADD ALL SPECIES ON YOUR PROJECT
  mutate(
    species_code = case_when(
      str_detect(species_code, regex("Odocoileus hemionus", ignore_case = TRUE)) ~ "M-ODHE",
      str_detect(species_code, regex("Ursus americanus", ignore_case = TRUE)) ~ "M-URAM",
      str_detect(species_code, regex("Cervus canadensis", ignore_case = TRUE)) ~ "M-CEEL-RO",
      str_detect(species_code, regex("Canis latrans", ignore_case = TRUE)) ~ "M-CALA",
      str_detect(species_code, regex("Canis lupus", ignore_case = TRUE)) ~ "M-CALU",
      str_detect(species_code, regex("Lynx rufus", ignore_case = TRUE)) ~ "M-LYRU",
      str_detect(species_code, regex("Procyon lotor", ignore_case = TRUE)) ~ "M-PRLO",
      str_detect(species_code, regex("Tamiasciurus douglasii", ignore_case = TRUE)) ~ "M-TADO",
      str_detect(species_code, regex("Homo sapiens", ignore_case = TRUE)) ~ "M-HOSA",
      str_detect(species_code, regex("Felis catus", ignore_case = TRUE)) ~ "M-FECA",
      str_detect(species_code, regex("Bird spp.", ignore_case = TRUE)) ~ "B",
      str_detect(species_code, regex("Sylvilagus floridanus", ignore_case = TRUE)) ~ "M-SYFL",
      str_detect(species_code, regex("Canis familiaris", ignore_case = TRUE)) ~ "M-CAFA")) %>%
  mutate(
    life_stage_code = if_else(life_stage_code == "Adult + Juvenile", "AdultJuvenile", life_stage_code)) %>%
  mutate(
    life_stage_code = case_when(
      str_detect(life_stage_code, regex("AdultJuvenile", ignore_case = TRUE)) ~ "A,J",
      str_detect(life_stage_code, regex("Sub-adult", ignore_case = TRUE)) ~ "S",
      str_detect(life_stage_code, regex("Adult", ignore_case = TRUE)) ~ "A",
      str_detect(life_stage_code, regex("Juvenile", ignore_case = TRUE)) ~ "J",
      str_detect(life_stage_code, regex("Unknown", ignore_case = TRUE)) ~ "UC")) %>%
  mutate(
    sex_code = case_when(
      str_detect(sex_code, regex("Unknown", ignore_case = TRUE)) ~ "UC",
      str_detect(sex_code, regex("Female", ignore_case = TRUE)) ~ "F",
      str_detect(sex_code, regex("Male", ignore_case = TRUE)) ~ "M",
      str_detect(sex_code, regex("Mixed", ignore_case = TRUE)) ~ "M,F")) %>%
  mutate(
    adult_males = case_when(
      sex_code == "M" & life_stage_code == "A" ~ count,
      sex_code == "M" & life_stage_code == "A,J" & count == 2 ~ 1,
      sex_code == "M,F" & life_stage_code == "A" & count == 2 ~ 1,
      sex_code == "M,F" & life_stage_code == "A,J" & count == 3 ~ 1,
      sex_code == "M" & life_stage_code == "A,J" | 
        sex_code == "M,F" & life_stage_code == "A" | 
        sex_code == "M,F" & life_stage_code == "A,J" ~ as.numeric(str_extract(comments, "\\d(?= male| males)")))) %>%
  mutate(
    adult_females = case_when(
      sex_code == "F" & life_stage_code == "A" ~ count,
      sex_code == "F" & life_stage_code == "A,J" & count == 2 ~ 1,
      sex_code == "M,F" & life_stage_code == "A" & count == 2 ~ 1,
      sex_code == "M,F" & life_stage_code == "A,J" & count == 3 ~ 1,
      sex_code == "F" & life_stage_code == "A,J" | 
        sex_code == "M,F" & life_stage_code == "A" | 
        sex_code == "M,F" & life_stage_code == "A,J" ~ as.numeric(str_extract(comments, "\\d(?= female| females)|\\d\\d(?= female| females)")))) %>%
  mutate(
    adult_unclassified = case_when(
      sex_code == "UC" & life_stage_code == "A" ~ count,
      sex_code == "UC" & life_stage_code == "A,J" & count == 2 ~ 1)) %>%
  mutate(
    juvenile_males = case_when(
      sex_code == "M" & life_stage_code == "J" ~ count)) %>%
  mutate(
    juvenile_females = case_when(
      sex_code == "F" & life_stage_code == "J" ~ count)) %>%
  mutate(
    juvenile_unclassified = case_when(
      sex_code == "UC" & life_stage_code == "J" ~ count,
      life_stage_code == "A,J" & count == 2 ~ 1,
      sex_code == "M,F" & life_stage_code == "A,J" & count == 3 ~ 1,
      sex_code == "F" & life_stage_code == "A,J" | 
        sex_code == "UC" & life_stage_code == "A,J" |
        sex_code == "M" & life_stage_code == "A,J" | 
        sex_code == "M,F" & life_stage_code == "A,J" ~ as.numeric(str_extract(comments, "\\d(?= juvenile| juveniles)")))) %>%
  mutate(
    yearling_males = NA) %>%
  mutate(
    yearling_females = NA) %>%
  mutate(
    yearling_UC = NA) %>%
  mutate(
    males_UC = case_when(
      sex_code == "M" & life_stage_code == "UC" ~ count)) %>%
  mutate(
    females_UC = case_when(
      sex_code == "F" & life_stage_code == "UC" ~ count)) %>%
  mutate(
    unclassified = case_when(
      sex_code == "UC" & life_stage_code == "UC" ~ count)) %>%
  mutate(
    sex_code = ifelse(sex_code == "M,F" | life_stage_code == "A,J", NA, sex_code)) %>%
  mutate(
    life_stage_code = na_if(life_stage_code, "A,J")) %>%
  mutate(
    animal_id = NA) %>%
  mutate(
    human_transport_mode_code = case_when(
      str_detect(activity_code, regex("hiking", ignore_case = TRUE)) ~ "OF",
      str_detect(activity_code, regex("mountain biking", ignore_case = TRUE)) ~ "NPB",
      str_detect(activity_code, regex("horse", ignore_case = TRUE)) ~ "OH",
      str_detect(activity_code, regex("quad", ignore_case = TRUE)) ~ "ORV",
      str_detect(activity_code, regex("motorbike", ignore_case = TRUE)) ~ "ORV",
      str_detect(activity_code, regex("snowmobile", ignore_case = TRUE)) ~ "ORV",
      str_detect(activity_code, regex("truck/car", ignore_case = TRUE)) ~ "SPV",
      str_detect(activity_code, regex("unknown vehicle", ignore_case = TRUE)) ~ "DC")) %>%
  mutate(
    activity_code = case_when(
      str_detect(activity_code, regex("traveling", ignore_case = TRUE)) ~ "TU",
      str_detect(activity_code, regex("foraging", ignore_case = TRUE)) ~ "FD",
      str_detect(activity_code, regex("inspecting camera", ignore_case = TRUE)) ~ "IC",
      str_detect(activity_code, regex("hunting", ignore_case = TRUE)) ~ "HU",
      str_detect(activity_code, regex("grooming", ignore_case = TRUE)) ~ "GR",
      str_detect(activity_code, regex("resting", ignore_case = TRUE)) ~ "BE",
      str_detect(activity_code, regex("other", ignore_case = TRUE)) ~ "DC")) %>%
  mutate(
    human_use_type_code = NA) %>%
  mutate(
    survey_observation_photos = NA)

image_data$yearling_males <- as.numeric(image_data$yearling_males)
image_data$yearling_females <- as.numeric(image_data$yearling_females)
image_data$yearling_UC <- as.numeric(image_data$yearling_UC)

image_data$tally <- image_data %>%
  select(adult_males:females_UC) %>%
  rowSums(na.rm = T)

image_data <- image_data %>%
  mutate(
    unclassified = if_else(species_code == "M-HOSA",
                           as.numeric(NA),
                           if_else(count == tally,
                                   unclassified,
                                   count-tally)))

## Organize and finalize ####

sequence_image_data <- image_data %>%
  select(
    study_area_name:sex_code,
    adult_males:unclassified,
    activity_code,
    animal_id:survey_observation_photos,
    surveyor:comments)

## Write to .csv ####

write.table(sequence_image_data, "processed_data/SPI/sequence_image_data.csv", na = "", row.names = FALSE, col.names = FALSE, sep = ",")

# File can be found in WD

## Copy & paste ####

# Open template

# Find sheet named "Sequence Image Data"

# Copy & paste

# 5. Add missing data ####

# Project information must be filled out manually

# If using 'Inspecting camera' behaviour, must add to 'User-defined codes' sheet

# Example for IC code:
# Column Name: Activity Code	
# Column Description:	A code indicating the activity of an animal when it was first detected or the activity that caused the sign, e.g. GR. INSTRUCTIONS: If observing a group then record the exact, sub sampled, or guesstimated mode activity of all the individuals in the group."
# Code for in Column: IC	
# Code Meaning: Inspecting Camera	
# Code Description: Animal is less than 1m from camera and is pawing at or looking closely at camera.
