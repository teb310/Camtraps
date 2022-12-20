# Formatting from WildCo to SPI

# READ FIRST
# Before running this code, you MUST RUN THE STANDARDIZATION SCRIPT in this repository to produce the 'events' and 'stations' dataframes
# This code relies on the following assumptions of data entry, and may require changes if these assumptions aren't met
# 1. Animal data relates to events (separated by an 'independence interval'), not images. This code will create a record for each event, rather than each image
# 2. Juveniles are always assumed to be unknown sex; any male/female IDs apply to adults only
# 3. Animals were ID'd as adults or juveniles; sub-adult & yearling was not used
# 4. Group composition (# of adults vs juveniles, males vs. females in each group) is specified in comments using one of 2 methods (see Group Composition section)
# 5. Cameras were active for one continuous period without breaks
# Any additional assumptions should be simple fixes if violated

# Start of script
# 0. Prepare R ####

# Load packages

library(tidyverse)
library(lubridate)

# Set project name and study area name (assuming only one study area)

proj_name <- "SPCS"
study_area <- "Sechelt Peninsula"

# Set timezone (if cameras don't track daylight savings, use UTC, otherwise use local tz)

tz = "UTC"

# Set the "independence" interval in minutes

independent <- 30

# Input any months that you conducted camera checks (this will need to match up with check_date on your camera_checks file)

checks <- c("2021-09","2022-05","2022-06")

# Set x  and y to choose how R should measure the time your cameras were deployed: days (D), hours (H), or minutes (M).
# DO NOT CHANGE D, H, and M; these are constants (number of seconds in each measurement unit)
D <- 86400
H <- 3600
M <- 60

# y should be the same as x but with quotes
x <- D
y <- "D"

# Read in files (replace with own filepaths)
# 'cameras' and 'checks' are taken directly from the WildCo output, 'events' and 'station' are produced using the Standardization script
cameras <- read_csv("input/raw_data/cameras.csv")
checks <- read_csv(paste0("input/raw_data/",proj_name,"/camera_checks.csv"))
events <- read_csv(paste0("input/processed_data/",proj_name,"_",independent,"min_Independent.csv"))
stations <- read_csv(paste0("input/raw_data/",proj_name,"_station_data.csv"))

# Create lookup for camera label vs. station ID (SPI likes to use camera label for some reason)
# 'checks' has this info
# some stations used >1 camera -> start and end dates will help us know which was used when

station_cameras <- checks %>%
  group_by(camera_label) %>%
  mutate(start_use = min(check_date),
         end_use = max(check_date)) %>%
  select(camera_label, station_id, start_use, end_use) %>%
  distinct()

# 1. Camera information ####

## Prepare data ####

# Join existing tables and organize variables

camera_info <- checks %>%
  # take record with earliest check_date (= date of deployment)
  arrange(check_date) %>%
  distinct(camera_label, .keep_all = TRUE) %>%
  # join with camera data and station data
  inner_join(cameras %>% select(camera_label, camera_model, camera_comments), by="camera_label") %>%
  inner_join(stations %>% select(station_id, longitude, latitude, site_comments), by="station_id") %>%
  # alter name of model variable
  rename(model = camera_model)

# if 2 cameras have the same station_id (because one replaced the other), note in comments
# first arrange by station_id and check_date to put original cameras one record above replacement cameras
camera_info <- camera_info %>%
  arrange(station_id, check_date)

# now check if a record has the same station ID as the previous record, and note name of camera being replaced
for(i in 2:nrow(camera_info))
{
  if(camera_info$station_id[i] == camera_info$station_id[i-1])
  {
    z <- camera_info$camera_label[i-1]
    # if comments has data in it already, keep it and add your note after a ';'
    if(is.na(camera_info$camera_comments[i]))
    {
      camera_info$camera_comments[i] <- paste0("Replaced ", z)
    } else {
      camera_info$camera_comments[i] <- paste0(camera_info$camera_comments[i], "; Replaced ", z)
    }
  }
}

## Camera make codes ####

camera_info <- camera_info %>%
  mutate(
    make = case_when(
      str_starts(model, regex("Browning", ignore_case = TRUE)) ~ "BR", 
      str_starts(model, regex("Bushnell", ignore_case = TRUE)) ~ "BU",
      str_starts(model, regex("Cabelas", ignore_case = TRUE)) ~ "CA",
      str_starts(model, regex("Covert", ignore_case = TRUE)) ~ "CO",
      str_starts(model, regex("LTL", ignore_case = TRUE)) ~ "LA",
      str_starts(model, regex("Moultrie", ignore_case = TRUE)) ~ "MO",
      str_starts(model, regex("Reconyx", ignore_case = TRUE)) ~ "RE",
      str_starts(model, regex("RidgeTec", ignore_case = TRUE)) ~ "RT",
      str_starts(model, regex("Spartan", ignore_case = TRUE)) ~ "SN",
      str_starts(model, regex("Spypoint", ignore_case = TRUE)) ~ "SP",
      str_starts(model, regex("Stealth", ignore_case = TRUE)) ~ "SC",
      str_starts(model, regex("TrailMaster", ignore_case = TRUE)) ~ "TM",
      str_starts(model, regex("Wildgame", ignore_case = TRUE)) ~ "WI")) %>%
  arrange(camera_label) 

## Format to SPI standards ####
camera_info <- camera_info %>%
  add_column(
    study_area_name = study_area, 
    UTM_zone = NA, 
    easting = NA, 
    northing = NA, 
    parent_sample_station_label = NA,
    site_description_date = NA,
  ) %>%
  select(
    study_area_name,
    camera_label, 
    parent_sample_station_label,
    UTM_zone:northing,
    longitude:latitude, 
    make,
    model, 
    camera_comments, 
    site_comments,
    site_description_date)

## Write to .csv ####

write.table(camera_info, "output/SPI/camera_information.csv", col.names = FALSE, row.names = FALSE, sep = ",", na = "")

# Locate file in output folder

## Copy & paste ####

# Open template at https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/wildlife-wildlife-habitat/wildlife-inventory/wildlife_camera_template.xlsm?forcedownload=true

# Find sheet named "Camera Information"

# Copy & paste into sheet



# 2. Camera setup & checks ####
## Prepare data ####

# Separate initial deployment dates out
deployment <- checks %>%
  arrange(check_date) %>%
  distinct(camera_label, .keep_all = TRUE) %>%
  select(camera_label, start_date = check_date)
camera_setup_checks <- checks %>%
  inner_join(deployment, by="camera_label")

# Separate out cameras with only one record (new cameras with no image data) and add comment
camera_setup_checks <- camera_setup_checks %>%
  group_by(camera_label) %>%
  count() %>%
  inner_join(camera_setup_checks, by="camera_label")

new_cams <- camera_setup_checks %>%
  filter(n == 1) %>%
  mutate(comments = "New camera, no images recovered yet",
         end_date = start_date) %>%
  select(-check_date, -camera_check_tbl_id)

# Set up start and end dates for other cameras
camera_setup_checks <- camera_setup_checks %>%  
  filter(check_date != start_date) %>%
  group_by(camera_label) %>%
  arrange(camera_label, check_date) %>%
  mutate(end_date = if_else(is.na(stop_date), max(check_date), stop_date)) %>%
  select(-check_date, -camera_check_tbl_id) %>%
  distinct() %>%
  filter(end_date == min(end_date)) %>%
  bind_rows(new_cams)

# if 2 cameras have the same station_id (because one replaced the other), note in comments
# first arrange by station_id and check_date to put original cameras one record above replacement cameras
camera_setup_checks <- camera_setup_checks %>%
  arrange(station_id, start_date)

# now check if a record has the same station ID as the previous record, and note name of camera being replaced
for(i in 2:nrow(camera_setup_checks))
{
  if(camera_setup_checks$station_id[i] == camera_setup_checks$station_id[i-1])
  {
    z <- camera_setup_checks$camera_label[i-1]
    # if comments has data in it already, keep it and add your note after a ';'
    if(is.na(camera_setup_checks$comments[i]))
    {
      camera_setup_checks$comments[i] <- paste0("Replaced ", z)
    } else {
      camera_setup_checks$comments[i] <- paste0(camera_setup_checks$comments[i], "; Replaced ", z)
    }
  }
}

# Set up total time column

camera_setup_checks <- camera_setup_checks %>%
  mutate(
    total_time = (int_length(start_date %--% end_date) / x))

## Format to SPI standards ####

# Fix date format
camera_setup_checks$date <-
  format(as.Date(camera_setup_checks$start_date, tz = tz, "%Y-%m-%d"), "%d %b %Y")
camera_setup_checks$end_date <-
  format(as.Date(camera_setup_checks$end_date, tz = tz, "%Y-%m-%d"), "%d %b %Y")

# Organize columns
camera_setup_checks <- camera_setup_checks %>%
  add_column(
    study_area_name = study_area, 
    time = NA, 
    end_time = NA, 
    unit_time = y, 
    trigger_timing = NA,
    video_length = NA,
    visit_photos = NA) %>%
  select(
    study_area_name,
    camera_label, 
    date,
    time,
    end_date,
    end_time,
    total_time,
    unit_time,
    quiet_period,
    sensitivity,
    trigger_timing,
    images_per_trigger,
    video_length,
    bait,
    comments,
    visit_photos) %>%
  arrange(camera_label) %>%
  mutate(bait = na_if(bait, "None"))

## Write to .csv ####

write.table(camera_setup_checks, "output/SPI/camera_setup_checks.csv", col.names = FALSE, row.names = FALSE, sep = ",", na = "")

# File can be found in WD

## Copy & paste ####

# Open template

# Find sheet named "Camera Setup and Checks"

# Copy & paste



# 3. Sequence image data ####

## Prepare data ####

image_data <- events %>%
  # add missing columns that we have data for
  add_column(study_area_name = study_area, .before = "station_id") %>%
  add_column(sequence_definition = independent) %>%
  # SPI wants date and time in separate columns
  separate(date_time, c("date", "time"), sep = " ") %>%
  # join to station_id/camera_label lookup
  inner_join(station_cameras, by="station_id") %>%
  # we will adjust the 'count' value to reflect group count
  # If you used a certain value to mean unknown group count, convert value to 0 below
  mutate(count = if_else(count==100, 0, count)) %>%
  # take the higher of group count and count fields to be official count
  mutate(count = if_else(count > group_count, count, group_count))


# Fix date format
image_data$date <- format(parse_date(image_data$date), "%d %b %Y")

## Species code ####
# ADD ALL SPECIES ON YOUR PROJECT BELOW - first check what species you have
sort(unique(image_data$species))
# Find species codes at http://a100.gov.bc.ca/pub/eswp/ and enter below
image_data <- image_data %>%
  mutate(
    species_code = case_when(
      str_detect(species, regex("Bird spp.", ignore_case = TRUE)) ~ "B",
      str_detect(species, regex("Canis familiaris", ignore_case = TRUE)) ~ "M-CAFA",
      str_detect(species, regex("Canis latrans", ignore_case = TRUE)) ~ "M-CALA",
      str_detect(species, regex("Canis lupus", ignore_case = TRUE)) ~ "M-CALU",
      str_detect(species, regex("Cervus canadensis", ignore_case = TRUE)) ~ "M-CEEL-RO",
      str_detect(species, regex("Felis catus", ignore_case = TRUE)) ~ "M-FECA",
      str_detect(species, regex("Homo sapiens", ignore_case = TRUE)) ~ "M-HOSA",
      str_detect(species, regex("Lepus americanus", ignore_case = TRUE)) ~ "M-LEAM",
      str_detect(species, regex("Lynx rufus", ignore_case = TRUE)) ~ "M-LYRU",
      str_detect(species, regex("Mus spp.", ignore_case = TRUE)) ~ "NULL",
      str_detect(species, regex("Odocoileus hemionus", ignore_case = TRUE)) ~ "M-ODHE",
      str_detect(species, regex("Procyon lotor", ignore_case = TRUE)) ~ "M-PRLO",
      str_detect(species, regex("Tamiasciurus douglasii", ignore_case = TRUE)) ~ "M-TADO",
      str_detect(species, regex("Ursus americanus", ignore_case = TRUE)) ~ "M-URAM",
      str_detect(species, regex("Unknown species", ignore_case = TRUE)) ~ "NULL"))

## Activity codes ####
image_data <- image_data %>%
  mutate(
    human_transport_mode_code = case_when(
      str_detect(behaviour, regex("hiking", ignore_case = TRUE)) ~ "OF",
      str_detect(behaviour, regex("mountain biking", ignore_case = TRUE)) ~ "NPB",
      str_detect(behaviour, regex("horse", ignore_case = TRUE)) ~ "OH",
      str_detect(behaviour, regex("quad", ignore_case = TRUE)) ~ "ORV",
      str_detect(behaviour, regex("motorbike", ignore_case = TRUE)) ~ "ORV",
      str_detect(behaviour, regex("snowmobile", ignore_case = TRUE)) ~ "ORV",
      str_detect(behaviour, regex("truck/car", ignore_case = TRUE)) ~ "SPV",
      str_detect(behaviour, regex("unknown vehicle", ignore_case = TRUE)) ~ "DC")) %>%
  mutate(
    activity_code = if_else(species_code == "M-HOSA", as.character(NA),
                            case_when(
                              str_detect(behaviour, regex("traveling", ignore_case = TRUE)) ~ "TU",
                              str_detect(behaviour, regex("foraging", ignore_case = TRUE)) ~ "FD",
                              str_detect(behaviour, regex("inspecting camera", ignore_case = TRUE)) ~ "IC",
                              str_detect(behaviour, regex("hunting", ignore_case = TRUE)) ~ "HU",
                              str_detect(behaviour, regex("grooming", ignore_case = TRUE)) ~ "GR",
                              str_detect(behaviour, regex("resting", ignore_case = TRUE)) ~ "BE",
                              str_detect(behaviour, regex("other", ignore_case = TRUE)) ~ "DC")))

## Age codes ####
image_data <- image_data %>%
  mutate(
    # only get age codes for non-human IDs
    age = if_else(species_code=="M-HOSA", as.character(NA),
                  # mixed age IDs are easier to deal with if we shorten to 'AJ'
                  if_else(age=="Adult + Juvenile", "AJ", age)),
    life_stage_code = case_when(
      age=="AJ" ~ "AJ",
      str_detect(age, regex("Sub-adult", ignore_case = TRUE)) ~ "S",
      str_detect(age, regex("Adult", ignore_case = TRUE)) ~ "A",
      str_detect(age, regex("Juvenile", ignore_case = TRUE)) ~ "J",
      str_detect(age, regex("Unknown", ignore_case = TRUE)) ~ "UC")) 

## Sex codes ####
image_data <- image_data %>%
  mutate(
    sex_code = if_else(species_code=="M-HOSA", as.character(NA),
                       case_when(
                         str_detect(sex, regex("Unknown", ignore_case = TRUE)) ~ "UC",
                         str_detect(sex, regex("Female", ignore_case = TRUE)) ~ "F",
                         str_detect(sex, regex("Male", ignore_case = TRUE)) ~ "M",
                         str_detect(sex, regex("Mixed", ignore_case = TRUE)) ~ "MF")))

### Group composition ####
# NOTE: THIS SECTION CAN BE MADE MUCH SIMPLER BY STICKING TO ONE METHOD AND USING IT ON EVERY ID WITH >1 INDIVIDUAL
# First, commute more complicated group compositions (i.e. notes in comments)
# Two styles used in our data: 
# 1. females are x, males are y, juveniles are z (e.g., 'xxxyyz')
# 2. list form (e.g. 3 females, 1 male, 2 juveniles)
image_data <- image_data %>%
  mutate(
    # remove any punctuation so that stringr can find consecutive x's or y's or z's (e.g. if you separated letters by commas)
    comments = str_remove_all(comments, "[:punct:]"),
    # change any written out numbers to digits (adjust if any additional numbers are written out)
    comments = str_replace(comments, "one", "1"),
    # extract number based on style used, then convert NAs to zero so that we can add them up
    adult_females = if_else(str_detect(comments, "x{2,}|y{2,}|z{2,}"),
                            as.numeric(str_count(comments, "x")),
                            as.numeric(str_extract(comments, "\\d{1,2}(?= fem)"))),
    adult_females = replace_na(adult_females, 0),
    adult_males = if_else(str_detect(comments, "x{2,}|y{2,}|z{2,}"),
                          as.numeric(str_count(comments, "y")),
                          as.numeric(str_extract(comments, "\\d{1,2}(?= mal)"))),
    adult_males = replace_na(adult_males, 0),
    juvenile_uc = if_else(str_detect(comments, "x{2,}|y{2,}|z{2,}"),
                          as.numeric(str_count(comments, "z")),
                          as.numeric(str_extract(comments, "\\d{1,2}(?= juv)"))),
    juvenile_uc = replace_na(juvenile_uc, 0),
    adult_uc = as.numeric(str_extract(comments, "\\d{1,2}(?= unk)")),
    adult_uc = replace_na(adult_uc, 0)) %>%
  # create a variable summing your class counts so far
  mutate(class_sum = adult_females+adult_males+juvenile_uc+adult_uc) %>%
  
  # Next, we'll assume simple group compositions if comments don't specify
  # if only one age/sex class is ID'd, use count. Otherwise, assume 1 individual of each age/sex class in ID is present
  mutate(
    adult_females = if_else(class_sum>0, 
                            adult_females,
                            if_else(life_stage_code=="A" & sex_code =="F", 
                                    count,
                                    if_else(count==2 & str_detect(sex_code, "F") | count==3 & life_stage_code=="AJ" & sex_code=="MF",
                                            1,
                                            0))),
    adult_males = if_else(class_sum>0, 
                          adult_males,
                          if_else(life_stage_code=="A" & sex_code =="M", 
                                  count,
                                  if_else(count==2 & str_detect(sex_code, "M") | count==3 & life_stage_code=="AJ" & sex_code=="MF",
                                          1,
                                          0))),
    juvenile_uc = if_else(class_sum>0, 
                          juvenile_uc,
                          if_else(life_stage_code=="J", 
                                  count,
                                  if_else(count==2 & life_stage_code=="AJ" | count==3 & life_stage_code=="AJ" & sex_code=="MF",
                                          1,
                                          0))),
    adult_uc = if_else(class_sum>0,
                       adult_uc,
                       if_else(life_stage_code=="A" & sex_code=="UC",
                               count,
                               0)),
    # make column for unclassified age & sex
    uc_uc = if_else(class_sum>0,
                    0,
                    if_else(life_stage_code=="UC"& sex_code=="UC",
                            count, 0)))%>%
  # update class sums
  mutate(class_sum = adult_females+adult_males+juvenile_uc+adult_uc+uc_uc) %>%
  # update uc_uc to account for unidentified individuals
  mutate(uc_uc = if_else(class_sum < count, uc_uc+(count-class_sum), uc_uc),
         class_sum = NULL)

# check class sums once more
image_data$class_sum <- image_data %>%
  select(adult_females:uc_uc) %>%
  rowSums()
image_data %>% filter(class_sum != count) %>% relocate(count, .after=class_sum) %>% view()
# if any class sums are higher than count -> adjust count to match (i.e. assume class sum is more accurate)
image_data <- image_data %>%
  mutate(count = if_else(class_sum > count, class_sum, count))

# check to make sure everything went as planned
view(image_data)
image_data$check <- image_data %>%
  select(adult_females:uc_uc) %>%
  rowSums(na.rm = F)
image_data %>% filter(check != count) %>% relocate(count, .after=check) %>% view()

# Save for future use:
write.csv(image_data, paste0("input/processed_data/",proj_name, "_SPI_image_data_full.csv"))

## Format to SPI standards ####

# remove zeros from adult_females:uc_uc
image_data <- image_data %>% 
  mutate(across(adult_females:uc_uc, na_if, 0))

image_data <- image_data %>%
  mutate(
    # SPI doesn't accept mixed age or sex codes
    sex_code = ifelse(sex_code == "MF" | life_stage_code == "AJ", NA, sex_code),
    life_stage_code = na_if(life_stage_code, "AJ"),
    juvenile_male = NA,
    juvenile_female = NA,
    yearling_males = NA,
    yearling_females = NA,
    yearling_uc = NA,
    uc_males = NA,
    uc_females = NA,
    animal_id = NA,
    human_use_type_code = NA,
    survey_observation_photos = NA,
    surveyor = NA,
    comments = NA)

# Organize columns
image_data <- image_data %>%
  select(
    study_area_name,
    camera_label,
    date,
    time,
    temperature,
    sequence_definition,
    species_code,
    count,
    life_stage_code,
    sex_code,
    adult_males,
    adult_females,
    adult_uc,
    juvenile_male,
    juvenile_female,
    juvenile_uc,
    yearling_males,
    yearling_females,
    yearling_uc,
    uc_males,
    uc_females,
    uc_uc,
    activity_code,
    animal_id,
    human_transport_mode_code,
    human_use_type_code,
    survey_observation_photos,
    surveyor,
    comments) %>%
  arrange(camera_label, date, time)

sequence_image_data <- image_data

## Write to .csv ####

write.table(sequence_image_data, "output/SPI/sequence_image_data.csv", na = "", row.names = FALSE, col.names = FALSE, sep = ",")

# File can be found in WD

## Copy & paste ####

# Open template

# Find sheet named "Sequence Image Data"

# Copy & paste

# 4. Add missing data ####
# The rest of the spreadsheet will be filled out manually

# 4a. Fill in 'Project information' sheet

# 4b. If you've used the 'Inspecting camera' behaviour, you must add it to the 'User-defined codes' sheet (see example below)

# Example for IC code:
# Column Name: Activity Code	
# Column Description:	A code indicating the activity of an animal when it was first detected or the activity that caused the sign, e.g. GR. INSTRUCTIONS: If observing a group then record the exact, sub sampled, or guesstimated mode activity of all the individuals in the group."
# Code for in Column: IC	
# Code Meaning: Inspecting Camera	
# Code Description: Animal is less than 1m from camera and is pawing at or looking closely at camera.

# End of script
