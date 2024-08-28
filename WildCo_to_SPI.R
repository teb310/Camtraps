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

# Run this code for as many years of data you have

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

# Set which year you are compiling data for (SPI likes to have survey observations split by calendar year)

year <- 2022

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
daily_lookup <- read_csv(paste0("input/processed_data/",proj_name,"_daily_effort_lookup.csv"))

# Let's first create an effort dataframe with each camera and station that were working this calendar year.
# We'll filter all our other data based on this

effort <- daily_lookup %>%
  filter(format(date, "%Y") == year) %>%
  group_by(camera_label, station_id) %>%
  summarize(total_time = n()-1,
            start = min(date),
            end = max(date)) %>%
  filter(total_time>0)

# 1. Camera information ####

## Prepare data ####

# Join existing tables and organize variables

camera_info <- checks %>%
  # take record with earliest check_date (= date of deployment)
  arrange(check_date) %>%
  distinct(camera_label, .keep_all = TRUE) %>%
  # join with camera data and station data
  inner_join(effort, by=c("camera_label", "station_id")) %>%
  inner_join(cameras %>% select(camera_label, camera_model, camera_comments), by="camera_label") %>%
  inner_join(stations %>% select(station_id, longitude, latitude, site_comments), by="station_id") %>%
  # add comment to indicate station ID
  mutate(site_comments = if_else(is.na(site_comments), paste("Station", station_id), (paste0(site_comments, "; Station ", station_id)))) %>%
  # alter name of model variable
  rename(model = camera_model) %>%
  # arrange to assist looking for >1 cam at 1 station
  arrange(station_id, camera_label)

# if 2 cameras have the same station_id (because one replaced the other), note in comments
for(i in 2:nrow(camera_info))
{
  if(camera_info$station_id[i] == camera_info$station_id[i-1])
  {
    z <- camera_info$camera_label[i-1]
    # if comments has data in it already, keep it and add your note after a ';'
    if(is.na(camera_info$camera_comments[i]))
    {
      camera_info$camera_comments[i] <- paste("Replaced", z, "on", camera_info$check_date[i])
    } else {
      camera_info$camera_comments[i] <- paste0(camera_info$camera_comments[i], "; Replaced ", z, " on ", camera_info$check_date[i])
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

write.table(camera_info, paste0("output/SPI/camera_information_", year ,".csv"), col.names = FALSE, row.names = FALSE, sep = ",", na = "")

# Locate file in output folder

## Copy & paste ####

# Open template at https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/wildlife-wildlife-habitat/wildlife-inventory/wildlife_camera_template.xlsm?forcedownload=true

# Find sheet named "Camera Information"

# Copy & paste into sheet



# 2. Camera setup & checks ####
## Prepare data ####

camera_setup_checks <- checks %>%
  group_by(camera_label, station_id) %>%
  slice_min(check_date) %>%
  inner_join(effort, by=c("camera_label", "station_id")) %>%
  mutate(comments = paste("Station", station_id))

# if 2 cameras have the same station_id (because one replaced the other), note in comments
# first arrange by station_id and check_date to put original cameras one record above replacement cameras
camera_setup_checks <- camera_setup_checks %>%
  arrange(station_id, start)

# now check if a record has the same station ID as the previous record, and note name of camera being replaced
for(i in 2:nrow(camera_setup_checks))
{
  if(camera_setup_checks$station_id[i] == camera_setup_checks$station_id[i-1])
  {
    z <- camera_setup_checks$camera_label[i-1]
    # if comments has data in it already, keep it and add your note after a ';'
    if(is.na(camera_setup_checks$comments[i]))
    {
      camera_setup_checks$comments[i] <- paste("Replaced", z, "on", camera_setup_checks$start[i])
    } else {
      camera_setup_checks$comments[i] <- paste0(camera_setup_checks$comments[i], "; Replaced ", z, " on ", camera_setup_checks$start[i])
    }
  }
}

## Format to SPI standards ####

# Fix date format
camera_setup_checks$date <-
  format(as.Date(camera_setup_checks$start, tz = tz, "%Y-%m-%d"), "%d %b %Y")
camera_setup_checks$end_date <-
  format(as.Date(camera_setup_checks$end, tz = tz, "%Y-%m-%d"), "%d %b %Y")

# Organize columns
camera_setup_checks <- camera_setup_checks %>%
  ungroup() %>%
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

write.table(camera_setup_checks, paste0("output/SPI/camera_setup_checks_",year,".csv"), col.names = FALSE, row.names = FALSE, sep = ",", na = "")

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
  # join to effort to match up camera_labels
  inner_join(effort, by="station_id", relationship = "many-to-many") %>%
  # filter to the start and end dates listed
  filter(as.Date(date) >= start, as.Date(date) <= end) %>%
  # we will adjust the 'count' value to reflect group count
  # If you used a certain value to mean unknown group count, convert value to 1 below
  mutate(count = if_else(count==100, 1, count),
         group_count = if_else(is.na(group_count), 1, group_count)) %>%
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
      str_detect(species, regex("Bird spp.", ignore_case = TRUE)) ~ "AVES",
      str_detect(species, regex("Canis familiaris", ignore_case = TRUE)) ~ "M-CAFA",
      str_detect(species, regex("Canis latrans", ignore_case = TRUE)) ~ "M-CALA",
      str_detect(species, regex("Canis lupus", ignore_case = TRUE)) ~ "M-CALU",
      str_detect(species, regex("Cervus canadensis", ignore_case = TRUE)) ~ "M-CEEL-RO",
      str_detect(species, regex("Felis catus", ignore_case = TRUE)) ~ "M-FECA",
      str_detect(species, regex("Homo sapiens", ignore_case = TRUE)) ~ "M-HOSA",
      str_detect(species, regex("Lepus americanus", ignore_case = TRUE)) ~ "M-LEAM",
      str_detect(species, regex("Lynx rufus", ignore_case = TRUE)) ~ "M-LYRU",
      str_detect(species, regex("Mus spp.", ignore_case = TRUE)) ~ "RODENTIA",
      str_detect(species, regex("Odocoileus hemionus", ignore_case = TRUE)) ~ "M-ODHE",
      str_detect(species, regex("Procyon lotor", ignore_case = TRUE)) ~ "M-PRLO",
      str_detect(species, regex("Tamiasciurus douglasii", ignore_case = TRUE)) ~ "M-TADO",
      str_detect(species, regex("Ursus americanus", ignore_case = TRUE)) ~ "M-URAM",
      str_detect(species, regex("Ursus arctos", ignore_case = TRUE)) ~ "M-URAR",
      str_detect(species, regex("Unknown species", ignore_case = TRUE)) ~ "NULL")) %>%
  filter(species_code != "NULL")

## Activity codes ####
image_data <- image_data %>%
  mutate(
    # human transport mode
    human_transport_mode_code = case_when(
      species_code == "M-HOSA" ~ case_when(
      str_detect(behaviour, regex("hiking", ignore_case = TRUE)) ~ "OF",
      str_detect(behaviour, regex("mountain biking", ignore_case = TRUE)) ~ "NPB",
      str_detect(behaviour, regex("horse", ignore_case = TRUE)) ~ "OH",
      str_detect(behaviour, regex("quad", ignore_case = TRUE)) ~ "ORV",
      str_detect(behaviour, regex("motorbike", ignore_case = TRUE)) ~ "ORV",
      str_detect(behaviour, regex("snowmobile", ignore_case = TRUE)) ~ "ORV",
      str_detect(behaviour, regex("truck/car", ignore_case = TRUE)) ~ "SPV",
      str_detect(behaviour, regex("unknown vehicle", ignore_case = TRUE)) ~ "DC"))) %>%
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
    age = if_else(species_code=="M-HOSA", as.character(NA), age),
    life_stage_code = case_when(
      str_detect(age, regex("Adult + Juvenile", ignore_case = TRUE)) ~ "AJ",
      str_detect(age, regex("Sub-adult", ignore_case = TRUE)) ~ "S",
      str_detect(age, regex("Adult", ignore_case = TRUE)) ~ "A",
      str_detect(age, regex("Juvenile", ignore_case = TRUE)) ~ "J",
      str_detect(age, regex("Unknown", ignore_case = TRUE)) ~ "UC")) 

## Sex codes ####
image_data <- image_data %>%
  mutate(
    # NA for humans
    sex_code = if_else(
      species_code == "M-HOSA",
      as.character(NA),
      # UC for juveniles
      if_else(
        life_stage_code == "J",
        "UC",
        # else shorten
        case_when(
          str_detect(sex, regex("Unknown", ignore_case = TRUE)) ~ "UC",
          str_detect(sex, regex("Female", ignore_case = TRUE)) ~ "F",
          str_detect(sex, regex("Male", ignore_case = TRUE)) ~ "M",
          str_detect(sex, regex("Mixed", ignore_case = TRUE)) ~ "MF"
        )
      )
    ))

### Group composition ####
# NOTE: THIS SECTION CAN BE MADE MUCH SIMPLER BY STICKING TO ONE METHOD AND USING IT ON EVERY ID WITH >1 INDIVIDUAL
# First, commute more complicated group compositions (i.e. notes in comments)
# Two styles used in our data: 
# 1. females are x, males are y, juveniles are z (e.g., 'xxxyyz')
# 2. list form (e.g. 3 females, 1 male, 2 juveniles)
# 3. Using F, M, and J rather than full words
image_data <- image_data %>%
  mutate(
    # remove any punctuation so that stringr can find consecutive x's or y's or z's (e.g. if you separated letters by commas)
    comments = str_remove_all(comments, "[:punct:]"),
    # change any written out numbers to digits (adjust if any additional numbers are written out)
    comments = str_replace(comments, "one", "1"),
    # extract number based on style used, then convert NAs to zero so that we can add them up
    adult_females = if_else(
      str_detect(comments, "(?<=[xyzu])[xyzu]|[xyzu](?=[xyzu])"),
      # if the xyzu method was used, search for x's
      as.numeric(str_count(comments, "x")),
      # else look for the word "females" preceded by digits
      if_else(
        str_detect(comments, "\\d{1,2}(?= female)"),
        as.numeric(str_extract(comments, "\\d{1,2}(?= fem)")),
        # else look for a digit before F
        as.numeric(str_extract(comments, "\\d{1,2}(?= F)"))
      )
    ), 
    # if none of those produced numbers, then make it 0
    adult_females = replace_na(adult_females, 0),
    # repeat for males
    adult_males = if_else(
      str_detect(comments, "(?<=[xyzu])[xyzu]|[xyzu](?=[xyzu])"),
      as.numeric(str_count(comments, "y")),
      if_else(
        str_detect(comments, "\\d{1,2}(?= male)"),
        as.numeric(str_extract(comments, "\\d{1,2}(?= male)")),
        as.numeric(str_extract(comments, "\\d{1,2}(?= M)"))
      )
    ), 
    adult_males = replace_na(adult_males, 0),
    # repeat for juvs
    juv_uc = if_else(
      str_detect(comments, "(?<=[xyzu])[xyzu]|[xyzu](?=[xyzu])"),
      as.numeric(str_count(comments, "z")),
      if_else(
        str_detect(comments, "\\d{1,2}(?= juv)"),
        as.numeric(str_extract(comments, "\\d{1,2}(?= juv)")),
        as.numeric(str_extract(comments, "\\d{1,2}(?= J)"))
      )
    ), 
    juv_uc = replace_na(juv_uc, 0),
    # repeat for UC
    adult_uc = if_else(
      str_detect(comments, "(?<=[xyzu])[xyzu]|[xyzu](?=[xyzu])"),
      as.numeric(str_count(comments, "u")),
      if_else(
        str_detect(comments, "\\d{1,2}(?= unk)"),
        as.numeric(str_extract(comments, "\\d{1,2}(?= unk)")),
        as.numeric(str_extract(comments, "\\d{1,2}(?= U)"))
      )
    ), 
    adult_uc = replace_na(adult_uc, 0)) %>%
      
  # create a variable summing your class counts so far
  mutate(class_sum = adult_females+adult_males+juv_uc+adult_uc) %>%
    
  # Next, we'll assume simple group compositions if comments don't specify
  # if only one age/sex class is ID'd, use count. Otherwise, assume 1 individual of each age/sex class in ID is present
  mutate(
    adult_females = if_else(class_sum>0, 
                            adult_females,
                            if_else(life_stage_code=="A" & sex_code =="F", 
                                    count,
                                    if_else(str_detect(sex_code, "F") | life_stage_code=="AJ" & sex_code=="MF",
                                            1,
                                            0))),
    adult_males = if_else(class_sum>0, 
                          adult_males,
                          if_else(life_stage_code=="A" & sex_code =="M", 
                                  count,
                                  if_else(str_detect(sex_code, "M") | life_stage_code=="AJ" & sex_code=="MF",
                                          1,
                                          0))),
    juv_uc = if_else(class_sum>0, 
                          juv_uc,
                          if_else(life_stage_code=="J", 
                                  count,
                                  if_else(life_stage_code=="AJ",
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
  mutate(class_sum = adult_females+adult_males+juv_uc+adult_uc+uc_uc) %>%
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

#replace any NAs in human obs
image_data$count <- replace_na(image_data$count, 1)

# check to make sure everything went as planned
view(image_data)

# Save for future use:
write.csv(image_data, paste0("input/processed_data/",proj_name, "_SPI_image_data_full_",year,".csv"))

## Format to SPI standards ####

# remove zeros from adult_females:uc_uc
sequence_image_data <- image_data %>% 
  mutate(across(adult_females:uc_uc, ~na_if(.,0)))

sequence_image_data <- sequence_image_data %>%
  mutate(
    # SPI doesn't accept mixed age or sex codes
    sex_code = ifelse(sex_code == "MF" | life_stage_code == "AJ", NA, sex_code),
    life_stage_code = na_if(life_stage_code, "AJ"),
    juv_male = NA,
    juv_female = NA,
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
sequence_image_data <- sequence_image_data %>%
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
    juv_male,
    juv_female,
    juv_uc,
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

## Write to .csv ####

write.table(sequence_image_data, paste0("output/SPI/sequence_image_data_",year,".csv"), na = "", row.names = FALSE, col.names = FALSE, sep = ",")

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
