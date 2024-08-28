
# PART 1: CORRECTING DATABASE OUTPUT ####

library(tidyverse)
library(lubridate)

getwd()

# Set WD if not working in an R project (you should be!)
# setwd("C:/")

# Set project name
proj_name <- "SPCS"

# Set timezone as UTC if cameras don't account for DST, otherwise set as local tz
tz <- "UTC"

## 1. Standardize camera deployments ####
# Extract database output to input/raw_data

stations <- read_csv(paste0("input/raw_data/",proj_name, "/stations.csv"))
camera_checks <- read_csv(paste0("input/raw_data/",proj_name, "/camera_checks.csv"))
images <- read_csv(paste0("input/raw_data/",proj_name, "/images.csv"))
idents <- read_csv(paste0("input/raw_data/",proj_name,"/images_idents.csv"))
# Note: idents may return a warning if you put any non-numeric data in your Tags/Marks (collar_tags) field

deployment  <- full_join(camera_checks, stations, by = "station_id")
head(deployment)

# get rid of unnecessary columns
deployment_short <- select(deployment, 
                           station_id,
                           camera_label,
                           treatment, 
                           latitude,
                           longitude, 
                           check_date,
                           stop_date,
                           media_recovered,
                           bait,
                           feature,
                           quiet_period,
                           camera_height, 
                           camera_angle,
                           camera_distance,
                           camera_status)

write.csv(deployment_short, paste0("input/raw_data/",proj_name,"_deployment_data.csv"), row.names = F)

## 2. Standardize station data ####

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


## 3. Standardize identifications from database output ####
# Correct date info

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

#06 - 2 hours off
#09 - 1 year behind until June2022
#13 - 12 hours behind
#21 - jumped from 5-22 to 6-09
#48 - 1 hr fast
#54 - 21 days behind
#55 - 21 days behind
#21 - got all sorts of weird, throw last check all away 

img_dates_new <- img_dates %>%
  mutate(date_time = if_else(
    station_id == "SPCS06" & 
      date_time < as.POSIXct("2022-05-31", tz=tz),
    date_time + hours(2),
    if_else(
      station_id == "SPCS09" & 
        date_time < as.POSIXct("2022-05-31", tz=tz),
      date_time + years(1),
      if_else(
        station_id == "SPCS21" &
          date_time > as.POSIXct("2022-05-21", tz = tz) &
          date_time < as.POSIXct("2022-06-02", tz = tz),
        date_time - days(18),
        if_else(
          station_id == "SPCS48" & 
            date_time < as.POSIXct("2022-06-01", tz=tz),
          date_time - hours(1),
          if_else(
            station_id == "SPCS54" & 
              date_time < as.POSIXct("2022-06-01", tz=tz),
            date_time + days(21),
            if_else(
              station_id == "SPCS55" & 
                date_time < as.POSIXct("2022-05-31", tz=tz),
              date_time + days(21),
              if_else(station_id ==
                        "SPCS13", date_time + hours(12), 
                      if_else(station_id=="SPCS21" &
                              date_time > as.POSIXct("2022-06-02", tz = tz),
                              NA,
                              date_time)
              )
            )
          )
        )
      )
    )
  ))

# make sure it looks right
summary(img_dates_new$date_time)
img_dates_new %>%
  filter(!is.na(date_time)) %>%
  group_by(station_id) %>%
  summarize(min = min(date_time),
            max = max(date_time)) %>%
  view()

#bring correct timestamps into ud.dat, then remove any with Date = NA

ud.dat <- left_join(idents, img_dates_new[,c(2,30)], by="image_id") %>%
  filter(!is.na(date_time))

# rename columns as needed
# NOTE: I'm renaming snow_cover to distance because this is where I entered my distance measurements
colnames(ud.dat)

ud.dat <- ud.dat %>%
  mutate(species = latin_name, .keep="unused") %>%
  rename(age = age_category,
         count = species_count,
         distance = snow_cover)

# remove misfires, staff images, and deleted IDs

ud.dat <- ud.dat %>%
  filter(deleted == F, 
         misfire == F,
         staff == F)

# other standardizations
ud.dat$`project_id` <- proj_name 
ud.dat$time_zone <- tz

# if you used a certain value to indicate NA in IDs, specify below
# e.g. I entered "100" to indicate unknown group size & distance(snow_cover)
ud.dat <- ud.dat %>%
  mutate(group_count = na_if(group_count, 100),
         distance = na_if(distance, 100))

# if anything else is NA that should have a value (or vice versa), correct below
ud.dat <- ud.dat %>%
  # humans should not have sex/age values
  mutate(
    sex = if_else(species == "Homo sapiens", NA, if_else(
      # any other species should be marked unknown if value is NA
      is.na(sex), "Unknown", sex)),
    age = if_else(species == "Homo sapiens", NA, if_else(
      !is.na(age), age,
      if_else(
        # if sex is specified but not age, then this was a mistake and age should be 'adult'. otherwise mark age unknown
        sex != "Unknown", "Adult", "Unknown")
    ))
  )


# pare down to desired columns only
ud.dat <- select(ud.dat,
                 project_id,
                 station_id,
                 image_id,
                 misfire,
                 species,
                 common_names,
                 date_time,
                 time_zone,
                 temperature,
                 count,
                 group_count,
                 age,
                 sex,
                 behaviour,
                 collar,
                 collar_tags,
                 distance,
                 comments)


# Write to csv
write.csv(ud.dat, paste0("input/raw_data/",proj_name,"_detection_data.csv"), row.names = F)

rm(list=ls())

# PART 2: DATA TESTS ####
# Run through this manually to be sure your data meets the requirements of reporting/analysis scripts

# Load your data [change the files paths to your data locations]
dat <- read.csv("input/raw_data/SPCS_detection_data.csv", header=T)
che <- read.csv("input/raw_data/SPCS_deployment_data.csv", header=T)
sta <- read.csv("input/raw_data/SPCS_station_data.csv", header=T)

# Create an output folder (this may return a warning if the folder already exists)
dir.create("input/processed_data/")

# Timezone [Use UTC if your cameras do not correct for daylight saving time, if they do use the timezone where the data was collected]
tz <- "UTC"

#Load Packages
list.of.packages <- c("leaflet", "dplyr", "colortools", "kriging", "corrplot", "lubridate", "kableExtra", "tidyr", "ggplot2", "gridExtra", "activity", "overlap", "webshot", "flextable")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 1) dat$misfire and che$media_recovered must be logical (should return TRUE)
is.logical(dat$misfire)
is.logical(che$media_recovered)
# If FALSE then convert column to TRUE/FALSE
# If you don't have a misfire column and all of you data have animals in them, run the following:
# dat$misfire <- FALSE

# 2) All dates must be in YYYY-MM-DD in 'che' and YYYY-MM-DD HH:MM:SS in 'dat' 
# If the following return NA, change your formatting

as.Date(ymd(che$check_date[1]))
ymd_hms(dat$date_time[1])

# 3) the dates in 'che$stop_date' must be the when the camera fails, not when you check the camera. 
#    If the camera fails (due to damage or full sd card), enter the date that it fails under stop_date. 
#    (Tip: use timelapse data, not motion data, to determine precise stop date)
sto <- che %>%
  filter(!is.na(stop_date))
as.Date(ymd(sto$stop_date))

# 4) Ensure your species names are consistent - check in the list below
table(dat$species)

# 5) Ensure group_count and species_count doesn't have any non-numeric data in it. The following should return TRUE
is.numeric(dat$group_count)
is.numeric(dat$count)

# if satisfied, write placeholder dat file
dat_new <- dat

# 6) separate check_dates into deployment and retrieval dates and ensure all deployment dates are before retrieval dates for each deployment
# We need to make camera status a factor so that we can use it for sorting
che$camera_status <- factor(che$camera_status, levels = c("Unknown Failure", "Vandalism/Theft", "Retired", "Available for use", "Active in field"))

che_new <- che %>%
  group_by(station_id, camera_label) %>%
  arrange(as.Date(check_date), camera_status) %>%
  mutate(start = lag(check_date)) %>%
  filter(!is.na(start)) %>%
  mutate(stop = if_else(!is.na(stop_date), stop_date, check_date)) %>%
  select(-check_date, -media_recovered, -stop_date, -camera_status, -treatment, -feature) %>%
  arrange(station_id) %>%
  unique()

# Logic = stations must be active for 0 or more days -> count of TRUE should equal n of rows in che_new dataframe
table((strptime(che_new$stop, "%Y-%m-%d", tz="UTC") - 
         strptime(che_new$start, "%Y-%m-%d", tz="UTC"))>=0)
nrow(che_new)
# if numbers don't match, check deployment, retrieval, and failure dates again
che_new$dep_length <- as.numeric(difftime(strptime(che_new$stop, "%Y-%m-%d", tz="UTC"),
  strptime(che_new$start, "%Y-%m-%d", tz="UTC"), units = "days"))

# 7) Do you have lat/long data for all of your sites you have che data for? If yes, both should return FALSE
anyNA(sta$latitude)
anyNA(sta$longitude)
# If TRUE, check sta to see where lat/long data is missing!

# 8) Do you have periods where cameras were covered by snow? If yes, enter in csv and load below
snow <- read_csv("input/raw_data/snow.csv")
# make sure names match
summary(sort(unique(snow$station_id)) == sort(unique(che$station_id)))

# extract dates of covered periods
snow_new <- snow %>%
  mutate(
    covered_periods = if_else(is.na(covered_start), 0, 
                              str_count(covered_start, ",") + 1),
    days = if_else(covered_periods==1, as.numeric(difftime(covered_end+days(1), covered_start, units = "days")), NA))



# set dates of first and last day camera was active FOR EACH PERIOD OF SNOW COVERAGE
# 
# for(i in 1:max(snow_new$covered_periods))
# {
#   for(j in 1:nrow(snow_new))
#   {
#     if(snow_new$covered_periods[j] < as.numeric(i))
#     {
#       snow_new[j, paste0("pause",i)] <- as.POSIXct(NA)
#       snow_new[j, paste0("resume",i)] <- as.POSIXct(NA)
#       
#     } else {
#       snow_new[j, paste0("pause",i)] <- ymd(as.POSIXct(str_extract(snow_new$covered_start[j], 
#                                                                    "\\d\\d\\d\\d-\\d\\d-\\d\\d")) - days(1))
#       snow_new$covered_start[j] <- str_remove(snow_new$covered_start[j], 
#                                               "\\d\\d\\d\\d-\\d\\d-\\d\\d")
#       snow_new[j, paste0("resume",i)] <- ymd(as.POSIXct(str_extract(snow_new$covered_end[j], 
#                                                                     "\\d\\d\\d\\d-\\d\\d-\\d\\d")) - days(1))
#       snow_new$covered_end[j] <- str_remove(snow_new$covered_end[j], 
#                                             "\\d\\d\\d\\d-\\d\\d-\\d\\d")
#     }
#   }
# }
# 
# # make sure it looks right, then fix name & delete snow_new
# view(snow_new)
# 
# snow <- snow_new %>%
#   select(-covered_start, -covered_end)
# snow_new <- NULL

# # join snow with che to ensure resume dates are not after stop dates, then count snow days
# 
# snow_eff <- inner_join(snow, eff_new %>% select(station_id, end_date), by="station_id") %>%
#   mutate(end_date = as.POSIXct(end_date)) %>%
#   distinct(station_id, .keep_all = T)
# view(snow_eff)
# # are any resume dates after the station's end_date? If so, correct before moving on
# 
# # count snow days
# for(i in 1:max(snow_eff$covered_periods))
# {
#   snow_eff$snow_days <- as.numeric(NA)
#   for(j in 1:nrow(snow_eff))
#   {
#     if(snow_eff$covered_periods[j] >= as.numeric(i))
#     {
#       snow_eff[j, paste0("period",i)] <- as.numeric(difftime(ymd(snow_eff[[j, paste0("resume",i)]]),
#                                                              ymd(snow_eff[[j, paste0("pause",i)]]),
#                                                              units="days"))
#     } else {
#       snow_eff[j, paste0("period",i)] <- 0
#     }
#     snow_eff$snow_days[j] <- 0
#     if(i == 1)
#     {
#       snow_eff$snow_days[j] <- snow_eff$period1[j]
#     } else {
#       snow_eff$snow_days[j] <- sum(snow_eff$snow_days[j], snow_eff[[j, paste0("period",i)]])
#     }
#     snow_eff[paste0("period",i)] <- NULL
#   }
# }
# 
# # check that it looks good
# view(snow_eff)

snow_eff <- snow_new %>%
  group_by(station_id) %>%
  summarize(snow_days = sum(days, na.rm = T))

# If you are satisfied with all of the above, continue to part 3


# PART 3: DATA TIDYING ####

# Create a variable in dat that uses species names as you would use them in the report
# bring in dat
dat <- dat_new

unique(dat$species)

dat$report_names <- if_else(
  dat$species == "Ursus americanus",
  "Black bears",
  if_else(
    dat$species == "Bird spp.",
    "Bird species",
    if_else(
      dat$species == "Canis familiaris",
      "Dogs",
      if_else(
        dat$species == "Canis latrans",
        "Coyotes",
        if_else(
          dat$species == "Canis lupus",
          "Wolves",
          if_else(
            dat$species == "Cervus canadensis",
            "Elk",
            if_else(
              dat$species == "Lynx rufus",
              "Bobcats",
              if_else(
                dat$species == "Odocoileus hemionus",
                "Deer",
                if_else(
                  dat$species == "Homo sapiens",
                  "Humans",
                  if_else(
                    dat$species == "Procyon lotor",
                    "Raccoons",
                    if_else(
                      dat$species == "Lepus americanus",
                      "Hares",
                      if_else(
                        dat$species == "Felis catus",
                        "Cats",
                        if_else(
                          dat$species == "Tamiasciurus douglasii",
                          "Squirrels",
                          if_else(
                            dat$species == "Mus spp.",
                            "Mice",
                            if_else(dat$species == "Ursus arctos",
                                    "Grizzly bears",
                                    NA)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Prepare eff dates
eff <- che_new %>%
  group_by(station_id) %>%
  summarize(days_active = sum(dep_length)) %>%
  filter(days_active>0)

# Join snow days data for calculating days of activity
eff <- full_join(eff, snow_eff, by="station_id")
# calculate number of days each station was active
eff$eff <- eff$days_active - eff$snow_days

# Prepare dat date & time
ymd_hms(dat$date_time[1],truncated=2)

dat$date_time <- ymd_hms(dat$date_time, truncated=2, tz=tz)

## Seasons ####
# IF YOU WANT TO SPLIT YOUR DATA BY SEASONS: edit season names, start and end dates below
# Keep in mind that start date will not be included in interval, but end date will.
# seasons <- tibble(season = c("summer", "winter"),
#                   start_date = c("2021-06-14", "2022-01-01"),
#                   end_date = c("2021-09-01", "2022-03-31"))
# seasons <- seasons %>%
#   mutate(start_date = as.POSIXct(start_date),
#          end_date = as.POSIXct(end_date),
#          int = interval(start_date, end_date),
#          days = round(int_length(int)/86400))
# 
# for(j in 1:nrow(dat))
# {
#   dat$season[j] <- as.character("other")
#   for (i in 1:nrow(seasons))
#   {
#     if (dat$date_time[j] %within% seasons$int[i])
#     {
#       dat$season[j] <- seasons$season[i]
#     }
#   }
# }
# 
# dat$season <- as.factor(dat$season)
# # check values are as expected
# summary(dat$season)


# PART 4: INDEPENDENCE ####
# Use this piece if you are analyzing using independent detections rather than raw

# Set the "independence" interval in minutes
independent <- 30

# Remove observations without animals detected
dat <- dat[dat$misfire==FALSE & is.na(dat$species)==FALSE,]
dat$species <- as.character(dat$species)
dat$station_id <- as.character(dat$station_id)

# Order the dataframe by Site, date
dat <- dat[order(dat$station_id, dat$date_time),]

dat <- dat %>%
  arrange(project_id,station_id) %>%
  group_by(station_id, species) %>%
  mutate(duration = int_length(lag(date_time) %--% date_time))

# loop that assign group ID
dat$event_id <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$event_id[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$event_id[nrow(dat)] <- dat$event_id[nrow(dat)-1]
} else{
  dat$event_id[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}

# If one image in an event has collar = T, make all images in that event collar = T.
dat <- dat %>% 
  group_by(event_id) %>%
  mutate(collar = if_else(any(collar), T, F))

# If there are duplicate IDs for one image, take the ID with fewer NAs
dat$empty <- apply(X = is.na(dat %>% select(count:comments)), MARGIN = 1, FUN = sum)
dat <- dat %>%
  group_by(image_id, species) %>%
  slice_min(empty)

# If there is no minimum groupsize take number of animals
if(!"group_count" %in% colnames(dat)) {dat$group_count <- dat$count}

# Calculate the event length and size
# order dat by event, then date_time
dat <- dat %>%
  arrange(event_id, date_time)

# find the last and the first image times in the event
diff <- dat %>%
  group_by(event_id) %>%
  summarize(first = min(date_time),
            last= max(date_time),
            event_observations= n(),
            event_groupsize= max(group_count))

# calculate the duration (add 1 second so that no events are 0 seconds)
diff <- diff %>%
  mutate(event_duration=abs(int_length(first %--% last))+1)

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

diff$first<-NULL
diff$last<-NULL
dat$duration <-NULL

# Merge the data
dat <-  dat %>%
  left_join(diff,by="event_id")

# Subset to rows with collar tags

# dat <- dat %>%
#   filter(!(collar == T & is.na(collar_tags))) %>%
#   as.data.frame()

# Subset to the observation with the fewest NAs in each event
ind.dat <- dat %>%
  group_by(event_id) %>%
  slice_min(empty)
ind.dat <- ind.dat[!duplicated(ind.dat$event_id),] %>%
  as.data.frame() %>%
  # clean it up
  mutate(report_names = as.factor(report_names)) %>%
  select(-image_id, -misfire, -empty)

# PART 5: EFFORT DATA ####

# Remove all observations with occur outside of camera activity schedules
# We need to know how many detections there are in each month -> create a row lookup
# This is just a list of ever day a camera was active.
che <- che_new

daily.lookup <- list()
for(i in 1:nrow(che)) {
  daily.lookup[[i]] <- data.frame(
    "date" = seq(as.Date(che$start[i]),
                 as.Date(che$stop[i]),
                 by = "days"),
    "station_id" = che$station_id[i],
    "camera_label" = che$camera_label[i]
  )
}
row.lookup <- do.call(rbind, daily.lookup)


# ONLY KEEP THIS SECOND PART IF YOU HAVE SNOW COVER DATES

snow_cover <- snow %>%
  filter(!is.na(covered_start))

snow.lookup <- list()
for(i in 1:nrow(snow_cover)) {
  snow.lookup[[i]] <- data.frame(
    "date" = seq(as.Date(snow_cover$covered_start[i]), 
                 as.Date(snow_cover$covered_end[i]), 
                 by = "days"),
    "station_id" = snow_cover$station_id[i]
  )
}
snow.lookup <- do.call(rbind, snow.lookup)

row.lookup <- anti_join(row.lookup, snow.lookup)

  # # ONLY KEEP THIS THIRD PART IF YOU ARE ASSIGNING SEASONS TO DATA
  # for (k in 1:nrow(daily.lookup[[i]]))
  # {
  #   daily.lookup[[i]]$season[k] <- "other"
  #   for (h in 1:nrow(seasons))
  #   {
  #     if (daily.lookup[[i]]$date[k] %within% seasons$int[h])
  #     {
  #       daily.lookup[[i]]$season[k] <- seasons$season[h]
  #     }
  #   }
  # }


# Remove duplicates
row.lookup <- row.lookup[duplicated(row.lookup)==F,]

# Make a dat/location lookup
tmp <- row.lookup
tmp <- paste(tmp$date, tmp$station_id)

#Subset to data that only occurs when a camera is active
ind.dat <- ind.dat[paste(as.Date(ind.dat$date_time), ind.dat$station_id) %in% tmp, ]
dat <- dat[paste(as.Date(dat$date_time), dat$station_id) %in% tmp, ]
# Reset the factor levels
ind.dat$report_names <- factor(ind.dat$report_names)

# Save your independent events dataframe
write.csv(ind.dat, paste0("input/processed_data/",dat$project_id[1], "_",independent ,"min_Independent.csv"), row.names = F)

# Also export the effort lookup
write.csv(row.lookup, paste0("input/processed_data/",dat$project_id[1], "_daily_effort_lookup.csv"), row.names = F)

# Save your environment for importing into reporting and analysis scripts
save(list=c("daily.lookup", "dat", "eff", "ind.dat", "row.lookup", "snow.lookup", "sta", "tz", "mins"), file = paste0(getwd(),"/input/processed_data/",dat$project_id[1],"_standard_input.RData"))
rm(list=ls())


