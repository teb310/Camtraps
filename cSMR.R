# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#####################################################################################
# 04_SMR_simulations.R
# script to simulate SMR models to inform camera survey study design
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 07-Apr-2021
# nimble code provided by Paul van Dam-Bates - 16-Apr-2021
#####################################################################################

#####################################################################################
# if working in the elk_sightability project, need to point to correct library
version$major
version$minor
R_version <- paste0("R-",version$major,".",version$minor)

.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive

tz = Sys.timezone() # specify timezone in BC

# LOAD PACKAGES ####
list.of.packages <- c("tidyverse","lubridate", "readxl","timetk", "sf", "rgdal", "Cairo", "rjags","coda","doParallel",  
                      "xtable", "R2jags","data.table","MCMCvis","PNWColors", "AHMbook", "raster","units")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

###############################################################################
# Load study area #
aoi_quad <- st_read(dsn="input", layer="Sechelt_quadrants")

aoi <- aoi_quad %>% st_transform(3005) %>% st_buffer(500) %>% st_union() %>% st_sf()

# create grid cells 600 x 600 m to emulate distance between transect lines (can see 300 m on either side)
aoi_grid <- st_make_grid(st_bbox(aoi), cellsize=1000, square=TRUE) #  grid for entire AOI (rectangle)
# rm(aoi_grid)
aSMR_grid = st_sf(geom = aoi_grid)
aSMR_grid$Areakm2 <- st_area(aSMR_grid)*1e-6
aSMR_grid <- drop_units(aSMR_grid)
aSMR_grid$Id <- as.numeric(rownames(aSMR_grid))
nrow(aSMR_grid)

# reduce to the buffered BC border for computation ease
aoi_aSMR_grid <- st_intersection(aSMR_grid, aoi)
ggplot()+
  geom_sf(data=aoi_aSMR_grid)

# LOAD AERIAL DATA ####
aerialDIR <- "//Sfp.idir.bcgov/s140/S40073/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/4. Technical/Data/Aerial_SMR"
list.files(aerialDIR)

# transects
aerialTransect1 <- st_read(paste(getwd(),"data/SMR_flight_data/Transects_300m_Flight1_final.kml", sep="/")) %>% st_transform(26910)
aerialTransect1$Transect <- "aSMR1"

aerialTransect2 <- st_read(paste(getwd(),"data/SMR_flight_data/Transects_300m_Flight2_final.kml", sep="/")) %>% st_transform(26910)
aerialTransect2$Transect <- "aSMR2"

aerialTransect3 <- st_read(paste(getwd(),"data/SMR_flight_data/Transects_300m_Flight3_final.kml", sep="/")) %>% st_transform(26910)
aerialTransect3$Transect <- "aSMR3"

aerialTransect <- rbind(aerialTransect1, aerialTransect2, aerialTransect3)
aerialTransect$length <- st_length(aerialTransect)

Cairo(file="out/aSMR_TransectsFlown.PNG", 
      type="png",
      width=5000, 
      height=1600, 
      pointsize=12,
      bg="white",
      dpi=300)
ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aerialTransect)+
  facet_wrap(~Transect)
dev.off()

# determine grid cell of each transect for 'operability'
glimpse(aoi_aSMR_grid)
nrow(aoi_aSMR_grid)

ggplot()+
  geom_sf(data=aoi_aSMR_grid, aes(fill=Id))

grid_transect <- st_join(aoi_aSMR_grid %>% st_transform(3005), aerialTransect %>% st_transform(3005)) %>%
  dplyr::select(Id, Transect) %>% st_drop_geometry() %>% as_tibble()
glimpse(grid_transect)
grid_transect$aSMR_grid_ID <- floor(grid_transect$Id)

grid_transect_aSMR1 <- grid_transect %>% filter(Transect=="aSMR1") %>% dplyr::select(aSMR_grid_ID)
grid_transect_aSMR1 <- as.vector(unique(grid_transect_aSMR1$aSMR_grid_ID))

grid_transect_aSMR2 <- grid_transect %>% filter(Transect=="aSMR2") %>% dplyr::select(aSMR_grid_ID)
grid_transect_aSMR2 <- as.vector(unique(grid_transect_aSMR2$aSMR_grid_ID))

grid_transect_aSMR3 <- grid_transect %>% filter(Transect=="aSMR3") %>% dplyr::select(aSMR_grid_ID)
grid_transect_aSMR3 <- as.vector(unique(grid_transect_aSMR3$aSMR_grid_ID))

aSMR_grid$aSMR1 <- aSMR_grid$aSMR2 <- aSMR_grid$aSMR3 <- NA
aSMR_grid$aSMR1 <- case_when(aSMR_grid$Id %in% grid_transect_aSMR1 ~ 1, 
                                      TRUE ~ 0)
aSMR_grid$aSMR2 <- case_when(aSMR_grid$Id %in% grid_transect_aSMR2 ~ 1, 
                                      TRUE ~ 0)
aSMR_grid$aSMR3 <- case_when(aSMR_grid$Id %in% grid_transect_aSMR3 ~ 1, 
                                      TRUE ~ 0)

aSMR_oper_sf <- aSMR_grid %>% pivot_longer(cols=starts_with("aSMR"), values_to = "operability") %>% dplyr::select(-Areakm2)
aSMR_oper_sf <- st_intersection(aSMR_oper_sf, aoi)

###--- creating grid (study area) meta data
#  UTM Zone 10N, NAD83 (EPSG:26910)
coord.scale=100
buffer=10
grid_coords <- st_centroid(aSMR_oper_sf %>% filter(name=="aSMR1"))
grid_coords <- arrange(grid_coords, Id)
grid_coords <- st_coordinates(grid_coords %>% st_transform(26910))

gridXY <- aSMR_oper_sf %>% filter(name=="aSMR1") %>% dplyr::select(Id) %>% st_drop_geometry()
gridXY <- cbind(gridXY, grid_coords)
colnames (gridXY) <- c("grid.id","x","y")

gridlocs <- as.matrix(gridXY[,c("x","y")])
X <- gridlocs/coord.scale

###--- create xlims and ylims of scaled coordinates, and area
Xl <- min(X[,1] - buffer)
X.scaled <- X[,1] - Xl

Yl <- min(X[,2] - buffer)
Y.scaled <- X[,2] - Yl

xlims.scaled <- c(min(X.scaled)-buffer,max(X.scaled)+buffer); ylims.scaled <- c(min(Y.scaled)-buffer,max(Y.scaled)+buffer)

areaha.scaled <- xlims.scaled[2]*ylims.scaled[2] # 86804.58 ha or 868 km2

X2 <- as.matrix(cbind(X.scaled,Y.scaled))

# need to figure out the plotting, or add the rows (longer) and facet wrap
pal=pnw_palette("Winter",2, type = "discrete")

Cairo(file="out/aSMR_Operability.PNG", 
      type="png",
      width=5000, 
      height=1600, 
      pointsize=12,
      bg="white",
      dpi=300)
ggplot()+
  geom_sf(data = aSMR_oper_sf, aes(fill=as.factor(operability)))+
  scale_fill_manual(values=unique(pal))+
  facet_wrap(~name)+
  theme(legend.title=element_blank())
dev.off()

## create operability matrix for SMR analysis
aSMR_oper <- pivot_wider(aSMR_oper_sf, names_from = "name", values_from = "operability") %>% st_drop_geometry()
aSMR_oper <- arrange(aSMR_oper, Id)
aSMR_oper <- as.matrix(aSMR_oper[c("Id","aSMR1","aSMR2","aSMR3")])
# rownames(aSMR_oper) <- aSMR_oper[,1]
aSMR_oper <- aSMR_oper[,2:4]
write.csv(aSMR_oper, "data/aSMR_oper.csv")

###########
# telem data to cover aSMR survey period (2 March to 28 March 2022)
telem <- read.csv("data/Collars_Sechelt.csv", stringsAsFactors = TRUE) %>% as_tibble() %>%
  dplyr::select(Collar_ID, SCTS__UTC_, Latitude_d, Longitude_ ,Easting, Northing) %>%
  rename(Date.Time.UTC = SCTS__UTC_, Latitude = Latitude_d, Longitude=Longitude_)

###--- telem locations
telem$Date.Time.UTC <- ymd_hms(telem$Date.Time.UTC, tz = "UTC")
telem$Date.Time.PST <- with_tz(telem$Date.Time.UTC, tz)
telem <- telem %>% mutate(Year = year(Date.Time.PST), Month = lubridate::month(Date.Time.PST, label = T), jDay = yday(Date.Time.PST))

telem %>% group_by(Collar_ID) %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
telem %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
# telem location data for Sechelt Peninsula from 2021-06-13 19:01:36 to 2022-06-01 10:19:55 

telem_sf <- st_as_sf(telem %>% filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs=4326)
telem_sf %>% count(Collar_ID) %>% st_drop_geometry()
telem_sf$Collar_ID <- as.factor(telem_sf$Collar_ID)

telem_sf %>% summarise(min(Date.Time.PST), max(Date.Time.PST)) %>% st_drop_geometry()

# jDay = 61 for March 2, jDay = 74 for March 15, jDay = 87 for March 28
telem_aSMR <- telem_sf %>% filter(Year==2022, Month=="Mar")
telem_aSMR <- telem_aSMR %>% filter(jDay %in% c(60,61,62, 73,74,75, 86,87,88)) # using date of survey plus a day on either side
length(unique(telem_aSMR$Collar_ID)) # 13 active collars 
telem_aSMR %>% count(Collar_ID) %>% st_drop_geometry()

ggplot()+
  geom_sf(data=aoi %>% st_transform(3005))+
  geom_sf(data=telem_aSMR %>% st_transform(3005), aes(fill=Collar_ID, col=Collar_ID))+
  ggtitle("Active Elk Collars During Aerial Surveys")

active.collars <- sort(unique(telem_aSMR$Collar_ID))
active.collars <- droplevels(active.collars)

num.reps.collar <- telem_aSMR %>% count(Collar_ID) %>% dplyr::select(n) %>% st_drop_geometry()
num.reps.collar <- as.numeric(num.reps.collar$n)
# summary(num.reps.collar) between 1 and 17 fixes per animal, median = 15, mean = 12
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   10.00   15.00   12.23   16.00   17.00
telem_aSMR %>% arrange(Collar_ID)
locs <- as.data.frame(st_coordinates(telem_aSMR %>% arrange(Collar_ID) %>% st_transform(crs=26910))) # convert to NAD 83 UTM Zone 10 for consistency with trapping grid (m)
colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
nlocs <- nrow(locs)

coord.scale=100
locs.scaled <- locs/coord.scale
locs.scaled$Xcoord.scaled <- locs.scaled[,1] - Xl
locs.scaled$Ycoord.scaled <- locs.scaled[,2] - Yl

ind <- rep(1:length(num.reps.collar), times=num.reps.collar) #  marked individuals and various locations from each


###############################################################################
# survey data
aerialSMR1 <- read_excel(paste(aerialDIR,"Flight1/2022Mar2_SMR_Data.xlsx", sep="/"), 
                       sheet = "Data", range = "A1:T12", col_types = "text") %>% type_convert()
aerialSMR1$SurveyNum <- "aSMR1"

aerialSMR2 <- read_excel(paste(aerialDIR,"Flight2/2022Mar15_SMR_Data.xlsx", sep="/"), 
                         sheet = "Data", range = "A1:U13", col_types = "text") %>% type_convert()
aerialSMR2$SurveyNum <- "aSMR2"

aerialSMR3 <- read_excel(paste(aerialDIR,"Flight3/2022Mar28_SMR_Data.xlsx", sep="/"), 
                         sheet = "Data", range = "A1:U10", col_types = "text") %>% type_convert()
aerialSMR3$SurveyNum <- "aSMR3"

aerialSMR <- bind_rows(aerialSMR1 %>% dplyr::select(`Elk Lat & Long`, `Group Size`, `Collar ID`, SurveyNum),
                       aerialSMR2 %>% dplyr::select(`Elk Lat & Long`, `Group Size`, `Collar ID`, SurveyNum), 
                       aerialSMR3 %>% dplyr::select(`Elk Lat & Long`, `Group Size`, `Collar ID`, SurveyNum))

aerialSMR$Latitude <- as.numeric(substr(aerialSMR$`Elk Lat & Long`, 1, 9))
aerialSMR$Longitude <- as.numeric(substr(aerialSMR$`Elk Lat & Long`, 12, nchar(aerialSMR$`Elk Lat & Long`)))

aerialSMR <- aerialSMR %>% rename(GrpSze = `Group Size`, CollarID = `Collar ID`)
aerialSMR <- aerialSMR[c("SurveyNum","Latitude","Longitude","CollarID","GrpSze")]

aerialSMR$Collar_ID <- as.numeric(gsub("([^0-9])", "", aerialSMR$CollarID))
aerialSMR %>% filter(!is.na(Collar_ID))

# one group had two collars & two groups had an old collar
# not sure how best to model this
# 1. creating two rows for the observation
# 2. divide the GrpSize between the two
# 3. attribute each duplicated row to just one collar
# 4. ignore the old collars as not unique ID
aerialSMR <- rbind(aerialSMR, aerialSMR[rep(26, times=1),])
# manual changing (I know not great but easy)
aerialSMR[26,]$Collar_ID <- 42256
aerialSMR[33,]$Collar_ID <- 42257

aerialSMR$GrpSze <- case_when(aerialSMR$Collar_ID==42256 ~ ceiling(17/2),
                              aerialSMR$Collar_ID==42257 ~ floor(17/2),
                              TRUE~aerialSMR$GrpSze)

# to  match with telem with individuals - for some reason no telem for 80333 - realise it is a mistake, should be 81333
aerialSMR$Collar_ID <- case_when(aerialSMR$Collar_ID==80333 ~ 81333,
                                 TRUE~aerialSMR$Collar_ID)

obsSMR_sf <- st_as_sf(aerialSMR, coords=c("Longitude", "Latitude"), crs=4326)
obsSMR_sf %>% group_by(SurveyNum) %>% summarise(mean(GrpSze), min(GrpSze), max(GrpSze), sum(GrpSze)) %>% st_drop_geometry()

obsSMR_sf <- st_join(obsSMR_sf %>% st_transform(3005), aoi_aSMR_grid %>% st_transform(3005) %>% dplyr::select(Id))


aerialSMR %>% filter(!is.na(Collar_ID))
# 7 active collars seen during 3 surveys: 3 in Survey1, 1 in Survey2, and 4 in Survey3
# 1 (31541) collar was seen in two surveys for a total of 8 collared observations

telem_aSMR %>% filter(jDay == 87) %>% dplyr::select(Date.Time.PST, jDay) %>% st_drop_geometry()
as.data.frame(telem_aSMR %>% filter(jDay %in% c(61, 74,87)) %>% arrange(jDay) %>% dplyr::select(Collar_ID, Date.Time.PST, jDay) %>% st_drop_geometry())

# jDay = 61 for March 2, jDay = 74 for March 15, jDay = 87 for March 28

aSMRnum <- c("aSMR2")
aSMRjDay <- c(73,74,75)

Cairo(file="out/aSMR2_TransectsObs_CollarObs.PNG", 
      type="png",
      width=2000, 
      height=3000, 
      pointsize=16,
      bg="white",
      dpi=300)
ggplot()+
  geom_sf(data=aoi %>% st_transform(3005))+
  geom_sf(data=aerialTransect %>% st_transform(3005) %>% filter(Transect==aSMRnum), col="darkgrey") +
  geom_sf(data=telem_aSMR %>% st_transform(3005) %>% filter(jDay %in% aSMRjDay), aes(fill=Collar_ID, col=Collar_ID), cex=2)+
  geom_sf(data=obsSMR_sf %>% st_transform(3005) %>% filter(SurveyNum==aSMRnum), fill="black", cex=2.8, pch=23) +
  geom_sf(data=obsSMR_sf %>% st_transform(3005) %>% filter(SurveyNum==aSMRnum) %>% filter(!is.na(Collar_ID)), 
          fill="red", cex=2.8, pch=23)+
  ggtitle("Aerial SMR Survey 2 \nTransects and Observations")
dev.off()

###--- prepping obs data for input to SMR
obsSMR_sf %>% filter(is.na(Id))
aerial_dat <- obsSMR_sf %>% st_drop_geometry()
grp_size <- aerial_dat %>% summarise(min = min(GrpSze), mean = mean(GrpSze), max = max(GrpSze), median = median(GrpSze))

ind.lookup <- as.data.frame(active.collars)
colnames(ind.lookup) <- "active.collar"
ind.lookup$ind <- rep(1:nrow(ind.lookup))
ind.lookup$active.collar <- as.factor(ind.lookup$active.collar)

aerial_dat$active.collar <- as.factor(aerial_dat$Collar_ID)
aerial_dat <- left_join(aerial_dat, ind.lookup)

aerialSMR %>% count(Collar_ID) %>% ungroup()
ind_dat <- aerial_dat %>% filter(!is.na(ind)) %>% dplyr::select(-CollarID, -Collar_ID, -active.collar) %>% arrange(ind, SurveyNum)
# ind_dat %>% count(Id)
# ind_dat %>% count(ind)

ind_dat$rowID <- rownames(gridXY)[match(ind_dat$Id, gridXY$grid.id)]


# dat$Yobs is a matrix nind:J:K
# rows are the 9 inds, columns are the traps J, and different slices for each occasion
n.marked = length(active.collars)
Yobs_ind_dat <- ind_dat %>% group_by(rowID, ind) %>% count(SurveyNum) %>% arrange(SurveyNum)
Yobs_ind_dat$Occ <- as.numeric(substr(Yobs_ind_dat$SurveyNum,5,5))

K = 3 
J = nrow(gridXY)

# create an empty array with rows as # of individuals, columns as camera traps, and slices as Occasions
Yobs <- array(0L, c(n.marked,J,K))
dim(Yobs)

for(i in 1:nrow(Yobs_ind_dat)){
  ind.value <- as.numeric(Yobs_ind_dat[i,c("ind")])
  J.value <- as.numeric(Yobs_ind_dat[i,c("rowID")])
  K.value <- as.numeric(Yobs_ind_dat[i,c("Occ")])
  dtn.value <- as.numeric(Yobs_ind_dat[i,c("n")])
  
  Yobs[ind.value,J.value,K.value] <- dtn.value
}

# sum(Yobs); sum(Yobs_ind_dat$n) # check to make sure the same

## Data on marked guys from resighting occasion
M = 250
yr.aug <- array(0L, c(M, J))
y2d <- apply(Yobs,c(1,2),sum) # 2-d encounter history 'nind' x 'ntraps' # remove k for faster processing
class(y2d) <- "integer"

yr.aug[1:n.marked,] <- y2d
# dim(yr.aug)
# dim(Yobs)
# sum(yr.aug) # should be the same as sum(Yobs)

n.tmp <- aerial_dat %>% group_by(Id) %>% count(SurveyNum)
n.tmp$gridID <- rownames(gridXY)[match(n.tmp$Id, gridXY$grid.id)]
n.tmp$Occ <- as.numeric(substr(n.tmp$SurveyNum,5,5))
n.tmp2 <- n.tmp %>% ungroup() %>% dplyr::select(-SurveyNum) %>% arrange(gridID)

# create an empty array with rows as # of individuals, columns as camera traps, and slices as Occasions
n <- array(0L, c(J,K))
dim(n)

for(i in 1:nrow(n.tmp2)){
  J.value <- as.numeric(n.tmp2[i,c("gridID")])
  K.value <- as.numeric(n.tmp2[i,c("Occ")])
  dtn.value <- as.numeric(n.tmp2[i,c("n")])
  
  n[J.value,K.value] <- dtn.value
}

sum(n.tmp2$n); sum(n); nrow(aerial_dat) # check they are all the same

yr.obs <- rowSums(n) # removing k for faster processing


eff <- rowSums(aSMR_oper)/K
length(eff)

length(yr.obs[yr.obs!=0])
length(yr.obs)
sum(yr.obs)


aSMR.data <- list(M=M,y=yr.aug, n=yr.obs, x=X2, nMarked=n.marked, 
                  J=J, effort=eff, nlocs=nlocs, ind=ind, locs=locs.scaled,
                  xlim=xlims.scaled, ylim=ylims.scaled, A=areaha.scaled)

###--- run aSMR

M=250

jd1 <- aSMR.data
ji1 <- function() list(z=rep(1,M))
# jp1 <- c("psi", "lam0", "sigma", "N", "D")
jp1 <- c("psi", "lam0", "sigma", "N", "D", "s", "z")


# run model - accounting for effort
(start.time <- Sys.time())
cl3 <- makeCluster(3)
clusterExport(cl3, c("jd1","ji1","jp1","M"))

aSMR_JAGS <- clusterEvalQ(cl3, {
  library(rjags)
  jm1 <- jags.model("elk_cSMR_trapeff.jag", jd1, ji1, n.chains=1, n.adapt=1000)
  jc1 <- coda.samples(jm1, jp1, n.iter=9000)
  return(as.mcmc(jc1))
})


mc.aSMR_JAGS <- mcmc.list(aSMR_JAGS)

(end.time <- Sys.time()) # 
mc.aSMR_JAGS.ET <- difftime(end.time, start.time, units='mins')
# Time difference of 475.9689 mins; 1708.4 mins for 9000 IT and M=250. Ouch.

save("mc.aSMR_JAGS",file="out/mc.elk_eff_aSMR_9KIt_map.RData")
# save("mc.aSMR_JAGS.ET",file="out/mc.elk_eff_aSMR_5KIt_map.ET.RData")
stopCluster(cl3)

summary(mc.aSMR_JAGS)
plot(mc.aSMR_JAGS)
################################################################################
# ## For simulations
# ## Sechelt Peninsula 95% KDe = summer 31.1 +/- 16.1 and winter = 17 +/- 2
# # consider range from 15 km2 to 45 km2
# # Following Royle et. al (2011), and assuming a
# # chi-squared distribution with 2 degrees of freedom, the range of sigma is given by
# # sqrt(15/pi)/sqrt(5.99)  # min 95 % HR  # 0.89 km
# # sqrt(45/pi)/sqrt(5.99)  # max 95% HR   # 1.55 km
# # spacing cameras ~2 km should be good (aim for distance of 2*sigma) so range from 1.8-3.2 km apart
# 
# ###--- for "elk"
# N <- 222
# M <- N*2   # to be on the safe side
# lambda0 <- 1
# sigma <- 1
# 
# J <- nrow(X2) # number of traps
# K <- 30   # sampling occasions
# n.marked <- 9 # number of marked elk (max collared elk in Sechelt Peninsula)
# nlocs = K


###--- simulate data
# use function from scrbook
# sim.pID.data <- function(N=N, K=K, sigma=sigma, lam0=lam0, knownID=knownID,X=X,
#                          xlims=xlims, ylims=ylims,  obsmod= c("pois", "bern"), nmarked=c("known", "unknown"),rat=1, tel =0, nlocs=0)
# {
#   
#   ###add an error message for when there are more tel guys than nmarked
#   if(tel>knownID) stop ("tel cannot be bigger than knownID")
#   
#   obsmod <- match.arg(obsmod)
#   nmarked <- match.arg(nmarked)
#   
#   # Home range centers
#   npts<-dim(X)[1]
#   sx <- runif(N, xlims[1], xlims[2])
#   sy <- runif(N, ylims[1], ylims[2])
#   S <- cbind(sx, sy)
#   D <- e2dist(S, X)
#   lam <- lam0*exp(-(D*D)/(2*sigma*sigma))
#   Y <- array(NA, c(N, npts, K))
#   for (i in 1:N){
#     for (j in 1: npts){
#       
#       if (identical(obsmod, "bern")){
#         Y[i,j,] <- rbinom(K,1, lam[i,j])
#       } else if (identical(obsmod, "pois"))  {
#         Y[i,j,] <- rpois(K,lam[i,j])
#       }
#     }}
#   
#   n <- apply(Y, c(2,3), sum)
#   
#   Yknown <- Y[1:knownID,,]
#   
#   if (identical(nmarked, "unknown")){
#     iobs<-which(apply(Yknown>0,1,any))
#     Yobs<-Yknown[iobs,,]
#   } else if (identical(nmarked, "known")){
#     Yobs<-Yknown }
#   
#   YknownR<-Yobs
#   counter<-array(0, c(dim(Yobs)[1],dim(X)[1],K ))
#   for (i in 1:dim(Yobs)[1]){
#     for (j in 1: dim(X)[1]){
#       for (k in 1:K){
#         
#         if (identical(obsmod, "bern")){
#           if (YknownR[i,j,k] ==1 ) {
#             IDed<-rbinom(1,1,rat)
#             if (IDed ==0) { 
#               YknownR[i,j,k]<-0
#               counter[i,j,k]<-1} #counter is the number of marked records that cannot be identified to individual level
#           }
#         } else if (identical(obsmod, "Ypois")) {
#           if (Yobs[i,j,k] > 0 ) {
#             
#             IDed<-sum(rbinom(Yobs[i,j,k] ,1,rat))
#             YknownR[i,j,k]<-IDed
#             
#             if (IDed!=Yobs[i,j,k] ) { 
#               counter[i,j,k]<-Yobs[i,j,k]-IDed}
#           }
#         }
#         
#         
#       }}}
#   
#   n<-n-apply(counter, 2:3, sum) #subtract unidentified pictures from n
#   
#   #generate telemetry locations if tel>0
#   if (tel>0) {
#     
#     itel<-sort(sample(1:knownID, tel, replace=F))
#     locs<-list()
#     for (i in 1:tel){
#       lx<-rnorm(nlocs, S[itel[i],1], sigma)
#       ly<-rnorm(nlocs, S[itel[i],2], sigma)
#       locs[[i]]<-cbind(lx, ly)
#     }
#     
#   } else {
#     locs<-NULL
#     itel<-NULL}
#   
#   list(n=n,Y=Y, Yknown=Yknown, Yobs=Yobs, YknownR=YknownR, counter=sum(counter), locs=locs,telID=itel)
#   
# }
# 
# dat <- sim.pID.data(N=N, K=K, sigma=sigma, lam0=lambda0, knownID = n.marked,
#                     X=X2, xlims=xlims.scaled, ylims=ylims.scaled,
#                     obsmod="pois", nmarked='known',
#                     tel=n.marked, nlocs=K) # each marked individual has telemetry and each has 1 fix per day (occasion)
################################################################################
###--- function to organise camera data for SMR

# consider area in ha, unit size is 100 m = 1 unit, buffer in unit lengths
organise_camera_SMR_data <- function(cam_sf=cam_sf, cam_dat=cam_dat, eff=eff, telem_sf=telem_sf, M=800, 
                                     coord.scale=100, buffer=20, start_date="2021-06-15", end_date="2021-09-01"){
  ###--- Camera meta data
  #  UTM Zone 10N, NAD83 (EPSG:26910)
  cam_coords <- st_coordinates(cam_sf %>% st_transform(26910))
  
  camXY <- cam_lcn %>% dplyr::select(station_id)
  camXY <- cbind(camXY, cam_coords)
  colnames (camXY) <- c("trap.id","x","y")
  
  traplocs <- as.matrix(camXY[,c("x","y")])
  X <- traplocs/coord.scale
  
  ###--- create xlims and ylims of scaled coordinates, and area
  Xl <- min(X[,1] - buffer)
  X.scaled <- X[,1] - Xl
  
  Yl <- min(X[,2] - buffer)
  Y.scaled <- X[,2] - Yl
  
  xlims.scaled <- c(min(X.scaled)-buffer,max(X.scaled)+buffer); ylims.scaled <- c(min(Y.scaled)-buffer,max(Y.scaled)+buffer)
  
  areaha.scaled <- xlims.scaled[2]*ylims.scaled[2]
  
  X2 <- as.matrix(cbind(X.scaled,Y.scaled))
  
  ### creating a data frame of study dates to align with occasions
  study.days <- colnames(eff[2:ncol(eff)])
  study.days <- str_replace(study.days, "X", "")
  study.days <- as.data.frame(study.days)
  study.days$Date <- ymd(study.days$study.days)
  study.days$Year <- year(study.days$Date)
  study.days$Month <- month(study.days$Date)
  study.days$jDay <- yday(study.days$Date)
  study.days$Occ <- rownames(study.days)
  
  study_period_days <- study.days%>% filter(between(Date, as.Date(start_date),as.Date(end_date))) %>% as_tibble
  study_period_days$SP_Occ <- rownames(study_period_days) # get the occasions for the study period, starting at 1
  
  ### creating operability matrix with rownames as station_id (same order as X2) and colnames as occasions
  eff <- eff %>% filter(!station_id %in% stations_to_exclude)
  rownames(eff) <- eff$station_id
  camop <- as.matrix(eff[,2:ncol(eff)])
  colnames(camop) <- 1:ncol(camop)
  SP_Occ_start <- min(as.numeric(study_period_days$Occ))
  SP_Occ_end <- max(as.numeric(study_period_days$Occ))
  
  camop <- camop[,SP_Occ_start:SP_Occ_end]
  
  telem_sf <- telem_sf %>% mutate(SmpPrd = case_when(Date.Time.PST %>% between(start_date, end_date) ~ 'SmpPrd'))
  
  telem_sf_smp <- telem_sf %>% filter(SmpPrd=="SmpPrd") %>% arrange(Collar_ID)
  active.collars <- unique(telem_sf_smp$Collar_ID)
  active.collars <- droplevels(active.collars)
  num.reps.collar <- telem_sf_smp %>% count(Collar_ID) %>% dplyr::select(n) %>% st_drop_geometry()
  num.reps.collar <- as.numeric(num.reps.collar$n)
  # summary(num.reps.collar)
  locs <- as.data.frame(st_coordinates(telem_sf_smp %>% st_transform(crs=26910))) # convert to NAD 83 UTM Zone 10 for consistency with trapping grid (m)
  colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
  nlocs <- nrow(locs)

  locs.scaled <- locs/coord.scale
  locs.scaled$Xcoord.scaled <- locs.scaled[,1] - Xl
  locs.scaled$Ycoord.scaled <- locs.scaled[,2] - Yl
  
  ind <- rep(1:length(num.reps.collar), times=num.reps.collar) #  marked individuals and various locations from each
  
  cam_dat <- cam_dat %>% dplyr::select(station_id, species, report_names, date_time, collar, collar_tags, event_id, event_duration, event_groupsize, event_observations)
  
  cam_dat$date_time <- ymd_hms(cam_dat$date_time, tz = tz)
  cam_dat$Date <- as.Date(cam_dat$date_time)
  cam_dat <- cam_dat %>% mutate(Year = year(Date), Month = lubridate::month(Date, label = T), jDay = yday(Date))
  
  cam_dat <- cam_dat %>% mutate(SmpPrd = case_when(Date %>% between(start_date, end_date) ~ 'SmpPrd'))
  cam_dat_smp <- cam_dat %>% filter(SmpPrd=="SmpPrd")
  elk_dat <- cam_dat_smp %>% filter(grepl("Cervus", species))
  
  grp_size <- elk_dat %>% summarise(min = min(event_groupsize), mean = mean(event_groupsize), max = max(event_groupsize), median = median(event_groupsize))
  
  # to  match with telem with individuals
  ind.lookup <- as.data.frame(active.collars)
  colnames(ind.lookup) <- "active.collar"
  ind.lookup$ind <- rep(1:nrow(ind.lookup))
  
  elk_dat$collar_tags <- as.factor(elk_dat$collar_tags)
  elk_dat <- left_join(elk_dat, ind.lookup, by=c("collar_tags"="active.collar"))
  elk_dat <- left_join(elk_dat %>% dplyr::select(!date_time), 
                       study.days %>% 
                         filter(between(Date, as.Date(start_date), as.Date(end_date))) %>% 
                         dplyr::select("Occ", "jDay"))
  elk_dat$SP_Occ <- as.numeric(study_period_days$SP_Occ[match(elk_dat$Occ, study_period_days$Occ)])
  
  ind_dat <- elk_dat %>% filter(!is.na(ind)) %>% dplyr::select(!collar) %>% arrange(ind, SP_Occ)
  # ind_dat %>% count(station_id)
  # ind_dat %>% count(ind)
  
  # dat$Yobs is a matrix nind:J:K
  # rows are the 9 inds, columns are the traps J, and different slices for each occasion
  n.marked = length(active.collars)
  ind_dat$trapID <- rownames(camXY)[match(ind_dat$station_id, camXY$trap.id)]
  Yobs_ind_dat <- ind_dat %>% group_by(trapID, ind) %>% count(SP_Occ) %>% arrange(SP_Occ)
  
  K = nrow(study_period_days) 
  J = nrow(camXY)
  
  # create an empty array with rows as # of individuals, columns as camera traps, and slices as Occasions
  Yobs <- array(0L, c(n.marked,J,K))
  
  for(i in 1:nrow(Yobs_ind_dat)){
    ind.value <- as.numeric(Yobs_ind_dat[i,c("ind")])
    J.value <- as.numeric(Yobs_ind_dat[i,c("trapID")])
    K.value <- as.numeric(Yobs_ind_dat[i,c("SP_Occ")])
    dtn.value <- as.numeric(Yobs_ind_dat[i,c("n")])
    
    Yobs[ind.value,J.value,K.value] <- dtn.value
  }
  
  # sum(Yobs); sum(Yobs_ind_dat$n) # check to make sure the same
  
  ## Data on marked guys from resighting occasion
  yr.aug <- array(0L, c(M, J))
  y2d <- apply(Yobs,c(1,2),sum) # 2-d encounter history 'nind' x 'ntraps' # remove k for faster processing
  class(y2d) <- "integer"
  
  yr.aug[1:n.marked,] <- y2d
  # dim(yr.aug)
  # dim(Yobs)
  # sum(yr.aug) # should be the same as sum(Yobs)
  
  # n.tmp <- elk_dat %>% group_by(station_id, SP_Occ) %>% summarise(n = sum(event_groupsize))
  n.tmp <- elk_dat %>% group_by(station_id) %>% count(SP_Occ) # to use if modelling groups and multiplying by group size
  
  n.tmp$trapID <- rownames(camXY)[match(n.tmp$station_id, camXY$trap.id)]
  n.tmp2 <- n.tmp %>% ungroup() %>% dplyr::select(-station_id) %>% arrange(trapID)
  
  # create an empty array with rows as # of individuals, columns as camera traps, and slices as Occasions
  n <- array(0L, c(J,K))
  
  for(i in 1:nrow(n.tmp2)){
    J.value <- as.numeric(n.tmp2[i,c("trapID")])
    K.value <- as.numeric(n.tmp2[i,c("SP_Occ")])
    dtn.value <- as.numeric(n.tmp2[i,c("n")])
    
    n[J.value,K.value] <- dtn.value
  }
  
  sum(n.tmp2$n); sum(n); nrow(elk_dat) # check they are all the same
  
  yr.obs <- rowSums(n) # removing k for faster processing
  
  return(list(M=M,yr.aug=yr.aug, yr.obs=yr.obs, n=n, Yobs_ind_dat=Yobs_ind_dat, X2=X2, n.marked=n.marked, J=J,K=K,
              nlocs=nlocs, ind=ind, locs.scaled=locs.scaled, camop = camop, grp_size=grp_size,
              xlims.scaled=xlims.scaled, ylims.scaled=ylims.scaled, areaha.scaled=areaha.scaled))
}


################################################################################
################################################################################
###--- Functions to create density maps
spatial_density_obj.function <- function(out = out){
  # summary(window(out[,c("D","lam0","sigma","psi")], start = 41001))
  # gelman.diag(window(out[,c("D","lam0","sigma","psi")], start = 1001), multivariate = F)
  # plot(window(out[,c("D","lam0","sigma","psi")], start = 1001))
  
  out1 <-out[[1]] # separate into individual chains - each 8000 iterations
  out2 <-out[[2]]
  out3 <-out[[3]]
  
  # str(out1)
  
  Sxout1 <- out1[4001:9000,5:254]
  Syout1 <- out1[4001:9000,255:504]
  z1 <- out1[4001:9000, 506:755]
  
  Sxout2 <- out2[4001:9000,5:254]
  Syout2 <- out2[4001:9000,255:504]
  z2 <- out2[4001:9000, 506:755]
  
  Sxout3 <- out3[4001:9000,5:254]
  Syout3 <- out3[4001:9000,255:504]
  z3 <- out3[4001:9000, 506:755]
  
  obj1 <- list(Sx=rbind(Sxout1, Sxout2, Sxout3), Sy=rbind(Syout1,Syout2,Syout3), z=rbind(z1,z2,z3))
  
  return(obj1)
}


SCRdensity <- function (obj, nx = 30, ny = 30, Xl = NULL, Xu = NULL, 
                        Yl = NULL, Yu = NULL, scalein = 100, scaleout = 100, 
                        col = "gray", ncolors = 10, whichguy = NULL){
  #obj <- SC_obj 
  
  Sxout <- obj$Sx
  Syout <- obj$Sy
  z <- obj$z
  niter <- nrow(z)
  if (is.null(Xl)) {
    Xl <- min(Sxout) * 0.999
    Xu <- max(Sxout) * 1.001
    Yl <- min(Syout) * 0.999
    Yu <- max(Syout) * 1.001
  }
  xg <- seq(Xl, Xu, , nx)
  yg <- seq(Yl, Yu, , ny)
  guy <- col(Sxout)
  Sxout <- cut(Sxout[z == 1], breaks = xg)
  Syout <- cut(Syout[z == 1], breaks = yg)
  if (is.null(whichguy)) {
    Dn <- table(Sxout, Syout)/niter
    area <- (yg[2] - yg[1]) * (xg[2] - xg[1]) * scalein
    Dn <- (Dn/area) * scaleout
  }
  else {
    Dn <- table(Sxout[guy == whichguy], Syout[guy == whichguy])/niter
  }
  cat("mean: ", mean(Dn), fill = TRUE)
  par(mar = c(3, 3, 3, 6))
  if (col == "gray") {
    cc <- seq(3, 17, , 10)/20
    cc <- rev(gray(cc))
  }
  else cc <- terrain.colors(ncolors)
  
  image(xg, yg, Dn)
  #image.scale(Dn, col = cc)
  box()
  
  return(list(grid = cbind(xg, yg), Dn = Dn))
}


###--- density map to raster function
# Dn object from SCRdensity function
# traplocs can be either matrix or dataframe of x and y locations (utm)
# buffer in m units

SCRraster <- function (Dn = Dn, traplocs = traplocs, buffer = buffer, crs = crs){
  r <- raster(ncol=ncol(Dn), nrow=nrow(Dn))
  values(r) <- Dn
  as.raster(r)
  t_r <- t(r) # for some reason map is flipped, need to transpose
  
  # add correct extent and coordinate system
  minx <- min(traplocs[,1])
  maxx <- max(traplocs[,1])
  miny <- min(traplocs[,2])
  maxy <- max(traplocs[,2])
  
  bb <- extent(minx-buffer, maxx+buffer,miny-buffer, maxy+buffer) 
  extent(r) <- bb
  r <- setExtent(r, bb, keepres=TRUE)
  crs(r) <- crs
  
  bb <- extent(minx-buffer, maxx+buffer,miny-buffer, maxy+buffer) 
  extent(t_r) <- bb
  t_r <- setExtent(t_r, bb, keepres=TRUE)
  crs(t_r) <- crs
  
  return(list(Dn_Traster = t_r, Dn_raster = r))
}

################################################################################
################################################################################
###--- function to load mcmc files and write output to table

#function to create output table for JAGS output
get_JAGS_output <- function(filename){
  out <- filename
  s <- summary(window(out[,c("D","N","lam0","sigma","psi")], start = 4001))
  gd <- gelman.diag(window(out[,c("D","N","lam0","sigma","psi")], start = 4001),multivariate = FALSE)
  output_table <- rbind(as.data.frame(t(s$statistics)),
                        as.data.frame(t(s$quantiles)),
                        as.data.frame(t(gd$psrf)))
  return(output_table)
}

#function to return model run time (hours)
get_JAGS_ET <- function(filename){
  ET <- parse_number(filename) # parse function no longer working, not sure why
  return(ET)
}

################################################################################
###--- For field data ---###

###--- Input study area  / camera trap location data
# trap ID for marking occasions - unknown so using camera array (elk marked by completely different process, independent)
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_NA")

aoi <- EPU_poly %>% filter(EPU_Unit_N=="Sechelt Peninsula")

cam_lcn <- read.csv("data/SPCS_station_data.csv", head=TRUE) %>% as_tibble()
glimpse(cam_lcn)
cam_lcn %>% count(treatment)
cam_lcn <- cam_lcn %>% mutate(habitat = case_when(treatment=="Forest - Mixed" ~ "Forest_Mixed",
                                                  grepl("Forest - Conifer", treatment) ~ "Forest_Conifer",
                                                  treatment=="Forest - Grass - Road" ~ "Forest_Conifer",
                                                  grepl("Open", treatment) ~ "Open"))

cam_lcn %>% count(habitat)
cam_lcn <- cam_lcn %>% arrange(station_id)
stations_to_exclude <- c("SPCS17", "SPCS61","SPCS04") # remove the two stations without data and one cam positioned too high
cam_lcn <- cam_lcn %>% filter(!station_id %in% stations_to_exclude) 

cam_sf <- st_as_sf(cam_lcn,coords = c("longitude", "latitude"), crs = 4326)

# To find out mean, min, max and se of min camera spacing
# library(nngeo)
# cam_dist <- st_nn(cam_sf %>% st_transform(crs=3005), cam_sf %>% st_transform(crs=3005), k=2, returnDist=TRUE)
# cam_dist <- unlist(cam_dist$dist)
# cam_dist <- cam_dist[cam_dist>0]
# mean(cam_dist) # 1888.764
# sd(cam_dist)/sqrt(length(cam_dist)) # 57.18362
# min(cam_dist) # 1358.971
# max(cam_dist) # 3494.763

cam_dat <- read.csv("data/SPCS_30min_Independent.csv", head=TRUE) %>% as_tibble() %>% type_convert()

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=cam_sf, aes(col=habitat))

eff <- read.csv("data/SPCS_operability_matrix.csv", head=TRUE)

telem <- read.csv("data/Collars_Sechelt.csv", stringsAsFactors = TRUE) %>% as_tibble() %>%
  dplyr::select(Collar_ID, SCTS__UTC_, Latitude_d, Longitude_ ,Easting, Northing) %>%
  rename(Date.Time.UTC = SCTS__UTC_, Latitude = Latitude_d, Longitude=Longitude_)

###--- telem locations
telem$Date.Time.UTC <- ymd_hms(telem$Date.Time.UTC, tz = "UTC")
telem$Date.Time.PST <- with_tz(telem$Date.Time.UTC, tz)
telem <- telem %>% mutate(Year = year(Date.Time.PST), Month = lubridate::month(Date.Time.PST, label = T), jDay = yday(Date.Time.PST))

telem %>% group_by(Collar_ID) %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
telem %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
# telem location data for Sechelt Peninsula from 2021-06-13 19:01:36 to 2022-06-01 10:19:55 

telem_sf <- st_as_sf(telem %>% filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs=4326)
telem_sf %>% count(Collar_ID) %>% st_drop_geometry()
telem_sf$Collar_ID <- as.factor(telem_sf$Collar_ID)

telem_sf %>% summarise(min(Date.Time.PST), max(Date.Time.PST)) %>% st_drop_geometry()

unique(telem_sf$Collar_ID)
ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=telem_sf %>% filter(Month %in% c("Feb","Mar","Apr")),aes(fill=Collar_ID, col=Collar_ID))+
  geom_sf(data=cam_sf, cex=2, col="blue")


##########################################
###--- create camera data for different study periods

# consider population periods based on seasonality and life history traits
# check with Tristen (Dan / Darryl) to finalise seasonaliy dates
# open hunting season (deer) in Sechelt Sept 1- Nov 30. Season 2 = hunting/rutting = Sept 15 - Nov 15
# Goal is to have ~90 day intervals to maximise data while minimising violation of closed population assumption
# have calving / summer season as first option and start of our research. Season 1 = calving = 15 June - 1 Sept
# then overlap with our surveys and winter. Season 2 = winter = 1 Jan - 31 Mar


cSMR_smp1 <- organise_camera_SMR_data(cam_sf=cam_sf, cam_dat=cam_dat, eff=eff, telem_sf=telem_sf,
                                      M=400,coord.scale=100, buffer=20, start_date="2021-06-15", end_date="2021-09-01")
  
cSMR_smp2 <- organise_camera_SMR_data(cam_sf=cam_sf, cam_dat=cam_dat, eff=eff, telem_sf=telem_sf,
                                      M=400,coord.scale=100, buffer=20, start_date="2022-01-01", end_date="2022-03-31")


elk_dat <- cam_dat %>% filter(grepl("Cervus", species))
elk_dat %>% summarise(mean = mean(event_groupsize), sd = sd(event_groupsize))
summary(elk_dat$event_groupsize)
cSMR_smp1$grp_size # 1  2.72    13 2
cSMR_smp2$grp_size # 1  3.11    10 2

cSMR_smp1$cam_eff <- rowSums(cSMR_smp1$camop)/cSMR_smp1$K
cSMR_smp1$cam_eff[cSMR_smp1$cam_eff==0]
length(cSMR_smp1$cam_eff)

length(cSMR_smp1$yr.obs[cSMR_smp1$yr.obs!=0])
length(cSMR_smp1$yr.obs)
sum(cSMR_smp1$yr.obs)

cSMR_smp2$cam_eff <- rowSums(cSMR_smp2$camop)/cSMR_smp2$K
cSMR_smp2$cam_eff[cSMR_smp2$cam_eff==0]
length(cSMR_smp2$cam_eff)

length(cSMR_smp2$yr.obs[cSMR_smp2$yr.obs!=0])
length(cSMR_smp2$yr.obs)
sum(cSMR_smp2$yr.obs)

# #########################################
# cSMR_smp1$grp_size # 1  2.72    13
# cSMR_smp2$grp_size # 1  3.11    10
# for now let's model events, and multiply density by average group size for total animals

## ----Data for SMR-----------------------------------------------------------
# Run JAGS code

cSMR.data <- list(M=cSMR_smp1$M,y=cSMR_smp1$yr.aug, n=cSMR_smp1$yr.obs, x=cSMR_smp1$X2, nMarked=cSMR_smp1$n.marked, 
                  J=cSMR_smp1$J, effort=cSMR_smp1$cam_eff, nlocs=cSMR_smp1$nlocs, ind=cSMR_smp1$ind, locs=cSMR_smp1$locs.scaled,
                  xlim=cSMR_smp1$xlims.scaled, ylim=cSMR_smp1$ylims.scaled, A=cSMR_smp1$areaha.scaled)

cSMR.data <- list(M=cSMR_smp2$M,y=cSMR_smp2$yr.aug, n=cSMR_smp2$yr.obs, x=cSMR_smp2$X2, nMarked=cSMR_smp2$n.marked, 
                  J=cSMR_smp2$J, effort=cSMR_smp2$cam_eff, nlocs=cSMR_smp2$nlocs, ind=cSMR_smp2$ind, locs=cSMR_smp2$locs.scaled,
                  xlim=cSMR_smp2$xlims.scaled, ylim=cSMR_smp2$ylims.scaled, A=cSMR_smp2$areaha.scaled)


M=400

jd1 <- cSMR.data
ji1 <- function() list(z=rep(1,M))
jp1 <- c("psi", "lam0", "sigma", "N", "D", "s", "z")

# run model - accounting for effort
(start.time <- Sys.time())
cl3 <- makeCluster(3)
clusterExport(cl3, c("jd1","ji1","jp1","M"))

cSMR_JAGS <- clusterEvalQ(cl3, {
  library(rjags)
  jm1 <- jags.model("elk_cSMR_trapeff.jag", jd1, ji1, n.chains=1, n.adapt=1000)
  jc1 <- coda.samples(jm1, jp1, n.iter=8000)
  return(as.mcmc(jc1))
})

mc.cSMR_JAGS <- mcmc.list(cSMR_JAGS)

(end.time <- Sys.time()) # 
mc.cSMR_JAGS.ET <- difftime(end.time, start.time, units='mins')

# save("mc.cSMR_JAGS",file="out/mc.elk_SmpPrd1_eff_cSMR_8KIt.RData")
# save("mc.cSMR_JAGS",file="out/mc.elk_SmpPrd1_eff_cSMR_8KIt_map.RData") # if including s and z in parameters to watch
save("mc.cSMR_JAGS",file="out/mc.elk_SmpPrd2_eff_cSMR_8KIt_map.RData")
# save("mc.cSMR_JAGS.ET",file="out/mc.elk_SmpPrd2_effcSMR_8KIt.ET.RData")
stopCluster(cl3)


###############################################################
###--- view output
mc.aSMR_JAGS.ET # 62 mins for 55 cams, M=400, 8000 IT

  summary(window(out[,c("D","lam0","sigma","psi")], start = 4001))
  gelman.diag(window(out[,c("D","lam0","sigma","psi")], start = 4001), multivariate = F)
  plot(window(out[,c("D","lam0","sigma","psi")], start = 1001))



######################################
###--- output tables
load("out/mc.elk_eff_aSMR_9KIt_map.RData")

# load("out/mc.elk_eff_aSMR_8KIt.RData")
# load("out/mc.elk_SmpPrd2_eff_cSMR_8KIt.RData")

out <-mc.aSMR_JAGS
str(out)
summary(out)
options(scipen = 100)

out_elk_aSMR_9KIT <- get_JAGS_output(out)
write.csv(out_elk_aSMR_9KIT, "out/out_elk_aSMR_9KIT.csv")

# out_elk_aSMR_5KIT <- get_JAGS_output(out)
# write.csv(out_elk_aSMR_5KIT, "out/out_elk_aSMR_5KIT.csv")

plot(out_elk_aSMR_5KIT)
plot(out)

out_elk_SmpPrd1_cSMR_8KIt_8KBIN <- get_JAGS_output(out)
write.csv(out_elk_SmpPrd1_cSMR_8KIt_8KBIN, "out/out_elk_SmpPrd1.cSMR_8KIt_8KBIN.csv")


out_elk_SmpPrd2_cSMR_8KIt_8KBIN <- get_JAGS_output(out)
write.csv(out_elk_SmpPrd2_cSMR_8KIt_8KBIN, "out/out_elk_SmpPrd2.cSMR_8KIt_8BIN.csv")


###---
# all MCMC output is in 100 m units
# if input coordinate system is in "100 m" then input scalein=1
# so have as 1 if want in 100 m units
load("out/mc.elk_SmpPrd2_eff_cSMR_8KIt_map.RData")
load("out/mc.elk_eff_aSMR_9KIt_map.RData")
out <- mc.aSMR_JAGS
str(out[[1]])
head(out[[1]][506])
head(out[[1]])
SC_obj <- spatial_density_obj.function(out = out)
str(SC_obj)

cam_coords <- st_coordinates(cam_sf %>% st_transform(26910))

camXY <- cam_lcn %>% dplyr::select(station_id)
camXY <- cbind(camXY, cam_coords)
colnames (camXY) <- c("trap.id","x","y")

traplocs <- as.data.frame(camXY[,c("x","y")])
traplocs
buffer <- 2000 # * 100 m scale for 2000 m buffer = 20
sa_x <- max(traplocs$x+buffer) - min(traplocs$x-buffer) 
sa_y <- max(traplocs$y+buffer) - min(traplocs$y-buffer) 

SC_map <- SCRdensity(SC_obj, nx=25, ny=25)
str(SC_map)
SC_raster <- SCRraster(Dn = SC_map$Dn, traplocs = traplocs, buffer = 2000, crs = c("+init=epsg:26910"))
plot(flip(SC_raster$Dn_Traster, direction="y"))
writeRaster(flip(SC_raster$Dn_Traster, direction="y"),"out/elk_aSMR_9KIt_raster_diry.tif", overwrite=TRUE)
writeRaster(flip(SC_raster$Dn_Traster, direction="y"),"out/elk_SmpPrd2_eff_cSMR_8KIt_raster_diry.tif", overwrite=TRUE)

Cairo(file="out/elk_aSMR_9KI_SpatialDensity.PNG", 
      type="png",
      width=2000, 
      height=1600, 
      pointsize=12,
      bg="white",
      dpi=300)
plot(flip(SC_raster$Dn_Traster, direction="y"))
# points(traplocs, pch=20, cex=1)
mtext(paste("Elk Winter 2022\nRealised Density Map",sep=""), side = 3, line = 1, cex=1.25)
dev.off()


getwd()
################################
# For simulations

load("out/mc.elk.sim.cSMR_60cam_9telem_20KIt.RData")

str(out[1])
out1 <- out[1]; out2 <- out[2]; out3<-out[3]

Nout1 <- unlist(out1[,2]); Nout2 <- unlist(out2[,2]); Nout3 <- unlist(out3[,2])
Nall <- c(Nout1, Nout2, Nout3)
length(Nall)


# create 30 datasets of simulated camera data from JAGS output
sim.cSMR_60.Sechelt <- vector('list', 30)
names(sim.cSMR_40.Sechelt) <- paste0('sim.cSMR_60.Sechelt', seq_along(sim.cSMR_60.Sechelt))
for(i in seq_along(sim.cSMR_60.Sechelt)){
  Nsample <- sample(Nall, 10000, replace=FALSE)
  sim.cSMR_60.Sechelt.list <- quantile(Nsample, c(0.05, 0.5, 0.95))
  sim.cSMR_60.Sechelt[[i]] <- sim.cSMR_60.Sechelt.list
}

quantile.cSMR_60 <- matrix(NA,30,4)
count <- 1
for(i in 1:nrow(quantile.cSMR_60)){
  quantile.cSMR_60[count,1] <- sim.cSMR_60.Sechelt[[i]][1]
  quantile.cSMR_60[count,2] <- sim.cSMR_60.Sechelt[[i]][2]
  quantile.cSMR_60[count,3] <- sim.cSMR_60.Sechelt[[i]][3]
  quantile.cSMR_60[count,4] <- "cam60"
  count <- count + 1
}

quantile.cSMR_60 <- as.data.frame(quantile.cSMR_60)
colnames(quantile.cSMR_60) <- c("LCL", "Mean", "UCL","Num.Cam")
pop.size=222

quantile.cSMR <- rbind(quantile.cSMR_40, quantile.cSMR_60)
quantile.cSMR$Count <- 1:nrow(quantile.cSMR)
quantile.cSMR$LCL <- as.numeric(quantile.cSMR$LCL)
quantile.cSMR$Mean <- as.numeric(quantile.cSMR$Mean)
quantile.cSMR$UCL <- as.numeric(quantile.cSMR$UCL)

col.cat <- as.character(c("#2028B2","#B2AA20"))

cSMR_simsplot = quantile.cSMR %>%
  ggplot(aes(x = reorder(rownames(quantile.cSMR),Mean), y=Mean, fill=Num.Cam))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(rownames(quantile.cSMR), ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))+
  ylim(c(0,500))
cSMR_simsplot
ggsave(cSMR_simsplot, file="out/cSMR_simsplot_40cam.PNG")


#######################################################################
######################################
#create function to load files and write output to table

#function to create output table for JAGS output
get_JAGS_output <- function(filename){
  out <- filename
  s <- summary(window(out, start = 4001))
  gd <- gelman.diag(window(out, start = 4001),multivariate = FALSE)
  output_table <- rbind(as.data.frame(t(s$statistics)),
                        as.data.frame(t(s$quantiles)),
                        as.data.frame(t(gd$psrf)))
  return(output_table)
}

get_JAGS_output(out)

#function to return model run time (hours)
get_JAGS_ET <- function(filename){
  ET <- parse_number(filename) # parse function no longer working, not sure why
  return(ET)
}

######################################
getwd()
info <- list.files(paste(getwd(),"/out",sep=""),"*.RData")

info.mc <- unique(gsub('ET.','',info))
info.mc.40 <- info.mc[grepl("40cam",info.mc)]
info.mc.60 <- info.mc[grepl("60cam",info.mc)]

######################################

# create datasets of simulated camera data from JAGS output
# output for 40 cam simulations
sim.cSMR_40.Sechelt <- vector('list', length(info.mc.40))
names(sim.cSMR_40.Sechelt) <- paste0('sim.cSMR_40.Sechelt', seq_along(sim.cSMR_40.Sechelt))
for(i in seq_along(sim.cSMR_40.Sechelt)){
  load(paste("out/",info.mc.40[i],sep=""))
  out <- mc.cSMR_JAGS
  sim.cSMR_40.Sechelt.list <- get_JAGS_output(out)
  sim.cSMR_40.Sechelt[[i]] <- sim.cSMR_40.Sechelt.list
}

quantile.cSMR_40 <- matrix(NA,length(info.mc.40),5)
count <- 1
for(i in 1:nrow(quantile.cSMR_40)){
  quantile.cSMR_40[count,1] <- sim.cSMR_40.Sechelt[[i]][5,"N"]
  quantile.cSMR_40[count,2] <- sim.cSMR_40.Sechelt[[i]][7,"N"]
  quantile.cSMR_40[count,3] <- sim.cSMR_40.Sechelt[[i]][9,"N"]
  quantile.cSMR_40[count,4] <- "cam40"
  quantile.cSMR_40[count,5] <- count
  count <- count + 1
}

quantile.cSMR_40 <- as.data.frame(quantile.cSMR_40)
colnames(quantile.cSMR_40) <- c("LCL", "Mean", "UCL","Num.Cam","Sim")

# output for 60 cam simulations
sim.cSMR_60.Sechelt <- vector('list', length(info.mc.60))
names(sim.cSMR_60.Sechelt) <- paste0('sim.cSMR_40.Sechelt', seq_along(sim.cSMR_60.Sechelt))
for(i in seq_along(sim.cSMR_60.Sechelt)){
  load(paste("out/",info.mc.60[i],sep=""))
  out <- mc.cSMR_JAGS
  sim.cSMR_60.Sechelt.list <- get_JAGS_output(out)
  sim.cSMR_60.Sechelt[[i]] <- sim.cSMR_60.Sechelt.list
}

quantile.cSMR_60 <- matrix(NA,length(info.mc.60),5)
count <- 1
for(i in 1:nrow(quantile.cSMR_60)){
  quantile.cSMR_60[count,1] <- sim.cSMR_60.Sechelt[[i]][5,"N"]
  quantile.cSMR_60[count,2] <- sim.cSMR_60.Sechelt[[i]][7,"N"]
  quantile.cSMR_60[count,3] <- sim.cSMR_60.Sechelt[[i]][9,"N"]
  quantile.cSMR_60[count,4] <- "cam60"
  quantile.cSMR_60[count,5] <- count
  count <- count + 1
}


quantile.cSMR_60 <- as.data.frame(quantile.cSMR_60)
colnames(quantile.cSMR_60) <- c("LCL", "Mean", "UCL","Num.Cam","Sim")


pop.size=222

quantile.cSMR <- rbind(quantile.cSMR_40, quantile.cSMR_60)
quantile.cSMR$LCL <- as.numeric(quantile.cSMR$LCL)
quantile.cSMR$Mean <- as.numeric(quantile.cSMR$Mean)
quantile.cSMR$UCL <- as.numeric(quantile.cSMR$UCL)

col.cat <- as.character(c("#2028B2","#B2AA20"))
quantile.cSMR <- quantile.cSMR %>% arrange(Num.Cam, Mean)
quantile.cSMR$Graph.Order <- 1:nrow(quantile.cSMR)

cSMR_simsplot = quantile.cSMR %>%
  ggplot(aes(x = Graph.Order, y=Mean, fill=Num.Cam))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(Graph.Order, ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
cSMR_simsplot
ggsave(cSMR_simsplot, file="out/cSMR_simsplot_40-60cam_9telem.PNG")

