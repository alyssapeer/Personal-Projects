# load in libraries
library(tidyverse)
library(dplyr)
library(sf)
library(viridis)

# read in data sets
energy2019 <- read.csv('Data/Chicago_Energy_Benchmarking_2019.csv')
energy2020 <- read.csv('Data/Chicago_Energy_Benchmarking_2020.csv')

# bind data frames together
energy <- rbind(energy2019, energy2020)

# remove unwanted variables
energy19_20 <- energy[, -c(8, 18, 19, 20, 23, 24)]
glimpse(energy19_20)

# filter to only locations that submitted
energy_new <-  energy19_20 %>%
  filter(Reporting.Status == 'Submitted Data')

energy_new %>% 
  filter(!is.na(Location))

energy_new %>%
  filter(!is.na(ZIP.Code))

# new column names
colnames(energy_new)[6] <- c('zip')


#  read in the location variable as an sf feature
energy_sf <- st_as_sf(energy_new, 
                     coords = c('Latitude', 'Longitude'),
                     crs = 4326,
                     remove = F)
glimpse(energy_sf)

# read in zip codes
zip_codes <- read_sf('zip')

st_crs(zip_codes)
plot(zip_codes)
glimpse(zip_codes)

# transform zip code crs
zip_4326 <- st_transform(zip_codes, crs = 4326)
glimpse(zip_4326)
class(zip_4326)
head(zip_4326)

# group data points into zip code
energy_zip <- st_join(energy_sf, zip_4326, join = st_within)

head(energy_zip)
nrow(energy_zip)
glimpse(energy_zip)

# turn back to data frame to remove sticky geometry
energy_by_zip <- energy_zip %>%
  as.data.frame() %>%
  group_by(zip.x) %>%
  summarise(source_eui = sum(Source.EUI..kBtu.sq.ft.))

glimpse(energy_by_zip)

# use a left_join to get zip code shapefile data; this step is needed to regain zip code geometries so we can make our maps
energy_zip_map <- left_join(zip_codes, energy_by_zip, by = c('zip' = 'zip.x'))
glimpse(energy_zip_map)

plot(energy_zip_map['source_eui'])
ggplot(energy_zip_map)+
  geom_sf(aes(fill = source_eui))+
  scale_fill_viridis()

# gray areas are where data was not captured