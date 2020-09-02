

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read data
data_orig <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/trawl_impacts/data/saup/raw/SeaAroundUs_all.csv", as.is=T)


# Inspect data
################################################################################

# Inspect data
colnames(data_orig)
sort(unique(data_orig$area_name))
sort(unique(data_orig$area_type))
sort(unique(data_orig$functional_group))
sort(unique(data_orig$commercial_group))
sort(unique(data_orig$fishing_entity)) # EEZs
sort(unique(data_orig$fishing_sector))
sort(unique(data_orig$reporting_status))
sort(unique(data_orig$gear_type))

# Build aggregated time series
data <- data_orig %>% 
  # Reported landings only
  filter(reporting_status=="Reported") %>% 
  # Add fishery
  mutate(fishery=paste(fishing_entity, fishing_sector, gear_type, sep="-")) %>% 
  # Summarize by sector, gear, year
  group_by(fishing_entity, fishing_sector, gear_type, fishery, year) %>% 
  summarize(catch_mt=sum(tonnes, na.rm=T)) %>% 
  ungroup()

# Fisheries
fisheries <- data %>% 
  # 2014
  filter(year==max(year)) %>% 
  # Proportion of global landings
  mutate(catch_prop=catch_mt/sum(catch_mt)) %>% 
  # Arrange by gear type
  arrange(fishing_entity, fishing_sector, gear_type, fishery)




# Gears

# Build recent year stats
gears <- data %>% 
  # Group by sector-gear
  group_by(fishing_sector, gear_type, year) %>% 
  summarize(catch_mt=sum(catch_mt)) %>% 
  ungroup() %>% 
  # 2014
  filter(year==max(year)) %>% 
  # Proportion of global landings
  mutate(catch_prop=catch_mt/sum(catch_mt)) %>% 
  # Arrange by gear type
  arrange(fishing_sector, gear_type)

# Export data
save(data, fisheries, gears, file=file.path(datadir, "SAUP_reported_catch_by_country_sectors_gears.Rdata"))
write.csv(gears, file=file.path(datadir, "SAUP_2014_reported_catch_by_sectors_gears.csv"), row.names=F)






