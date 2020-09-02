

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read data
load(file.path(datadir, "SAUP_reported_catch_by_country_sectors_gears.Rdata"))

# Read sector-gear key
key <- readxl::read_excel(file.path(datadir, "SAUP_sector_gear_key.xlsx")) %>% 
  setNames(c("sector", "gear", "devices", "limits_yn", "defacto_yn", "discard_mortality"))

# Key with catch
key_w_catch <- read.csv(file.path(datadir, "SAUP_2014_reported_catch_by_sectors_gears.csv"), as.is=T)

# Build data
################################################################################

# Inspect
table(key$limits_yn)
table(key$defacto_yn)
table(key$discard_mortality)

# Identity sector-gears that are good candidates for size limits
key_out <- key %>% 
  # Classify as candidate
  mutate(candidate=ifelse(discard_mortality %in% c("low", "medium") &
                            limits_yn %in% c("unlikely", "less likely") & 
                            defacto_yn %in% c("unlikely", "less likely"), "yes", "no")) %>% 
  # Add catch stats
  left_join(key_w_catch, c("sector"="fishing_sector", "gear"="gear_type")) %>% 
  # Arrange
  select(sector, gear, year, nfisheries, catch_mt, catch_prop, everything())
  
# Export key
write.csv(key_out, file=file.path(datadir, "SAUP_sector_gear_key_done.csv"), row.names=F)

# Calculate stats
stats <- key_out %>% 
  group_by(candidate) %>% 
  summarize(nfisheries=sum(nfisheries),
            catch_mt=sum(catch_mt)) %>% 
  ungroup() %>% 
  mutate(pfisheries=nfisheries/sum(nfisheries),
         pcatch=catch_mt/sum(catch_mt))

# Classify fisheries
fisheries_out <- fisheries %>%
  # Mark candidates
  left_join(key_out %>% select(sector, gear, candidate), 
            by=c("fishing_sector"="sector", "gear_type"="gear")) %>% 
  # Rename columns
  rename(sector=fishing_sector, gear=gear_type, country=fishing_entity) %>% 
  # Add country
  mutate(country_use=countrycode(country, "country.name", "country.name"),
         iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
  # Rearrange
  select(country, country_use, iso3_use, everything())

# Export fisheries
write.csv(fisheries_out, file=file.path(datadir, "SAUP_country_sector_gear_fisheries_key.csv"),
          row.names=F)



