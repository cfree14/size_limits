

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
load(file.path(datadir, "SAUP_reported_catch_by_country_sectors_gears.Rdata"))

# Read sector-gear key
key <- readxl::read_excel(file.path(datadir, "SAUP_sector_gear_key.xlsx")) %>% 
  setNames(c("sector", "gear", "nfisheries", "catch_mt", "catch_prop", 
             "devices", "limits_yn", "defacto_yn", "discard_mortality"))


# Build data
################################################################################

# Inspect
table(key$limits_yn)
table(key$defacto_yn)
table(key$discard_mortality)

# Identity sector-gears that are good candidates for size limits
key <- key %>% 
  mutate(candidate=ifelse(discard_mortality %in% c("low", "medium") &
                            limits_yn %in% c("unlikely", "less likely") & 
                            defacto_yn %in% c("unlikely", "less likely"), "yes", "no"))

# Calculate stats
stats <- key %>% 
  group_by(candidate) %>% 
  summarize(nfisheries=sum(nfisheries),
            catch_mt=sum(catch_mt)) %>% 
  ungroup() %>% 
  mutate(pfisheries=nfisheries/sum(nfisheries),
         pcatch=catch_mt/sum(catch_mt))

