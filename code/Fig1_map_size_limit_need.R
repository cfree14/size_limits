

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(rnaturalearth)

# Directories
datadir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read data
data <- read.csv(file=file.path(datadir, "SAUP_country_sector_gear_fisheries_key.csv"), as.is=T)

# World
world <- ne_countries(type="countries", scale="small", returnclass="sf") %>% 
  # Fix a few ISO3s
  mutate(sov_a3_use=countrycode(sovereignt, "country.name", "iso3c"))

# Setup
################################################################################

# Totals
ngood <- data %>% 
  group_by(iso3_use) %>% 
  summarize(nfisheries=sum(candidate=="yes")) %>% 
  filter(!is.na(iso3_use))

# Add totals to world
ngood_sf <- world %>% 
  left_join(ngood, by=c("sov_a3_use"="iso3_use"))

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot world
g <- ggplot(ngood_sf) + 
  geom_sf(ngood_sf, mapping=aes(fill=nfisheries), color="grey30", lwd=0.1) +
  # Labels
  labs(title="Number of fisheries\nthat are good candidates for size limits") +
  # Legend
  scale_fill_gradientn(name="Number of fisheries",
                       colors=RColorBrewer::brewer.pal(n=9, "YlOrRd"),
                       na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "map_of_size_limit_candidates.png"), 
       width=6.5, height=4.5, units="in", dpi=600)





