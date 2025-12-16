library(dplyr)
library(sf)
library(zipcodeR)
library(MASS)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(patchwork)
library(scales)
library(tidyr)
library(tigris)
library(tidycensus)
library(viridis)
library(lubridate)
library(broom)

#Loading my data, filtering for my urban and rural lakes and making it easier on myself by making my urban lake ID's vectors

creeltable <- read.csv("creeltable.csv")

urban_lakes <- c("MD", "ML", "KE", "WA", "WI", "YA", "SW", "FI",
                 "UM", "EBP", "CEP", "CUP", "HUBP", "MM", 
                 "LSSP", "KLP", "RSP", "SSMP")

rural_lakes <- c("AQ", "AR", "BH", "BK", "BOT", "BY", "CA", "DS", "DY", "ER",
                 "FD", "HT", "ID", "IV", "JS", "LC", "LE", "LH", "LL", "LR",
                 "LT", "LV", "MS", "NH", "OB", "PK", "PN", "PT", "SA", "SE",
                 "SM", "SV", "TO", "UG", "WB", "WC", "WN", "WS", "BS", "BV",
                 "NT", "LJ", "TR", "BA", "EA", "BO", "BL", "BM", "LG", "ST", "HH")

#This makes an additional column called "site_group" that makes it easier to filter and plot later by site type
creeltable <- creeltable %>%
  mutate(site_group = case_when(
    lakeID %in% urban_lakes ~ "urban",
    lakeID %in% rural_lakes ~ "rural",
    TRUE ~ NA_character_))

creeltableurban <- creeltable %>%
  filter(site_group == "urban")

creeltablerural <- creeltable %>%
  filter(site_group == "rural")

#removing any NA's in the homezip data, making another df so I don't mess with the raw rural data
ruralmap <- creeltablerural %>%
  filter(!is.na(homeZip)) %>%
  mutate(homeZip = as.character(homeZip))

zip_locs <- reverse_zipcode(ruralmap$homeZip) %>%
  filter(!is.na(lat)) %>%
  rename(home_lat = lat, home_lon = lng)

ruralmap_geo <- ruralmap %>%
  left_join(zip_locs, by = c("homeZip" = "zipcode")) %>%
  filter(!is.na(home_lat) & !is.na(home_lon))

#loading in my lake data, which is alread in lat long
rural_lakes_sf <- read.csv("lakeTable.csv") %>%
  filter(lakeID %in% rural_lakes) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#this code sets a density threshold, so showing 95% densest area
kde_rural <- with(ruralmap_geo, kde2d(home_lon, home_lat, n = 900))
kde_df_rural <- expand.grid(lon = kde_rural$x, lat = kde_rural$y)
kde_df_rural$density <- as.vector(kde_rural$z)

# Filter for densest 95%
density_threshold <- quantile(kde_df_rural$density, probs = 0.05)
kde_df_rural_95 <- kde_df_rural %>%
  filter(density >= density_threshold)

# Base map of WI and neighboring states taken from the rnaturalearth package

states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name %in% c("Wisconsin", "Illinois", "Minnesota", "Michigan"))

#map limits
xlims <- c(-93.5, -83.5)
ylims <- c(41.2, 47.5)

#finally we can make our map. I think its because I am essentially layering to maps or grids on top of eachother, but I cannot for the life of me figure out how to get the main figure white and not grey. I asked chat GPT to help and it was less than helpful in resolving that, this includes some of the code that I integrated from it

ruraldensitymap <- ggplot() +
  geom_sf(data = states, fill = "white", color = "black", size = 0.4) +
  geom_tile(
    data = kde_df_rural_95,
    aes(x = lon, y = lat, fill = density),
    alpha = 0.6
  ) +
  geom_sf(data = rural_lakes_sf, color = "blue", size = 0.3) +
  scale_fill_viridis(option = "magma", name = "Angler Density") +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  labs(title = "Rural Angler Density Map") +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  )

ruraldensitymap
#Now I can save my map
ggsave(
  "rural_density_map.png",
  ruraldensitymap,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

#here is the Urban angler Density Map 
# 1. Clean urban angler home ZIPs
urbanmap <- creeltableurban %>%
  filter(!is.na(homeZip)) %>%
  mutate(homeZip = as.character(homeZip))

# 2. Convert ZIPs to lat/lon
zip_locs_urban <- reverse_zipcode(urbanmap$homeZip) %>%
  filter(!is.na(lat)) %>%
  rename(home_lat = lat, home_lon = lng)

# 3. Join coordinates back to urban angler data
urbanmap_geo <- urbanmap %>%
  left_join(zip_locs_urban, by = c("homeZip" = "zipcode")) %>%
  filter(!is.na(home_lat) & !is.na(home_lon))

# 4. Load urban lakes spatial data (from lakeTable.csv)
urban_lakes_sf <- read.csv("lakeTable.csv") %>%
  filter(lakeID %in% urban_lakes) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# 5. Compute 2D KDE for urban angler home locations
kde_urban <- with(urbanmap_geo, kde2d(home_lon, home_lat, n = 900))
kde_df_urban <- expand.grid(lon = kde_urban$x, lat = kde_urban$y)
kde_df_urban$density <- as.vector(kde_urban$z)

# 6. Filter for densest 95%
density_threshold_urban <- quantile(kde_df_urban$density, probs = 0.05)
kde_df_urban_95 <- kde_df_urban %>%
  filter(density >= density_threshold_urban)

# 7. Base map of WI + neighboring states
states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name %in% c("Wisconsin", "Illinois", "Minnesota", "Michigan"))

xlims <- c(-93.5, -83.5)
ylims <- c(41.2, 47.5)

# 8. Plot urban angler density map
urbandensitymap <- ggplot() +
  geom_sf(data = states, fill = "white", color = "black", size = 0.4) +
  geom_tile(
    data = kde_df_urban_95,
    aes(x = lon, y = lat, fill = density),
    alpha = 0.6
  ) +
  geom_sf(data = urban_lakes_sf, color = "red", size = 0.5) +
  scale_fill_viridis(option = "magma", name = "Angler Density") +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  labs(title = "Urban Angler Density Map") +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  )

# 9. Save the map
ggsave(
  "urban_density_map.png",
  urbandensitymap,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)


#since that wrecked my spirit, here is an easy study site map of my urban sites.

urban_lakes_sf <- read.csv("lakeTable.csv") %>%
  filter(lakeID %in% urban_lakes) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

wisconsin <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Wisconsin")

#alas here is the map
urban_sites_map <- ggplot() +
  geom_sf(data = wisconsin, fill = "white", color = "black", size = 0.4) +
  geom_sf(data = urban_lakes_sf, color = "red", size = 1) +
  labs(title = "Urban Angler Creel Study Sites") +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  )

# Display the map
urban_sites_map

# Add site type to lakes
lake_sites_sf <- read.csv("lakeTable.csv") %>%
       mutate(site_group = case_when(
             lakeID %in% urban_lakes ~ "Urban",
             lakeID %in% rural_lakes ~ "Rural",
             TRUE ~ NA_character_
       )) %>%
       filter(!is.na(site_group)) %>%   # keep only urban/rural
      st_as_sf(coords = c("long", "lat"), crs = 4326)
 
# Count urban vs rural lakes
   site_counts <- lake_sites_sf %>%
       st_set_geometry(NULL) %>%  # remove geometry to count
       group_by(site_group) %>%
       summarize(count = n())
 
   site_counts_text <- paste0(site_counts$site_group, ": ", site_counts$count, collapse = " | ")
 
   # Wisconsin outline
   wisconsin <- ne_states(country = "United States of America", returnclass = "sf") %>%
       filter(name == "Wisconsin")
 
   # Map
   study_sites_map <- ggplot() +
       geom_sf(data = wisconsin, fill = "white", color = "black", size = 0.8) +
       geom_sf(data = lake_sites_sf, aes(color = site_group), size = 1.5) +
       scale_color_manual(values = c("Urban" = "blue", "Rural" = "green"), name = "Site Type") +
       labs(
             title = "Creel Survey Sample Sites",
            subtitle = paste0("Site counts — ", site_counts_text)
         ) +
       theme_void(base_size = 12) +
       theme(
             plot.background = element_rect(fill = "white", color = NA),
             panel.background = element_rect(fill = "white", color = NA),
             plot.title = element_text(hjust = 0.5, face = "bold"),
             plot.subtitle = element_text(hjust = 0.5),
             legend.position = "bottom"
         )
 
   study_sites_map

   
