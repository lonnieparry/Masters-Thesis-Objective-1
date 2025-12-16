##ALTERNATE CODE TO MAKE MAP WITH UD AND ELLIPSES/STRAIGHT LINE DISTANCE

# - UD shading grayscale, shown only within 95% UD boundary
# - Ellipse footprint = 97% (after trimming farthest 5% distances)
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

mode_colors <- c("Boat" = "#4B8BC8", "Shore" = "#3BAA36")

xlims <- c(-93.5, -83.5)
ylims <- c(41.2, 47.5)

# UD / KDE
ud_levels <- c(0.50, 0.75, 0.90, 0.95)
ud_fill_max <- 0.95          # UD fill shown only inside this boundary
kde_n <- 300
kde_h <- c(0.65, 0.65)

# Ellipse footprint
ellipse_level <- 0.99         # <-- requested


creel <- read.csv("cleanedcreeltable.csv")
lakeTable <- read.csv("lakeTable.csv")

# Urban only + ZIP centroids
urban <- creel %>%
  dplyr::filter(
    Region %in% c("Great Lakes", "Inland"),
    FishingMode %in% c("Boat", "Shore"),
    !is.na(homeZip)
  ) %>%
  dplyr::mutate(homeZip = substr(as.character(homeZip), 1, 5)) %>%
  dplyr::filter(nchar(homeZip) == 5)

zip_coords <- zipcodeR::zip_code_db %>%
  dplyr::transmute(homeZip = zipcode, home_lat = lat, home_lon = lng)

urban <- urban %>%
  dplyr::left_join(zip_coords, by = "homeZip") %>%
  dplyr::filter(!is.na(home_lat), !is.na(home_lon))

# Sample sites from lakeTable + join
lake_region <- urban %>%
  dplyr::select(lakeID, Region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(lakeID = as.character(lakeID))

sites <- lakeTable %>%
  dplyr::transmute(
    lakeID = as.character(lakeID),
    site_lat = lat,
    site_lon = long
  ) %>%
  dplyr::filter(!is.na(site_lat), !is.na(site_lon)) %>%
  dplyr::left_join(lake_region, by = "lakeID") %>%
  dplyr::filter(Region %in% c("Great Lakes", "Inland")) %>%
  dplyr::distinct(lakeID, Region, site_lat, site_lon)

urban <- urban %>%
  dplyr::mutate(lakeID = as.character(lakeID)) %>%
  dplyr::left_join(sites %>% dplyr::select(lakeID, site_lat, site_lon), by = "lakeID") %>%
  dplyr::filter(!is.na(site_lat), !is.na(site_lon))

# Straight-line distance km
homes_sf <- urban %>%
  sf::st_as_sf(coords = c("home_lon", "home_lat"), crs = 4326) %>%
  sf::st_transform(3071)

sites_sf <- urban %>%
  sf::st_as_sf(coords = c("site_lon", "site_lat"), crs = 4326) %>%
  sf::st_transform(3071)

urban$distance_km <- as.numeric(sf::st_distance(homes_sf, sites_sf, by_element = TRUE)) / 1000


# Trim farthest 5% 
urban_map <- urban %>%
  dplyr::group_by(Region, FishingMode) %>%
  dplyr::mutate(d95 = quantile(distance_km, 0.95, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(distance_km <= d95)

#CUMULATIVE UD 

make_ud_df <- function(df) {
  if (nrow(df) < 10) return(NULL)
  
  kde <- with(df, MASS::kde2d(
    home_lon, home_lat,
    n = kde_n, h = kde_h,
    lims = c(xlims[1], xlims[2], ylims[1], ylims[2])))
  
  kde_df <- expand.grid(lon = kde$x, lat = kde$y)
  kde_df$density <- as.vector(kde$z)
  
  kde_df <- kde_df %>%
    dplyr::mutate(density = ifelse(is.na(density) | density < 0, 0, density)) %>%
    dplyr::arrange(dplyr::desc(density)) %>%
    dplyr::mutate(cum = cumsum(density) / sum(density)) %>%
    dplyr::arrange(lon, lat) %>%
    dplyr::mutate(cum_fill = ifelse(cum <= ud_fill_max, cum, NA_real_))
  
  kde_df
}

# Ellipse polygon (closed sf polygon) at specified level
make_ellipse_sf <- function(df, level = 0.99) {
  if (nrow(df) < 10) return(NULL)
  
  pts <- sf::st_as_sf(df, coords = c("home_lon", "home_lat"), crs = 4326) %>%
    sf::st_transform(3071)
  XY <- sf::st_coordinates(pts)
  if (nrow(unique(XY)) < 5) return(NULL)
  
  mu <- colMeans(XY)
  S  <- stats::cov(XY)
  if (any(!is.finite(S)) || any(is.na(S))) return(NULL)
  
  A <- tryCatch(chol(S), error = function(e) NULL)
  if (is.null(A)) return(NULL)
  
  r <- sqrt(stats::qchisq(level, df = 2))
  theta <- seq(0, 2*pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  ell <- t(mu + r * t(circle %*% A))
  ell <- rbind(ell, ell[1, ])  # close polygon
  
  ell_sf <- sf::st_sfc(sf::st_polygon(list(ell)), crs = 3071) %>%
    sf::st_transform(4326)
  
  sf::st_sf(level = level, geometry = ell_sf)
}

#build plot data for the fishing groups
groups <- urban_map %>%
  dplyr::mutate(Group = paste(Region, FishingMode, sep = " — "))

ud_list <- lapply(split(groups, groups$Group), make_ud_df)
ud_df <- dplyr::bind_rows(lapply(names(ud_list), function(g) {
  x <- ud_list[[g]]
  if (is.null(x)) return(NULL)
  x$Group <- g
  x
}))

ellipse_list <- lapply(split(groups, groups$Group), make_ellipse_sf, level = ellipse_level)
ellipse_sf <- dplyr::bind_rows(lapply(names(ellipse_list), function(g) {
  x <- ellipse_list[[g]]
  if (is.null(x)) return(NULL)
  x$Group <- g
  x
}))

ud_df <- ud_df %>%
  tidyr::separate(Group, into = c("Region", "FishingMode"), sep = " — ", remove = FALSE)
ellipse_sf <- ellipse_sf %>%
  tidyr::separate(Group, into = c("Region", "FishingMode"), sep = " — ", remove = FALSE)


# Base map

states <- rnaturalearth::ne_states(
  country = "United States of America",
  returnclass = "sf"
) %>%
  dplyr::filter(name %in% c("Wisconsin", "Illinois", "Minnesota", "Michigan"))

#single panel UD plot

make_ud_panel <- function(region_val, mode_val) {
  df_ud <- ud_df %>% dplyr::filter(Region == region_val, FishingMode == mode_val)
  df_el <- ellipse_sf %>% dplyr::filter(Region == region_val, FishingMode == mode_val)
  
  ggplot() +
    geom_sf(data = states, fill = "white", color = "gray50", linewidth = 0.35) +
    geom_raster(
      data = df_ud,
      aes(x = lon, y = lat, fill = cum_fill),
      interpolate = TRUE,
      alpha = 0.75
    ) +
    geom_contour(
      data = df_ud,
      aes(x = lon, y = lat, z = cum),
      breaks = ud_levels,
      color = "black",
      linewidth = 0.35
    ) +
    scale_fill_gradient(
      low = "white",
      high = "black",
      name = "Cumulative UD",
      labels = scales::percent,
      na.value = "white"
    ) +
    geom_sf(
      data = df_el,
      aes(color = FishingMode),
      fill = NA,
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    scale_color_manual(values = mode_colors) +
    geom_point(
      data = sites %>% dplyr::filter(Region == region_val),
      aes(x = site_lon, y = site_lat),
      shape = 21, fill = "white", color = "black",
      size = 2, stroke = 0.6
    ) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    labs(title = paste(region_val, "—", mode_val)) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      legend.position = "none"
    )
}

# Create the four panels
p_gl_boat  <- make_ud_panel("Great Lakes", "Boat")
p_gl_shore <- make_ud_panel("Great Lakes", "Shore")
p_in_boat  <- make_ud_panel("Inland", "Boat")
p_in_shore <- make_ud_panel("Inland", "Shore")

# ----------------------------
# Population panel (WI county population)
# ----------------------------
options(tigris_use_cache = TRUE)

wi_counties <- tigris::counties(state = "WI", cb = TRUE, year = 2022, class = "sf")

wi_pop <- tidycensus::get_acs(
  geography = "county",
  state = "WI",
  variables = "B01003_001",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
) %>%
  dplyr::select(GEOID, pop = estimate)

wi_pop_sf <- wi_counties %>%
  dplyr::left_join(wi_pop, by = "GEOID")

pop_panel <- ggplot(wi_pop_sf) +
  geom_sf(aes(fill = pop), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Population", option = "plasma") +
  labs(title = "Wisconsin County Total Population") +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# ----------------------------
# WIDE LAYOUT:
# Row 1: Great Lakes boat | Great Lakes shore
# Row 2: Inland boat      | Inland shore      | Population map
# ----------------------------
top_row <- p_gl_boat | p_gl_shore
bottom_row <- p_in_boat | p_in_shore | pop_panel

final_fig <- (top_row / bottom_row) +
  plot_layout(heights = c(1, 1.05), widths = c(1, 1, 1.1)) +
  plot_annotation(
    title = "Urban Angler Origins: Utilization Distributions + Access Footprints",
    subtitle = "UD fill shown only within 95% UD boundary; ellipse = 99% footprint (after trimming farthest 1%); points = sample sites."
  ) &
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

final_fig

ggsave("urban_ud_gray_ellipse97_wide_with_population.png",
       final_fig, width = 16, height = 9, dpi = 300, bg = "white")


##RUNNING ASSUMPTIONS 
pdf("Distance_AssumptionChecks.pdf", width = 11, height = 8.5)
par(mfrow = c(2,2))
plot(dist_mod)
shapiro.test(residuals(dist_mod))  # note sensitivity in text
dev.off()

##IF WE WANT A GRAPH JUST SHOWING BARE BONES DIFFERENCES
ggplot(urban, aes(x = distance_km, fill = FishingMode)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Region) +
  scale_fill_manual(values = c("Boat"="#4B8BC8","Shore"="#3BAA36")) +
  labs(
    x = "Straight-line distance to fishing site (km)",
    y = "Density",
    fill = "Fishing Mode"
  ) +
  theme_minimal(base_size = 14)
