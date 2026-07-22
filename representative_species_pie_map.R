# ============================================================
# Mosquito Species Composition Map for Connecticut
# Five 5-year period maps, each with pie charts per site
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(tigris)
library(readxl)
library(scatterpie)
library(patchwork)
library(cowplot)
library(grid)

setwd("/Users/macbook/Desktop/mosquito/data")
options(tigris_use_cache = TRUE)

# ----------------------------------------------------------
# 0. Projection setting
# ----------------------------------------------------------
# Use a projected CRS in meters so scatterpie circles remain circular
map_crs <- 26918   # NAD83 / UTM zone 18N

# ----------------------------------------------------------
# 1. Load Connecticut base map layers
# ----------------------------------------------------------
ct_towns <- county_subdivisions(
  state = "CT", year = 2024, cb = TRUE, class = "sf"
) %>%
  st_make_valid() %>%
  st_transform(map_crs)

ct_counties <- counties(
  state = "CT", year = 2020, cb = TRUE, class = "sf"
) %>%
  st_make_valid() %>%
  st_transform(map_crs)

# ----------------------------------------------------------
# 2. Load and combine species data from all four sheets
# ----------------------------------------------------------
sheets <- c(
  "Aedes albopictus 2001-2025",
  "Culex erraticus 2001-2025",
  "Culex pipiens 2001-2025",
  "Culiseta melanura 2001-2025"
)

mosq_raw <- map_dfr(
  sheets,
  ~ read_excel("species.xlsx", sheet = .x)
)

# Rename columns
colnames(mosq_raw) <- c(
  "species", "site", "town", "county",
  "trap_type", "date", "cdc_week",
  "accession", "n_mosquitoes", "virus", "comments"
)

mosq <- mosq_raw %>%
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    species = str_trim(species),
    site = str_trim(site)
  ) %>%
  filter(
    !is.na(year),
    year >= 2001,
    year <= 2025,
    !is.na(site),
    !is.na(species)
  )

# ----------------------------------------------------------
# 3. Load trap site coordinates from trap_sites.xlsx (Sheet2)
# ----------------------------------------------------------
trap_raw <- read_excel("trap_sites.xlsx", sheet = "Sheet2")

colnames(trap_raw) <- c(
  "town", "trap_location", "site_code",
  "lat_raw", "lon_raw", "years_active"
)

# Parse latitude: e.g. "41 35.4" -> 41 + 35.4/60
parse_lat <- function(x) {
  x <- str_trim(as.character(x))
  m <- str_match(x, "(\\d+)[^0-9]+([0-9.]+)")
  if (is.na(m[1])) return(NA_real_)
  as.numeric(m[2]) + as.numeric(m[3]) / 60
}

# Parse longitude and force western hemisphere negative
parse_lon <- function(x) {
  x <- str_trim(as.character(x))
  m <- str_match(x, "(\\d+)[^0-9]+([0-9.]+)")
  if (is.na(m[1])) return(NA_real_)
  -(as.numeric(m[2]) + as.numeric(m[3]) / 60)
}

# Keep raw lat/lon for checking, then project to map_crs and extract x/y in meters
site_coords_sf <- trap_raw %>%
  mutate(
    site = str_trim(site_code),
    lat  = map_dbl(lat_raw, parse_lat),
    lon  = map_dbl(lon_raw, parse_lon)
  ) %>%
  filter(
    !is.na(site),
    !is.na(lat),
    !is.na(lon)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(map_crs)

site_coords <- site_coords_sf %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(site, lat, lon, x, y)

# ----------------------------------------------------------
# 4. Define 5-year periods
# ----------------------------------------------------------
periods <- tibble(
  period_start = seq(2001, 2021, by = 5),
  period_end   = c(2005, 2010, 2015, 2020, 2025),
  label        = c("2001–2005", "2006–2010", "2011–2015", "2016–2020", "2021–2025")
)

species_list <- c(
  "Culex pipiens",
  "Culiseta melanura",
  "Aedes albopictus",
  "Culex erraticus"
)

species_colors <- c(
  "Aedes albopictus"  = "#C23B3B",
  "Culex erraticus"   = "#E08A2E",
  "Culex pipiens"     = "#3A9D5C",
  "Culiseta melanura" = "#3C6EB4"
)

# ----------------------------------------------------------
# 5. Get CT map bounding box for consistent axis limits
# ----------------------------------------------------------
ct_bbox <- st_bbox(ct_counties)

# Units are meters now
x_pad <- 5000
y_pad <- 5000

x_range <- c(ct_bbox["xmin"] - x_pad, ct_bbox["xmax"] + x_pad)
y_range <- c(ct_bbox["ymin"] - y_pad, ct_bbox["ymax"] + y_pad)

# ----------------------------------------------------------
# 6. Determine global max total per site for pie size scaling
# ----------------------------------------------------------
global_max <- mosq %>%
  group_by(site) %>%
  summarise(total = sum(n_mosquitoes, na.rm = TRUE), .groups = "drop") %>%
  summarise(max_total = max(total, na.rm = TRUE)) %>%
  pull(max_total)

# Max pie radius in meters
# Tune this if pies are too large or too small
max_radius <- 12000

# ----------------------------------------------------------
# 7. Base theme
# ----------------------------------------------------------
base_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title       = element_blank(),
    axis.text        = element_text(size = 7, color = "grey30"),
    panel.grid.major = element_line(linewidth = 0.15, color = "grey88"),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

# ----------------------------------------------------------
# 8. Function to build one period map
# ----------------------------------------------------------
make_period_map <- function(ps, pe, lab) {
  
  # Summarise counts by site and species for this period
  period_data <- mosq %>%
    filter(year >= ps, year <= pe) %>%
    group_by(site, species) %>%
    summarise(count = sum(n_mosquitoes, na.rm = TRUE), .groups = "drop")
  
  # Convert to wide format and ensure all species columns exist
  period_wide <- period_data %>%
    pivot_wider(
      names_from = species,
      values_from = count,
      values_fill = 0
    )
  
  for (sp in species_list) {
    if (!sp %in% names(period_wide)) {
      period_wide[[sp]] <- 0
    }
  }
  
  period_wide <- period_wide %>%
    mutate(
      total = rowSums(across(all_of(species_list)), na.rm = TRUE)
    ) %>%
    filter(total > 0)
  
  # Join projected coordinates
  pie_data <- period_wide %>%
    inner_join(site_coords, by = "site") %>%
    mutate(
      radius = max_radius * sqrt(total / global_max)
    )
  
  ggplot() +
    geom_sf(
      data = ct_towns,
      fill = "#f0f4f8",
      color = "#c8d6e0",
      linewidth = 0.15
    ) +
    geom_sf(
      data = ct_counties,
      fill = NA,
      color = "#4a6fa5",
      linewidth = 0.55
    ) +
    geom_scatterpie(
      data = pie_data,
      aes(x = x, y = y, r = radius),
      cols = species_list,
      color = NA,
      alpha = 0.72
    ) +
    scale_fill_manual(
      values = species_colors,
      name   = "Species",
      labels = species_list
    ) +
    coord_sf(
      xlim = x_range,
      ylim = y_range,
      expand = FALSE
    ) +
    labs(title = lab) +
    base_theme
}

# Generate all five maps
plots <- pmap(periods, ~ make_period_map(..1, ..2, ..3))
names(plots) <- periods$label

# ----------------------------------------------------------
# 9. Build a standalone legend panel
# ----------------------------------------------------------
legend_data <- tibble(
  x = rep(1, length(species_list)),
  y = seq_along(species_list),
  species = factor(species_list, levels = species_list)
)

legend_plot <- ggplot(legend_data, aes(x = x, y = y, fill = species)) +
  geom_point(shape = 21, size = 7, color = NA) +
  scale_fill_manual(
    values = species_colors,
    name   = "Species",
    labels = species_list
  ) +
  theme_void() +
  theme(
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 14),
    legend.text      = element_text(size = 11, face = "italic"),
    legend.key.size  = unit(1.2, "lines"),
    legend.spacing.y = unit(0.35, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 7)))

legend_grob  <- cowplot::get_legend(legend_plot)
legend_panel <- ggdraw(legend_grob)

# ----------------------------------------------------------
# 10. Compose final layout
# ----------------------------------------------------------
final <- (
  (plots[[1]] | plots[[2]] | plots[[3]]) /
    (plots[[4]] | plots[[5]] | legend_panel)
) +
  plot_annotation(
    # title   = "Mosquito Species Composition by Trapping Site in Connecticut",
    # caption = "Pie size reflects total mosquitoes collected. Source: CAES",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 22, hjust = 0.5),
      plot.caption = element_text(size = 12, color = "grey55")
    )
  )

# ----------------------------------------------------------
# 11. Save
# ----------------------------------------------------------
final

# ggsave(
#   "/cloud/project/map/species_pie_map.png",
#   plot   = final,
#   width  = 20,
#   height = 10,
#   dpi    = 300,
#   bg     = "white"
# )
# 
# message("Map saved to /cloud/project/map/species_pie_map.png")