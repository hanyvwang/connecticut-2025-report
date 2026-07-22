library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(tigris)

setwd("path to the folder")
options(tigris_use_cache = TRUE)

tick <- read.csv("tick data towns.csv", stringsAsFactors = FALSE) %>%
  mutate(year = as.integer(year),
         town_clean = str_trim(str_remove(town, "\\s*\\(.*\\)$")),
         town_clean = str_to_upper(town_clean),
         t_tested_num = suppressWarnings(as.numeric(t_tested)),
         percent_positive = if_else(percent_positive > 100, 1, percent_positive)) %>%
  filter(year >= 1996, year <= 2025)

ct_towns <- county_subdivisions(state = "CT", year = 2024, cb = TRUE, class = "sf") %>%
  st_make_valid() %>%
  mutate(town_clean = stringr::str_to_upper(NAME)) %>%
  st_transform(4326)

ct_counties <- tigris::counties(state = "CT", year = 2020, cb = TRUE, class = "sf") %>%
  st_make_valid() %>%
  st_transform(4326)

clean_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.title = element_text(face = "bold"))

fill_cols1 <- c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c")
fill_cols2 <- c("#fff5f0", "#fcbba1", "#fb6a4a", "#cb181d", "#67000d")

tick <- tick %>%
  mutate(period_start = 1996 + 5 * ((year - 1996) %/% 5),
         period_end = pmin(period_start + 4, 2025),
         period_lab = paste0(period_start, "–", period_end))

periods <- tick %>%
  distinct(period_start, period_end, period_lab) %>%
  arrange(period_start)

for (i in seq_len(nrow(periods))) {
  ps <- periods$period_start[i]
  pe <- periods$period_end[i]
  lab <- periods$period_lab[i]
  
  tick_p <- tick %>%
    filter(year >= ps, year <= pe)
  
  sum_submitted <- tick_p %>%
    group_by(town_clean) %>%
    summarise(submitted = sum(t_submitted, na.rm = TRUE), .groups = "drop")
  
  sum_positive <- tick_p %>%
    group_by(town_clean) %>%
    summarise(pos_rate = mean(percent_positive, na.rm = TRUE) / 100, .groups = "drop")
  
  map_submitted <- ct_towns %>%
    left_join(sum_submitted, by = "town_clean")
  
  map_positive <- ct_towns %>%
    left_join(sum_positive, by = "town_clean")
  
  unmatched <- anti_join(sum_submitted, ct_towns %>% st_drop_geometry() %>% select(town_clean), by = "town_clean")
  
  p1 <- ggplot() +
    geom_sf(data = map_submitted, aes(fill = submitted), color = NA) +
    geom_sf(data = ct_counties, fill = NA, color = "black", linewidth = 0.5) +
    coord_sf(expand = FALSE) +
    scale_fill_gradientn(colours = fill_cols1,
                         limits = c(0, 4000),
                         na.value = "grey",
                         labels = comma,
                         name = "Ticks submitted") +
    labs(title = paste0("Total Deer Ticks Submitted by Town (", lab, ")"),
         x = "Longitude (°W)",
         y = "Latitude (°N)") +
    clean_theme
  
  ggsave(filename = paste0("T_town_ticks_submitted_", gsub("–", "-", lab), ".png"),
         plot = p1, width = 10, height = 8, dpi = 300)
  
  p2 <- ggplot() +
    geom_sf(data = map_positive, aes(fill = pos_rate), color = NA) +
    geom_sf(data = ct_counties, fill = NA, color = "black", linewidth = 0.5) +
    coord_sf(expand = FALSE) +
    scale_fill_gradientn(colours = fill_cols2,
                         limits = c(0, 1),
                         na.value = "grey",
                         labels = percent_format(accuracy = 1),
                         name = "Positive rate") +
    labs(title = paste0("Overall Tick Pathogen Positivity Rate by Town (", lab, ")"),
         x = "Longitude (°W)",
         y = "Latitude (°N)") +
    clean_theme
  
  ggsave(filename = paste0("CT_town_ticks_positive_rate_", gsub("–", "-", lab), ".png"),
         plot = p2, width = 10, height = 8, dpi = 300)
}