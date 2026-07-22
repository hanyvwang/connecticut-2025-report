# ============================================================
# Mosquito Combined Map — All 4 Species × 5 Periods
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(tigris)
library(readxl)
library(patchwork)
library(cowplot)
library(grid)

setwd("/Users/macbook/Desktop/mosquito/data")
options(tigris_use_cache = TRUE)

# ----------------------------------------------------------
# 1. CT base map layers
# ----------------------------------------------------------

ct_towns <- county_subdivisions(state = "CT", year = 2024,
                                cb = TRUE, class = "sf") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate(town_upper = str_to_upper(NAME))

ct_counties <- counties(state = "CT", year = 2020,
                        cb = TRUE, class = "sf") %>%
  st_make_valid() %>%
  st_transform(4326)

ct_bbox <- st_bbox(ct_counties)
x_lim   <- c(ct_bbox["xmin"] - 0.02, ct_bbox["xmax"] + 0.02)
y_lim   <- c(ct_bbox["ymin"] - 0.02, ct_bbox["ymax"] + 0.02)

# ----------------------------------------------------------
# 2. Trap site coordinates
# ----------------------------------------------------------

trap_raw <- read_excel("trap_sites.xlsx", sheet = "Sheet2")
colnames(trap_raw) <- c("town", "trap_location", "site_code",
                        "lat_raw", "lon_raw", "years_active")

parse_lat <- function(x) {
  x <- str_trim(x)
  m <- str_match(x, "(\\d+)[^0-9]+([0-9.]+)")
  if (is.na(m[1])) return(NA_real_)
  as.numeric(m[2]) + as.numeric(m[3]) / 60
}
parse_lon <- function(x) {
  x <- str_trim(x)
  m <- str_match(x, "(\\d+)[^0-9]+([0-9.]+)")
  if (is.na(m[1])) return(NA_real_)
  -(as.numeric(m[2]) + as.numeric(m[3]) / 60)
}

site_coords <- trap_raw %>%
  filter(!is.na(lat_raw), !is.na(lon_raw)) %>%
  mutate(
    lat        = map_dbl(lat_raw, parse_lat),
    lon        = map_dbl(lon_raw, parse_lon),
    town_upper = str_to_upper(str_trim(town))
  ) %>%
  filter(!is.na(lat), !is.na(lon))

towns_with_sites <- unique(site_coords$town_upper)

site_active_in_period <- function(years_str, ps, pe) {
  if (is.na(years_str)) return(FALSE)
  s <- str_replace_all(years_str, "(?i)supp[^,]*,?", "")
  parts <- str_split(str_trim(s), ",")[[1]] %>%
    str_trim() %>% .[nchar(.) > 0]
  any(map_lgl(parts, function(p) {
    if (str_detect(p, "-Present$")) {
      start <- as.integer(str_extract(p, "^\\d+"))
      return(!is.na(start) && start <= pe)
    } else if (str_detect(p, "^\\d{4}-\\d{4}$")) {
      yr <- as.integer(str_split(p, "-")[[1]])
      return(yr[1] <= pe && yr[2] >= ps)
    } else if (str_detect(p, "^\\d{4}$")) {
      yr <- as.integer(p)
      return(yr >= ps && yr <= pe)
    }
    FALSE
  }))
}

# ----------------------------------------------------------
# 3. Species mosquito data
# ----------------------------------------------------------

sheets <- c("Aedes albopictus 2001-2025",
            "Culex erraticus 2001-2025",
            "Culex pipiens 2001-2025",
            "Culiseta melanura 2001-2025")

mosq_raw <- map_dfr(sheets, ~ read_excel("species.xlsx", sheet = .x))
colnames(mosq_raw) <- c("species", "site", "town", "county",
                        "trap_type", "date", "cdc_week",
                        "accession", "n_mosquitoes", "virus", "comments")

mosq <- mosq_raw %>%
  mutate(
    year       = as.integer(format(as.Date(date), "%Y")),
    species    = str_trim(species),
    town_upper = str_to_upper(str_trim(town))
  ) %>%
  filter(year >= 2001, year <= 2025)

# ----------------------------------------------------------
# 4. Settings
# ----------------------------------------------------------

species_list <- c("Culex pipiens", "Culiseta melanura",
                  "Aedes albopictus", "Culex erraticus")

species_colors <- c(
  "Aedes albopictus"  = "#C23B3B",
  "Culex erraticus"   = "#E08A2E",
  "Culex pipiens"     = "#3A9D5C",
  "Culiseta melanura" = "#3C6EB4"
)

periods <- tibble(
  ps    = seq(2001, 2021, by = 5),
  pe    = c(2005, 2010, 2015, 2020, 2025),
  label = c("2001-2005", "2006-2010", "2011-2015",
            "2016-2020", "2021-2025")
)

# ----------------------------------------------------------
# 5. Build one map panel
# ----------------------------------------------------------

make_panel <- function(sp, ps, pe, lab, sp_color, global_max) {
  
  town_counts <- mosq %>%
    filter(species == sp, year >= ps, year <= pe) %>%
    group_by(town_upper) %>%
    summarise(total = sum(n_mosquitoes, na.rm = TRUE), .groups = "drop")
  
  active_sites <- site_coords %>%
    filter(map_lgl(years_active,
                   ~ site_active_in_period(.x, ps, pe)))
  
  map_data <- ct_towns %>%
    left_join(town_counts, by = "town_upper") %>%
    mutate(
      fill_type = case_when(
        !is.na(total) & total > 0        ~ "caught",
        town_upper %in% towns_with_sites ~ "site_no_catch",
        TRUE                             ~ "no_site"
      )
    )
  
  map_caught   <- map_data %>% filter(fill_type == "caught")
  map_no_catch <- map_data %>% filter(fill_type == "site_no_catch")
  map_no_site  <- map_data %>% filter(fill_type == "no_site")
  
  ggplot() +
    geom_sf(data = map_no_site,  fill = "#606060", color = "#aaaaaa",
            linewidth = 0.1) +
    geom_sf(data = map_no_catch, fill = "#C8C8C8", color = "#aaaaaa",
            linewidth = 0.1) +
    geom_sf(data = map_caught,   aes(fill = total),
            color = "#aaaaaa", linewidth = 0.1) +
    scale_fill_gradient(
      low    = "#fff5f0",
      high   = sp_color,
      limits = c(1, global_max),
      trans  = "log10",
      guide  = "none"
    ) +
    geom_sf(data = ct_counties, fill = NA, color = "#333333",
            linewidth = 0.4) +
    geom_point(data = active_sites, aes(x = lon, y = lat),
               shape = 21, size = 1.2,
               fill = "white", color = "black", stroke = 0.35) +
    coord_sf(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    theme_void(base_size = 8) +
    theme(plot.margin = margin(2, 2, 2, 2))
}

# ----------------------------------------------------------
# 6. Build a vertical colorbar for one species
# ----------------------------------------------------------

make_colorbar <- function(sp_color, global_max) {
  dummy <- tibble(x = 1, y = 1, v = exp(mean(log(c(1, global_max)))))
  gg <- ggplot(dummy, aes(x, y, fill = v)) +
    geom_tile() +
    scale_fill_gradient(
      low    = "#fff5f0",
      high   = sp_color,
      limits = c(1, global_max),
      trans  = "log10",
      name   = "",
      labels = scales::label_comma(),
      guide  = guide_colorbar(
        barwidth       = unit(0.4, "cm"),
        barheight      = unit(3.2, "cm"),
        title.position = "top",
        title.hjust    = 0.5,
        title.theme    = element_text(size = 0),
        label.theme    = element_text(size = 6.5)
      )
    ) +
    theme_void() +
    theme(legend.position   = "right",
          legend.margin     = margin(0, 0, 0, 0),
          legend.box.margin = margin(t = -30, 0, 0, 0))
  
  ggdraw(get_legend(gg))
}

# ----------------------------------------------------------
# 7. Shared grey / site-marker legend panel (horizontal)
# ----------------------------------------------------------

grey_legend_panel <- ggplot() +
  annotate("rect",  xmin = 0,    xmax = 0.4, ymin = 0.3, ymax = 0.7,
           fill = "#C8C8C8", color = "#aaaaaa", linewidth = 0.4) +
  annotate("text",  x = 0.55, y = 0.5,
           label = "Site present, no catch",
           hjust = 0, vjust = 0.5, size = 2.6) +
  annotate("rect",  xmin = 3.0, xmax = 3.4, ymin = 0.3, ymax = 0.7,
           fill = "#606060", color = "#aaaaaa", linewidth = 0.4) +
  annotate("text",  x = 3.55, y = 0.5,
           label = "No trap site",
           hjust = 0, vjust = 0.5, size = 2.6) +
  annotate("point", x = 6.0, y = 0.5,
           shape = 21, size = 2.2,
           fill = "white", color = "black", stroke = 0.5) +
  annotate("text",  x = 6.2, y = 0.5,
           label = "Trap site location",
           hjust = 0, vjust = 0.5, size = 2.6) +
  
  xlim(-0.2, 8.5) + ylim(0, 1) + 
  theme_void() +
  theme(plot.margin = margin(4, 4, 4, 4),
        plot.background = element_rect(fill = "white", color = NA))

# ----------------------------------------------------------
# 8. Assemble one row per species
# ----------------------------------------------------------

global_maxes <- map_dbl(species_list, function(sp) {
  mosq %>%
    filter(species == sp) %>%
    group_by(town_upper) %>%
    summarise(total = sum(n_mosquitoes, na.rm = TRUE), .groups = "drop") %>%
    pull(total) %>%
    max(na.rm = TRUE)
}) %>% set_names(species_list)

row_grobs <- lapply(species_list, function(sp) {
  
  sp_color   <- species_colors[sp]
  global_max <- global_maxes[sp]
  
  panels <- pmap(periods, ~ make_panel(sp, ..1, ..2, ..3, sp_color, global_max))
  
  sp_label <- ggdraw() +
    draw_label(sp, fontface = "bold.italic", size = 9,
               x = 0.01, y = 0.5, hjust = 0, vjust = 0.5)
  
  cbar <- make_colorbar(sp_color, global_max)
  
  maps_row <- plot_grid(
    panels[[1]], panels[[2]], panels[[3]],
    panels[[4]], panels[[5]],
    cbar,
    nrow       = 1,
    rel_widths = c(1, 1, 1, 1, 1, 0.3),
    align      = "h",
    axis       = "tb"
  )
  
  plot_grid(
    sp_label,
    maps_row,
    ncol        = 1,
    rel_heights = c(0.13, 1)
  )
})

# ----------------------------------------------------------
# 9. Stack rows + period title strip
# ----------------------------------------------------------

total_w <- 5 + 0.3
title_strip <- ggdraw() +
  draw_label("2001-2005", x = 0.5  / total_w, y = 0.5,
             hjust = 0.5, size = 10, fontface = "bold") +
  draw_label("2006-2010", x = 1.5  / total_w, y = 0.5,
             hjust = 0.5, size = 10, fontface = "bold") +
  draw_label("2011-2015", x = 2.5  / total_w, y = 0.5,
             hjust = 0.5, size = 10, fontface = "bold") +
  draw_label("2016-2020", x = 3.5  / total_w, y = 0.5,
             hjust = 0.5, size = 10, fontface = "bold") +
  draw_label("2021-2025", x = 4.5  / total_w, y = 0.5,
             hjust = 0.5, size = 10, fontface = "bold") +
  draw_label("Mosquitoes\ncollected",
             x = (5 + 0.15) / total_w, y = 0.5,
             hjust = 0.5, size = 7.5, fontface = "bold", lineheight = 1.1)

main_grid <- plot_grid(NULL,
  title_strip,        NULL,
  row_grobs[[1]],     NULL,
  row_grobs[[2]],     NULL,
  row_grobs[[3]],     NULL,
  row_grobs[[4]],
  ncol        = 1,
  rel_heights = c(0.10, 0.10, 0.06, 1, 0.10, 1, 0.10, 1, 0.10, 1)
)

# ----------------------------------------------------------
# 10. Add legend at bottom center with SPACING
# ----------------------------------------------------------

legend_row <- plot_grid(
  NULL, grey_legend_panel, NULL,
  nrow       = 1,
  rel_widths = c(0.15, 0.70, 0.15) 
)

final <- plot_grid(
  main_grid,
  NULL,
  legend_row,
  ncol        = 1,
  rel_heights = c(1, 0.03, 0.06) 
)

# ----------------------------------------------------------
# 11. Save
# ----------------------------------------------------------
final