# ============================================================
# Mosquito Species Abundance + Total — Faceted Plot
# Upper panel : Total line/area for all species
# Lower panel : Stacked bars for 4 focal species

# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)   # install.packages("patchwork") if needed

# ── 1. Paths ────────────────────────────────────────────────
setwd("/Users/macbook/Desktop/data")
species_path <- "representative_species.xlsx"
total_path   <- "total_mosquito.xlsx"

# ── 2. 4-species stacked data (same as original script) ─────
sheets <- excel_sheets(species_path)

combined <- lapply(sheets, function(sh) {
  df <- read_excel(species_path, sheet = sh)
  df %>%
    mutate(Year = as.integer(format(as.Date(Date), "%Y"))) %>%
    group_by(Year, Species) %>%
    summarise(Count = sum(`# Mosquitoes`, na.rm = TRUE), .groups = "drop")
}) %>%
  bind_rows()

plot_data <- combined %>%
  complete(Year, Species, fill = list(Count = 0)) %>%
  mutate(Species = factor(Species, levels = c(
    "Culex pipiens",
    "Culiseta melanura",
    "Aedes albopictus",
    "Culex erraticus"
  )))

# ── 3. Yearly total (all species) ───────────────────────────
total_wide <- read_excel(total_path, sheet = "Cumulative Mosquitoes per year")

total_data <- total_wide %>%
  filter(Species == "Yearly TOTAL") %>%
  select(-Species, -`Total Of # Mosquitoes`) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "Total") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Total))

# ── 4. Colour palette ────────────────────────────────────────
species_colors <- c(
  "Aedes albopictus"  = "#C23B3B",
  "Culex erraticus"   = "#E08A2E",
  "Culex pipiens"     = "#3A9D5C",
  "Culiseta melanura" = "#3C6EB4"
)

# Shared x-axis breaks (every year as character for discrete scale)
all_years <- sort(unique(plot_data$Year))

# ── 5. Upper panel: 4-species stacked area + total line ──────
p_top <- ggplot() +
  # Stacked area for 4 species
  geom_area(
    data = plot_data,
    aes(x = factor(Year), y = Count, fill = Species, group = Species),
    position = "stack", alpha = 0.85, colour = "white", linewidth = 0.2
  ) +
  # Total line overlay
  geom_line(
    data = total_data,
    aes(x = factor(Year), y = Total, group = 1, colour = "All species total"),
    linewidth = 0.9
  ) +
  geom_point(
    data = total_data,
    aes(x = factor(Year), y = Total, group = 1, colour = "All species total"),
    size = 1.5, show.legend = FALSE
  ) +
  scale_fill_manual(values = species_colors, guide = "none") +
  scale_colour_manual(
    values = c("All species total" = "#222222"),
    name   = NULL
  ) +
  scale_y_continuous(
    name   = "Mosquitoes collected\n(all species total)",
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.06))
  ) +
  scale_x_discrete(
    breaks = as.character(all_years),
    labels = as.character(all_years)
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.title.x       = element_blank(),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.line.x        = element_blank(),
    axis.title.y       = element_text(size = 9, margin = margin(r = 8)),
    axis.text.y        = element_text(colour = "grey20"),
    axis.ticks         = element_line(colour = "grey60", linewidth = 0.4),
    axis.line.y        = element_line(colour = "grey60", linewidth = 0.4),
    legend.position    = "right",
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.4, "cm"),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(14, 14, 2, 14)
  )

# ── 6. Lower panel: stacked bars ─────────────────────────────
p_bot <- ggplot(plot_data, aes(x = factor(Year), y = Count, fill = Species)) +
  geom_bar(stat = "identity", width = 0.72,
           colour = "white", linewidth = 0.25) +
  scale_fill_manual(
    values = species_colors,
    guide  = guide_legend(reverse = FALSE)
  ) +
  scale_y_continuous(
    name   = "Mosquitoes collected\n(4 focal species)",
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.04))
  ) +
  scale_x_discrete(
    breaks = as.character(all_years),
    labels = as.character(all_years),
    name   = "Year",
    guide  = guide_axis(angle = 45)
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.title.x       = element_text(margin = margin(t = 8)),
    axis.title.y       = element_text(size = 9, margin = margin(r = 8)),
    axis.text          = element_text(colour = "grey20"),
    axis.ticks         = element_line(colour = "grey60", linewidth = 0.4),
    axis.line          = element_line(colour = "grey60", linewidth = 0.4),
    legend.title       = element_text(face = "bold", size = 9.5),
    legend.text        = element_text(face = "italic", size = 9),
    legend.key.size    = unit(0.45, "cm"),
    legend.position    = "right",
    legend.margin      = margin(l = 6),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(2, 14, 10, 14)
  )

# ── 7. Combine with patchwork ────────────────────────────────
p <- p_top / p_bot +
  plot_layout(heights = c(1, 1), guides = "collect") &   # equal height, shared legend
  theme(legend.position = "right")

p

# ggsave("mosquito_facet.png", plot = p,
#        width = 11, height = 7, dpi = 300, bg = "white")
