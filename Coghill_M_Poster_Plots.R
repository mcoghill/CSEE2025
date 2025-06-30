# Load libraries and Roboto Slab font styles - install from here: https://fonts.google.com/specimen/Roboto+Slab
library(tidyverse)
library(extrafont)
library(data.table)
f_check <- fonttable() %>%
  filter(FontName == "RobotoSlab-Light")
if(nrow(f_check) < 1) font_import(prompt = FALSE, pattern = "RobotoSlab")
loadfonts()

# Fire frequency by latitude plot - data available by request
fire_by_latitude_data <- read.csv("data/fire_by_latitude.csv") %>%
  filter(size_class != "small")

pretty_name <- c(large = "Large\n (> 200 ha)",
                 medium = "Medium\n (1 - 200 ha)")

fire_lat_plot <- ggplot(fire_by_latitude_data, aes(x = Y)) +
  geom_histogram(binwidth = 1, fill = "#003e51") +
  facet_grid(
    size_class ~ decade,
    labeller = labeller(size_class = pretty_name)) +
  coord_flip() +
  scale_x_continuous(labels = function(x) paste0(x, "\u00b0N"), breaks = c(20, 45, 70)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-5, 445), breaks = c(0, 200, 400))+
  theme_bw(base_size = 24, base_family = "Roboto Slab") +
  theme(
    panel.grid = element_blank(),
    panel.spacing.x = unit(5, "pt"),
    panel.spacing.y = unit(5, "pt"),
    panel.border = element_rect(color = "#003e51", linewidth = 1, fill = NA),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title = element_text(color = "#003e51", face = "bold"),
    axis.text.y = element_text(color = "#003e51"),
    axis.text.x = element_text(color = "#003e51", angle = 45, vjust = 1, hjust = 1),
    axis.ticks =  element_line(color = "#003e51"),
    strip.text.x = element_text(face = "bold", color = "#003e51"),
    strip.text.y = element_text(angle = 0, color = "#003e51"),
    strip.background.y = element_blank(),
    strip.background.x = element_rect(fill = "#9ab7c1", color = "#003e51", linewidth = 1),
    panel.spacing = unit(1.2, "lines")) +
  labs(x = "Latitude", y = "Frequency of lightning caused fires") 

ggsave("images/fig1_fire_frequency_latitude.png", fire_lat_plot, grDevices::png(), height = 5, width = 9.02, units = "in", dpi = 600)

# Log response ratio plot - data available by request
dt_lrr <- fread("data/dt_lrr.csv")
dt_lrr <- dt_lrr[!ecoregion_level1_id %in% c("TROPICAL WET FORESTS", "ARCTIC CORDILLERA")]

dt_lrr_num <- dt_lrr[, var_group := as.numeric(paste(var_group))
  ][, ecoregion_level1_id := factor(ecoregion_level1_id, levels = c(
    "ARCTIC CORDILLERA", "TUNDRA", "TAIGA", "HUDSON PLAIN", "NORTHERN FORESTS",
    "NORTHWESTERN FORESTED MOUNTAINS", "MARINE WEST COAST FOREST",
    "EASTERN TEMPERATE FORESTS", "GREAT PLAINS", "NORTH AMERICAN DESERTS",
    "MEDITERRANEAN CALIFORNIA", "SOUTHERN SEMIARID HIGHLANDS",
    "TEMPERATE SIERRAS", "TROPICAL WET FORESTS"))
  ][, signif := ecoregion_level1_id %in% c(
    "GREAT PLAINS", "SOUTHERN SEMIARID HIGHLANDS", "TROPICAL WET FORESTS")]

stability_plot <- ggplot(dt_lrr_num, aes(x = var_group, y = median_lrr_stability, linetype = signif)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), color = "#fff5de") +
  facet_wrap( ~ ecoregion_level1_id, ncol = 4, labeller = label_wrap_gen(width = 25)) +
  labs(x = "Time since fire (years)", y = "LRR stability") +
  scale_linetype_discrete(name = "signif", breaks = c("TRUE", "FALSE")) +
  ggeasy::easy_all_text_color("black") +
  theme_bw(base_size = 24, , base_family = "Roboto Slab") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    panel.spacing.x = unit(10, "pt"),
    panel.spacing.y = unit(10, "pt"),
    panel.border = element_rect(color = "#fff5de", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title = element_text(color = "#fff5de", face = "bold"),
    axis.text = element_text(color = "#fff5de"),
    axis.ticks =  element_line(color = "#fff5de"),
    strip.text.x = element_text(face = "bold", color = "#003e51"),
    strip.background.y = element_blank(),
    strip.background.x = element_rect(fill = "#9ab7c1", color = "#fff5de", linewidth = 1))

ggsave("images/fig3_stability.png", stability_plot, grDevices::png(), height = 13, width = 18.52, units = "in", dpi = 600)

# Replace the HTML template file within the posterdown package to the custom one here
file.copy("etc/template.html", file.path(find.package("posterdown"), "rmarkdown/templates/posterdown_betterland/resources/template.html"), overwrite = TRUE)

# Create PDF of the poster
pagedown::chrome_print("Coghill_M_Poster.Rmd", options = list(pageRanges = "1"))
