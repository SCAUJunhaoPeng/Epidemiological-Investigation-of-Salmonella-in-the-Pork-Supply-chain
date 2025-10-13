# Global Salmonella map + city points

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(readxl)
library(raster)
library(stringr)
library(Cairo)

# Paths
world_raster_path <- "HYP_50M_SR_W.tif"
country_data_path <- "Country_data.xlsx"
city_data_path    <- "City_data.xlsx"
output_path       <- "Fig1_Global_Salmonella_Map.pdf"

# Base map (countries) and raster background
world_map <- ne_countries(scale = "medium", returnclass = "sf")
bg_raster <- raster::brick(world_raster_path)

# Country-level counts
counts_raw <- read_excel(country_data_path)

# Robust degree parser: accepts "12.3°N" / "12.3N" / "12.3" (assumes N/E if hemisphere missing)
parse_deg <- function(x, hemi = c("NS", "EW")) {
  x <- as.character(x)
  hpat <- if (hemi[1] == "NS") "[NSns]" else "[EWew]"
  h <- toupper(str_extract(x, paste0(hpat, "$")))
  v <- suppressWarnings(as.numeric(str_replace_all(x, "[^0-9.]", "")))
  v <- ifelse(is.na(v), NA_real_, v)
  ifelse(h %in% c("S", "W"), -v, v)
}

# Expect columns: Country, Counts, lat, lon  (lat/lon may include N/S/E/W)
counts <- counts_raw %>%
  mutate(
    lat = parse_deg(lat, "NS"),
    lon = parse_deg(lon, "EW")
  )

# Join counts to map by normalized name
normalize_country <- function(x) tolower(str_trim(x))
world_map <- world_map %>%
  mutate(key = normalize_country(name_long))
counts <- counts %>%
  mutate(key = normalize_country(Country))

world_map <- world_map %>%
  left_join(counts %>% select(key, Counts), by = "key")

world_map$Counts[is.na(world_map$Counts)] <- 0

# Bins for choropleth
world_map <- world_map %>%
  mutate(Counts_fill = cut(
    Counts,
    breaks = c(-1, 0, 50, 100, 500, 1000, 5000, Inf),
    labels = c(NA, "1-50", "51-100", "101-500", "501-1000", "1001-5000", ">5000")
  ))

# Label positions (use point on surface to avoid ocean centroids)
world_map_labels <- sf::st_point_on_surface(world_map) %>%
  mutate(Counts_label = ifelse(Counts > 0, as.character(Counts), NA))

# City-level points
cities <- read_excel(city_data_path)
# Expect columns: Lon, Lat, Salmonella_counts
cities_sf <- st_as_sf(cities, coords = c("Lon", "Lat"), crs = 4326)

# Plot
CairoPDF(output_path, width = 12, height = 8)
p <- ggplot() +
  ggspatial::layer_spatial(bg_raster, alpha = 0.6) +
  geom_sf(data = world_map, aes(fill = Counts_fill), color = "white", size = 0.4) +
  geom_sf(data = cities_sf, aes(size = Salmonella_counts),
          color = "#4E79A7", alpha = 0.35, show.legend = TRUE) +
  scale_size_area(name = "Salmonella counts", max_size = 10) +
  scale_fill_manual(
    values = c(
      "1-50" = "#FFDADA", "51-100" = "#FFB3B3", "101-500" = "#FF8C8C",
      "501-1000" = "#FF6666", "1001-5000" = "#E04C4C", ">5000" = "#B33636"
    ),
    na.value = "#FFFFFF", drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin") +
  annotation_north_arrow(
    location = "tl", which_north = TRUE,
    pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(x = NULL, y = NULL, fill = "Salmonella counts") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "white")
  )

print(p)
dev.off()
