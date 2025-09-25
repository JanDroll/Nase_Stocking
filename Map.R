library(sf)
library(tidyverse)
library(ggthemes)
library(scico)
library(ggrepel)
library(gganimate)

antenna_geo <- tibble(area = c("Weitbach", "Attler Au", "Feldkirchen", "Gars", "Isen", "Jettenbach", "Mangfall", "Marktl", "Nasenloch",
                               "Neuoetting R", "Neuoetting L", "Perach", "Teufelsbruck", "Thal", "Vorderleiten", "Wasserburg", "Stammham"),
                      lat = c(48.261443, 48.039546, 47.937989, 48.154116, 48.265996, 48.177227, 47.845103, 48.256078, 48.250140,
                              48.247684,48.251749, 48.255754, 48.114646, 48.149465, 48.101377, 48.058129, 48.247058),
                      lon = c(12.787426, 12.194533, 12.148067, 12.310458, 12.664817, 12.378618, 12.122969, 12.837187, 12.664581,
                              12.689597, 12.691187, 12.777654, 12.272147, 12.295568, 12.237741, 12.218788, 12.860261),
                      type = c("Tributary", "Backwater", "Fish pass", "Fish pass", "Tributary", "Fish pass", "Tributary", "Backwater", "Backwater",
                               "Fish pass", "Fish pass", "Fish pass", "Fish pass", "Backwater", "Backwater", "Fish pass", "Fish pass"))

Inn_trib <- st_read("~/Documents/R_Projects/Nase_Stocking/GEO_data/Zubringer_Inn_Joku.shp", quiet = TRUE) %>% 
  filter(Name %in% c("Mangfall", "Isen"))
rivers <- st_read("~/Documents/R_Projects/Nase_Stocking/rivers_europe/ne_10m_rivers_europe.shp", quiet = TRUE) %>% 
  filter(name_de != "Salzach")
Inn <- st_read("~/Documents/R_Projects/Nase_Stocking/GEO_data/Line_Inn_and_Tributaries.shp", quiet = TRUE)
new_Inn <- st_read("~/Documents/R_Projects/Nase_Stocking/new_GEO_data/neu.shp", quiet = TRUE)
countries <- st_read("~/Documents/R_Projects/Nase_Stocking/countries/ne_10m_admin_0_countries.shp", quiet = TRUE) %>% 
  filter(NAME == "Germany")

coords <- antenna_geo %>% 
  select(site, lat, lon) %>% 
  rename(area = site)

den_det <- Detec_Nase %>% 
  group_by(area, Tag_ID, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  ungroup()
  
den_det <- left_join(den_det, coords, by = "area") %>% 
  mutate(Year = lubridate::year(date), 
         Month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>% 
  mutate(season = case_when(Month %in% c("March", "April", "May") ~ paste("Spring", Year),
                            Month %in% c("June", "July", "August") ~ paste("Summer", Year),
                            Month %in% c("September", "October", "November") ~ paste("Autumn", Year),
                            Month == "January" & Year == 2023 ~ "Winter 2022/2023",
                            Month == "February" & Year == 2023 ~ "Winter 2022/2023",
                            Month == "December" & Year == 2022 ~ "Winter 2022/2023",
                            Month == "January" & Year == 2024 ~ "Winter 2023/2024",
                            Month == "February" & Year == 2024 ~ "Winter 2023/2024",
                            Month == "December" & Year == 2023 ~ "Winter 2023/2024")) %>% 
  mutate(season = season %>% fct_relevel("Stocking", "Autumn 2022", "Winter 2022/2023",
                                       "Spring 2023", "Summer 2023", "Autumn 2023", "Winter 2023/2024",
                                       "Spring 2024", "Summer 2024"))

det_2022 <- den_det %>% 
  filter(Year == 2022)
det_2023 <- den_det %>% 
  filter(Year == 2023)
det_2024 <- den_det %>% 
  filter(Year == 2024)

n_fish_2022 <- det_2022 %>% 
  group_by(area) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(area)
n_fish_2022 <- left_join(n_fish_2022, antenna_geo, by = "area")

n_fish_2023 <- det_2023 %>% 
  group_by(area) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(area)
n_fish_2023 <- left_join(n_fish_2023, antenna_geo, by = "area")

n_fish_2024 <- det_2024 %>% 
  group_by(area) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(area)
n_fish_2024 <- left_join(n_fish_2024, antenna_geo, by = "area")


heat_map_all <- ggplot() +
  geom_sf(data = countries, linewidth = .6, fill = "white") +
  stat_density_2d(data = den_det, geom = "polygon", aes(x = lon, y = lat, fill = after_stat(level)), alpha = .3, bins = 30) +
  geom_sf(data = rivers, col = "#03879A") +
  geom_sf(data = new_Inn, col = "#03879A", linewidth = 1) +
  geom_sf(data = Inn_trib, col = "#038780") +
  geom_point(data = antenna_geo, aes(x = lon, y = lat, shape = type), size = 3, col = "grey20") +
  geom_text_repel(data = antenna_geo, aes(x = lon, y = lat, label = site),
                  min.segment.length = 0, seed = 42, box.padding = .5, alpha = .8 , col = "black") +
  xlim(11.99, 12.98) +
  ylim(47.795, 48.35) +
  theme_few() +
  scale_fill_scico(palette = "lipari") +
  theme(panel.background = element_rect(fill = "grey90")) +
  labs(fill = "Level", shape = "Habitat type")

ggsave(heat_map_all, filename = "heat_map.png", height = 20, width = 25, units = "cm", dpi = 600)

dens_map2022 <- ggplot() +
  geom_sf(data = countries, linewidth = .6, fill = "white") +
  stat_density_2d(data = det_2022, geom = "polygon", aes(x = lon, y = lat, fill = after_stat(level)), alpha = .2, bins = 30) +
  geom_sf(data = rivers, col = "#03879A") +
  geom_sf(data = new_Inn, col = "#03879A", linewidth = 1) +
  geom_sf(data = Inn_trib, col = "#038780") +
  geom_point(data = n_fish_2022, aes(x = lon, y = lat, col = type, size = n)) +
  geom_text_repel(data = n_fish_2022, aes(x = lon, y = lat, label = area), max.overlaps = Inf,
                  min.segment.length = 0, seed = 42, box.padding = .5, alpha = .8 , col = "black") +
  xlim(11.99, 12.98) +
  ylim(47.795, 48.35) +
  theme_few() +
  scale_fill_scico(palette = "lipari") +
  scale_color_viridis_d(option = "inferno") +
  theme(panel.background = element_rect(fill = "grey90")) +
  labs(col = "Habitat type", fill = "Level", size = "Number (n)") +
  guides(fill = guide_colourbar(order = 1)) +
  ggtitle("Detections - 2022")

dens_map2023 <- ggplot() +
  geom_sf(data = countries, linewidth = .6, fill = "white") +
  stat_density_2d(data = det_2023, geom = "polygon", aes(x = lon, y = lat, fill = after_stat(level)), alpha = .2, bins = 30) +
  geom_sf(data = rivers, col = "#03879A") +
  geom_sf(data = new_Inn, col = "#03879A", linewidth = 1) +
  geom_sf(data = Inn_trib, col = "#038780") +
  geom_point(data = n_fish_2023, aes(x = lon, y = lat, col = type, size = n)) +
  geom_text_repel(data = n_fish_2023, aes(x = lon, y = lat, label = area), max.overlaps = Inf,
                  min.segment.length = 0, seed = 42, box.padding = .5, alpha = .8 , col = "black") +
  xlim(11.99, 12.98) +
  ylim(47.795, 48.35) +
  theme_few() +
  scale_fill_scico(palette = "lipari") +
  scale_color_viridis_d(option = "inferno") +
  theme(panel.background = element_rect(fill = "grey90")) +
  labs(col = "Habitat type", fill = "Level", size = "Number (n)") +
  guides(fill = guide_colourbar(order = 1), shape = guide_legend(order = 2), col = "none") +
  ggtitle("Detections - 2023")

dens_map2024 <- ggplot() +
  geom_sf(data = countries, linewidth = .6, fill = "white") +
  stat_density_2d(data = det_2024, geom = "polygon", aes(x = lon, y = lat, fill = after_stat(level)), alpha = .2, bins = 30) +
  geom_sf(data = rivers, col = "#03879A") +
  geom_sf(data = new_Inn, col = "#03879A", linewidth = 1) +
  geom_sf(data = Inn_trib, col = "#038780") +
  geom_point(data = n_fish_2024, aes(x = lon, y = lat, col = type, size = n)) +
  geom_text_repel(data = n_fish_2024, aes(x = lon, y = lat, label = area), max.overlaps = Inf,
                  min.segment.length = 0, seed = 42, box.padding = .5, alpha = .8 , col = "black") +
  xlim(11.99, 12.98) +
  ylim(47.795, 48.35) +
  theme_few() +
  scale_fill_scico(palette = "lipari") +
  scale_color_viridis_d(option = "inferno") +
  theme(panel.background = element_rect(fill = "grey90")) +
  labs(col = "Habitat type", fill = "Level", size = "Number (n)") +
  guides(fill = guide_colourbar(order = 1), col = "none") +
  ggtitle("Detections - 2024")

dens_map_years <- dens_map2022 + dens_map2023 + dens_map2024 + plot_layout(ncol = 1) 
ggsave(dens_map_years, filename = "dens_map_years.png", height = 40, width = 30, units = "cm", dpi = 600)

tracks <- ggplot() +
  geom_sf(data = rivers, col = "#03879A") +
  geom_sf(data = new_Inn, col = "#03879A", linewidth = 1) +
  geom_sf(data = Inn_trib, col = "#038780") +
  geom_line(data = den_det, show.legend = FALSE, alpha = .5, aes(x = lon, y = lat, col = Tag_ID)) +
  geom_point(data = den_det, show.legend = FALSE, size = 2, alpha = .5, aes(x = lon, y = lat, col = Tag_ID)) +
  geom_point(data = antenna_geo, aes(x = lon, y = lat, shape = type), size = 3, col = "grey40") +
  xlim(12.05, 12.865) +
  ylim(47.8, 48.3) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") +
  transition_reveal(date) +
  labs(title = "Date: {frame_along}")

animate(tracks, fps = 7, height = 15, width = 20, units = "cm", res = 140)
anim_save("tracks.gif")
