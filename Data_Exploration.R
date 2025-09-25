library(tidyverse)
library(lubridate)
library(patchwork)
library(ggalluvial)
library(scico)
library(flextable)
library(janitor)
library(ggforce)

## Read in the data ----
Master <- read_delim("Master.csv", delim = ";",
                     escape_double = FALSE,
                     col_types = cols(Date = col_date(format = "%d.%m.%y"), Time = col_time(format = "%H:%M:%S")),
                     locale = locale(decimal_mark = ","),
                     trim_ws = TRUE)

files <- dir(path = "Detection_data",
             pattern = "*.csv",
             full.names = TRUE)

raw_det_data <- map_dfr(files, read_csv) 

X9SD <- read_csv("9SD_tag_50093a9a-068c-4d23-9cb1-ca16273befcc.csv",
                 col_types = cols(detected = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                  reader = col_character()))

X9SU <- read_csv("9SU_tag_37ff47c0-64c8-49d7-acc3-b8ea4bdca679.csv", 
                 col_types = cols(detected = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                  reader = col_character()))

raw_det_data <- rbind(raw_det_data, X9SD, X9SU)

Detec_download <- read_csv("Detections.csv") %>% 
  filter(!site %in% c("9TB", "9TU"))
TB_data <- read_csv2("Teufelsbruck.csv")

raw_det_data <- rbind(Detec_download, TB_data)

Detections <- raw_det_data %>% 
  mutate(date = lubridate::date(detected), time = format(detected, format = "%H:%M:%S")) %>% 
  mutate(area = case_when(site == "9AA" ~ "Attler Au",
                          site == "9ED" ~ "Ering",
                          site == "9EU" ~ "Ering",
                          site == "9EO" ~ "Ering",
                          site == "9FD" ~ "Feldkirchen",
                          site == "9FU" ~ "Feldkirchen",
                          site == "9ED" ~ "Ering",
                          site == "9GD" ~ "Gars",
                          site == "9GU" ~ "Gars",
                          site == "9IT" ~ "Isen",
                          site == "9JD" ~ "Jettenbach",
                          site == "9KM" ~ "Mangfall",
                          site == "9NL" ~ "Neuoetting L",
                          site == "9NC" ~ "Nussdorf",
                          site == "9NI" ~ "Nussdorf",
                          site == "9NO" ~ "Nussdorf",
                          site == "9PD" ~ "Perach",
                          site == "9PU" ~ "Perach",
                          site == "9DS" ~ "Simbach",
                          site == "9US" ~ "Simbach",
                          site == "9SD" ~ "Stammham",
                          site == "9SU" ~ "Stammham",
                          site == "9TH" ~ "Thal",
                          site == "9ND" ~ "Neuoetting R",
                          site == "9NU" ~ "Neuoetting L",
                          site == "9VB" ~ "Vorderleiten",
                          site == "9WT" ~ "Weitbach",
                          site == "9WD" ~ "Wasserburg",
                          site == "9WU" ~ "Wasserburg",
                          site == "9JU" ~ "Jettenbach",
                          site == "9MF" ~ "Mangfall",
                          site == "9NB" ~ "Nasenloch",
                          site == "9MT" ~ "Marktl",
                          site == "9TU" ~ "Teufelsbruck",
                          site == "9TB" ~ "Teufelsbruck")) %>%
  mutate(site = case_when(site == "9AA" ~ "Attler Au",
                          site == "9ED" ~ "Ering down",
                          site == "9EU" ~ "Ering up",
                          site == "9EO" ~ "Ering old",
                          site == "9FD" ~ "Feldkirchen up",
                          site == "9FU" ~ "Feldkirchen down",
                          site == "9ED" ~ "Ering down",
                          site == "9GD" ~ "Gars down",
                          site == "9GU" ~ "Gars up",
                          site == "9IT" ~ "Isen",
                          site == "9JD" ~ "Jettenbach down",
                          site == "9KM" ~ "Kalten",
                          site == "9NL" ~ "Neuoetting left",
                          site == "9NC" ~ "Steinbach",
                          site == "9NI" ~ "Nussdorf down",
                          site == "9NO" ~ "Nussdorf up",
                          site == "9PD" ~ "Perach down",
                          site == "9PU" ~ "Perach up",
                          site == "9DS" ~ "Simbach down",
                          site == "9US" ~ "Simbach up",
                          site == "9SD" ~ "Stammham down",
                          site == "9SU" ~ "Stammham up",
                          site == "9TH" ~ "Thal",
                          site == "9ND" ~ "Neuoetting R down",
                          site == "9NU" ~ "Neuoetting R up",
                          site == "9VB" ~ "Vorderleiten",
                          site == "9WT" ~ "Weitbach",
                          site == "9WD" ~ "Wasserburg down",
                          site == "9WU" ~ "Wasserburg up",
                          site == "9JU" ~ "Jettenbach up",
                          site == "9MF" ~ "Mangfall",
                          site == "9NB" ~ "Nasenloch",
                          site == "9MT" ~ "Marktl",
                          site == "9TU" ~ "Teufelsbruck up",
                          site == "9TB" ~ "Teufelsbruck down")) %>% 
  mutate(antenna = as.character(antenna)) %>% 
  mutate(antenna_position = case_when(grepl("up", site) ~ "Up",
                                      grepl("down", site) ~ "Down",
                                      site == "Feldkirchen down" & antenna == "02" ~ "Hammerbach",
                                      site == "Feldkirchen down" & antenna == "03" ~ "Hammerbach",
                                      site == "Neuoetting left" & antenna == "01" ~ "Aubach",
                                      site == "Neuoetting left" & antenna == "02" ~ "Down",
                                      site == "Neuoetting left" & antenna == "03" ~ "Up",
                                      TRUE ~ "Tributary")) %>%
  mutate(type = case_when(antenna_position == "Up" ~ "Fish pass",
                          antenna_position == "Down" ~ "Fish pass",
                          site == "Isen" ~ "Tributary",
                          site == "Weitbach" ~ "Tributary",
                          site == "Mangfall" ~ "Tributary",
                          site == "Kalten" ~ "Tributary",
                          site == "Steinbach" ~ "Tributary",
                          site == "Aubach" ~ "Tributary",
                          TRUE ~ "Backwater")) %>%
  mutate(antenna_type = case_when(site %in% c("Attler Au", "Feldkirchen up", "Gars up", "Stammham up",
                                              "Neuoetting R up", "Neuoetting left", "Vorderleiten", "Thal",
                                              "Wasserburg down", "Wasserburg up", "Nasenloch", "Maktl",
                                              "Teufelsbruck up", "Teufelsbruck down") ~ "Pass-through", TRUE ~ "Pass-over"))


results_ctsm <- read_csv2("Two_States_Results.csv")

## Filter for stocked Nase ----

M_Nase <- Master %>% 
  filter(Release_ID %in% paste0("S", 1:11)) %>% 
  mutate(Type = case_when(Type == "Main stream" ~ "Mainstream",
                          TRUE ~ Type))

Detec_Nase <- Detections %>% 
  filter(tag %in% M_Nase$Tag_ID, date <= "2024-10-31") %>% 
  rename("Tag_ID" = "tag")

M_Nase %>% 
  group_by(Release_ID, Site, Type) %>% 
  distinct(Release_ID, .keep_all = TRUE) %>% 
  select(Release_ID, Site, Type) %>% 
  flextable() %>% 
  save_as_docx(path = "Release_table.docx")

n_distinct(Detec_Nase$Tag_ID) # 626 Nase were redetected -> 37.9 %

## Data exploration ----

# Bar plot for re-detection rates for stocking batches
stock_id <- M_Nase %>% 
  select(Tag_ID, Release_ID)

Detec_Nase <- left_join(Detec_Nase, stock_id, by = "Tag_ID")

D_all_n <- Detec_Nase %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  #mutate(n = (n/150)*100) %>% 
  rename("Overall" = "n") 

D_2022_n <- Detec_Nase %>% 
  mutate(Year = lubridate::year(date)) %>% 
  filter(Year == 2022) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  #mutate(n = (n/150)*100)%>% 
  rename("2022" = "n") %>% 
  select(-Release_ID)

D_2023_n <- Detec_Nase %>% 
  mutate(Year = lubridate::year(date)) %>% 
  filter(Year == 2023) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  #mutate(n = (n/150)*100)%>% 
  rename("2023" = "n") %>% 
  select(-Release_ID)

D_2024_n <- Detec_Nase %>% 
  mutate(Year = lubridate::year(date)) %>% 
  filter(Year == 2024) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  #mutate(n = (n/150)*100)%>% 
  rename("2024" = "n") %>% 
  select(-Release_ID)

D_YR_n <- cbind(D_all_n, D_2022_n, D_2023_n, D_2024_n)

bar_det_ID <- D_YR_n %>% 
  mutate(Released = 150) %>% 
  pivot_longer(!Release_ID, names_to = "Occasion", values_to = "n") %>%
  mutate(Habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ "Fish Pass",
                             Release_ID %in% c("S2", "S5", "S8") ~ "Mainstream",
                             TRUE ~ "Backwater")) %>% 
  mutate(Release_ID = fct_relevel(Release_ID, paste0("S", 1:11)),
         Occasion = fct_relevel(Occasion,"Released", "Overall", "2022","2023", "2024"),
         Habitat = fct_relevel(Habitat, "Fish Pass", "Mainstream", "Backwater")) %>% 
  ggplot(aes(x = Release_ID, y = n, fill = Occasion)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha  = .8, col = "grey40") +
  theme_light(base_size = 14) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(color = "black", face = "bold"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(~Habitat, scales = "free_x", ncol = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  geom_text(aes(label = n, group = Occasion), position = position_dodge(width = .9), vjust = -.5) +
  labs(x = "Release ID", y = "Count (n)") +
  scale_fill_manual(values = c("#03879A", "#FCA800", "grey50", "grey70", "grey90"))

ggsave(bar_det_ID, filename = "bar_det_ID.png", height = 25, width = 25, units = "cm", dpi = 600)

# Table for re-detection rates over time for stocking batches
D_all <- Detec_Nase %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  adorn_totals() %>% 
  mutate(Overall_rate = (n/150)*100) %>% 
  rename("Overall" = "n") 

D_2022 <- Detec_Nase %>% 
  mutate(Year = lubridate::year(date)) %>% 
  filter(Year == 2022) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  adorn_totals() %>% 
  mutate("2022_rate" = (n/150)*100)%>% 
  rename("2022" = "n") %>% 
  select(-Release_ID)

D_2023 <- Detec_Nase %>% 
  mutate(Year = lubridate::year(date)) %>% 
  filter(Year == 2023) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  adorn_totals() %>% 
  mutate("2023_rate" = (n/150)*100)%>% 
  rename("2023" = "n") %>% 
  select(-Release_ID)

D_2024 <- Detec_Nase %>% 
  mutate(Year = lubridate::year(date)) %>% 
  filter(Year == 2024) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  count(Release_ID) %>% 
  adorn_totals() %>% 
  mutate("2024_rate" = (n/150)*100)%>% 
  rename("2024" = "n") %>% 
  select(-Release_ID)

D_YR <- cbind(D_all, D_2022, D_2023, D_2024)
D_YR <- D_YR[c(1, 4:11, 2, 3, 12),]

flextable(D_YR) %>% 
  colformat_double() %>% 
  bold(part = "header") %>%
  bold(j = 1) %>% 
  width(j = 1, width = 1.3) %>% 
  color(j = 1, i = c(1,7,9,10,11), color = "#03879A") %>% 
  color(j = 1, i = c(2,5,8), color = "#FCA800") %>% 
  color(j = 1, i = c(3,4,6), color = "firebrick") %>% 
  save_as_docx(path = "redetection_table.docx")

## Circular density plots showing detection intensities per habitat ----

circ_2023 <- Detec_Nase %>% 
  filter(date > "2023-01-05" & date < "2024-01-04") %>% 
  group_by(site, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
ggplot(aes(x = date, y =  after_stat(density), fill = type)) +
  geom_density(col = NA, alpha = .9) +
  ylim(-0.05, 0.025) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = .2),
        strip.text = element_text(size = 11),
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~type) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#CC9933", "#669999", "#000033")) +
  coord_polar() +
  ggtitle("2023")

circ_2024 <- Detec_Nase %>% 
  filter(date > "2024-01-04") %>% 
  group_by(site, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  ggplot(aes(x = date, y =  after_stat(density), fill = type)) +
  geom_density(col = NA, alpha = .9) +
  ylim(-0.05, 0.027) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = .2),
        strip.text = element_text(size = 11),
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~type) +
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = as.Date(c("2024-01-04", "2025-01-05"))) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#CC9933", "#669999", "#000033")) +
  coord_polar() +
  ggtitle("2024")

circ_den_plots <- circ_2023 / circ_2024

ggsave(circ_den_plots, filename = "cir_den_plots.pdf", height = 20, width = 20, units = "cm", dpi = 600)

## Visualization for the results of the survival model ----

h_rate <- ggplot(results_ctsm, aes(x = days, y = h, ymin = h_lo, ymax = h_hi, fill = Habitat, col = Habitat)) +
  geom_ribbon(alpha = .5, col = NA) +
  geom_line(linewidth = 1) +
  theme_light(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  scale_x_continuous(breaks = seq(0, 720, 30)) +
  scale_y_continuous(breaks = seq(0, 0.1, 0.02)) +
  scale_color_manual(values = c("#CC9933", "#669999", "#000033")) +
  scale_fill_manual(values = c("#CC9933", "#669999", "#000033")) +
  labs(x = "", y = "Hazard rate (h)")

det_rate <- ggplot(results_ctsm, aes(x = days, y = mu, ymin = mu_lo, ymax = mu_hi, fill = Habitat, col = Habitat)) +
  geom_ribbon(alpha = .5, col = NA) +
  geom_line(linewidth = 1) +
  theme_light(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  scale_x_continuous(breaks = seq(0, 720, 30)) +
  scale_y_continuous(breaks = seq(0, 0.05, 0.01)) +
  scale_color_manual(values = c("#CC9933", "#669999", "#000033")) +
  scale_fill_manual(values = c("#CC9933", "#669999", "#000033")) +
  labs(x = "Days", y = expression(Detection~rate~(lambda)))

surv_model <- h_rate / det_rate + plot_annotation(tag_levels = "a")
ggsave(surv_model, filename = "surv_model.png", height = 20, width = 22, units = "cm", dpi = 600)       

# Abacus Plot ----
abacus <- Detec_Nase %>% 
  group_by(Tag_ID, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(Short_ID = str_sub(Tag_ID, -4),
         area = fct_relevel(area, "Mangfall", "Feldkirchen", "Attler Au", "Wasserburg", "Vorderleiten",
                            "Teufelsbruck", "Thal", "Gars", "Jettenbach", "Nasenloch", "Isen", "Neuoetting L", "Neuoetting R",
                            "Perach", "Marktl", "Stammham"),
         start_habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ paste(Release_ID, "(Fish Pass)"),
                                   Release_ID %in% c("S2", "S5", "S8") ~ paste(Release_ID, "(Mainstream)"),
                                   TRUE ~ paste(Release_ID, "(Backwater)")),
         start_habitat = fct_relevel(start_habitat, "S1 (Fish Pass)", "S2 (Mainstream)", "S3 (Backwater)", "S4 (Backwater)", "S5 (Mainstream)",
                                     "S6 (Backwater)", "S7 (Fish Pass)", "S8 (Mainstream)", "S9 (Fish Pass)", "S10 (Fish Pass)", "S11 (Fish Pass)")) %>% 
  ggplot(aes(x = date, y = Short_ID, col = area, shape = type)) +
  geom_point(size = 2.2, alpha = .7) +
  theme_light(base_size = 14) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(color = "black", face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~start_habitat, scales = "free_y", nrow = 3) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b-%y") +
  scale_color_viridis_d(option = "viridis", name = "Detection area") +
  scale_shape_manual(values = c(17, 16, 18), name = "Habitat type") +
  labs(x = "", y = "")

ggsave(abacus, filename = "abacus.png", width = 28, height = 38, units = "cm", dpi = 600)
