library(tidyverse)
library(scico)
library(brms)
library(tidybayes)
library(emmeans)
library(vcd)
library(flextable)

redetection_dat <- D_YR_n %>% 
  mutate(Released = 150) %>% 
  pivot_longer(!Release_ID, names_to = "Occasion", values_to = "n") %>%
  mutate(Habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ "Fish Pass",
                             Release_ID %in% c("S2", "S5", "S8") ~ "Mainstream",
                             TRUE ~ "Backwater")) %>% 
  mutate(Release_ID = fct_relevel(Release_ID, paste0("S", 1:11)),
         Occasion = fct_relevel(Occasion,"Released", "Overall", "2022","2023", "2024"),
         Habitat = fct_relevel(Habitat, "Fish Pass", "Mainstream", "Backwater"))

## Chi-square test for the redetections
re_all <- redetection_dat %>% 
  filter(Occasion == "Overall") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11))) %>% 
  rename(reappered = n) %>% 
  pivot_wider(names_from = Release_ID, values_from = reappered) %>% 
  add_row(150 - .)

chisq_all <- chisq.test(re_all)
chisq_all$stdres

re_2022 <- redetection_dat %>% 
  filter(Occasion == "2022") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11))) %>% 
  rename(reappered = n) %>% 
  pivot_wider(names_from = Release_ID, values_from = reappered) %>% 
  add_row(150 - .)

chisq_2022 <- chisq.test(re_2022)

re_2023 <- redetection_dat %>% 
  filter(Occasion == "2023") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11))) %>% 
  rename(reappered = n) %>% 
  pivot_wider(names_from = Release_ID, values_from = reappered) %>% 
  add_row(150 - .)

chisq_2023 <- chisq.test(re_2023)

re_2024 <- redetection_dat %>% 
  filter(Occasion == "2024") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11))) %>% 
  rename(reappered = n) %>% 
  pivot_wider(names_from = Release_ID, values_from = reappered) %>% 
  add_row(150 - .)

chisq_2024 <- chisq.test(re_2024)

results_chi2 <- tibble(Occasion = c("Overall", "2022", "2023", "2024"),
                       X2 = c(305.31, 522.1, 159.58, 99.21),
                       DF = rep(10, 4),
                       P_Value = 0.00000000000000022)
flextable(results_chi2) %>% 
  save_as_docx(path = "results_chi2.docx")

assocstats(re_2022)

as.data.frame(res_2022) %>% 
  cbind(data.frame(Outcome = c("Reappeared", "Not Reappeared")),.) %>% 
  pivot_longer(-Outcome, names_to = "Release_ID", values_to = "StdResiduals") %>% 
  ggplot(aes(x = Release_ID, y = Outcome, fill = StdResiduals)) +
  geom_tile(color = "white") +
  scale_fill_scico(palette = "vik", midpoint = 0) +
  labs(fill = "Std. Residuals") +
  theme_minimal()



## Binomial GLM fit with brms ----
# Overall
bin_glm_all <- redetection_dat %>% 
  filter(Occasion == "Overall") %>% 
  select(Release_ID, n) %>% 
  mutate(Stocked = 150) %>% 
  rename(Reappeared = n)

glm_fit_all <- brm(formula = Reappeared | trials(Stocked) ~ Release_ID,
                   data = bin_glm_all,
                   family = binomial(link = "logit"),
                   chains = parallel::detectCores() -1,
                   cores = parallel::detectCores() -1,
                   iter = 10000,
                   warmup = 5000,
                   seed = 123)

summary(glm_fit_all)
plot(glm_fit_all)
pp_check(glm_fit_all)

comp_all <- emmeans(glm_fit_all, ~Release_ID, type = "response")

pc_all <- comp_all %>% 
  as.data.frame() %>% 
  mutate(Release_ID = factor(Release_ID, levels = paste0("S", 1:11)),
         Habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ "Fish Pass",
                             Release_ID %in% c("S2", "S5", "S8") ~ "Mainstream",
                             TRUE ~ "Backwater"),
         Occasion = "Overall")

# 2022
bin_glm2022 <- redetection_dat %>% 
  filter(Occasion == "2022") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11)),
         Stocked = 150) %>% 
  rename(Reappeared = n)

glm_fit2022 <- brm(formula = Reappeared | trials(Stocked) ~ Release_ID,
                   data = bin_glm2022,
                   family = binomial(link = "logit"),
                   chains = parallel::detectCores() -1,
                   cores = parallel::detectCores() -1,
                   iter = 10000,
                   warmup = 5000,
                   seed = 234)

summary(glm_fit2022)
plot(glm_fit2022)
pp_check(glm_fit2022)

comp_2022 <- emmeans(glm_fit2022, ~Release_ID, type = "response")

pc_2022 <- comp_2022 %>% 
  as.data.frame() %>% 
  mutate(Release_ID = factor(Release_ID, levels = paste0("S", 1:11)),
         Habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ "Fish Pass",
                             Release_ID %in% c("S2", "S5", "S8") ~ "Mainstream",
                             TRUE ~ "Backwater"),
         Occasion = "2022")

# 2023
bin_glm2023 <- redetection_dat %>% 
  filter(Occasion == "2023") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11)),
         Stocked = 150) %>% 
  rename(Reappeared = n)

glm_fit2023 <- brm(formula = Reappeared | trials(Stocked) ~ Release_ID,
                   data = bin_glm2023,
                   family = binomial(link = "logit"),
                   chains = parallel::detectCores() -1,
                   cores = parallel::detectCores() -1,
                   iter = 10000,
                   warmup = 5000,
                   seed = 345)

summary(glm_fit2023)
plot(glm_fit2023)
pp_check(glm_fit2023)

comp_2023 <- emmeans(glm_fit2023, ~Release_ID, type = "response")

pc_2023 <- comp_2023 %>% 
  as.data.frame() %>% 
  mutate(Release_ID = factor(Release_ID, levels = paste0("S", 1:11)),
         Habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ "Fish Pass",
                             Release_ID %in% c("S2", "S5", "S8") ~ "Mainstream",
                             TRUE ~ "Backwater"),
         Occasion = "2023")

# 2024
bin_glm2024 <- redetection_dat %>% 
  filter(Occasion == "2024") %>% 
  select(Release_ID, n) %>% 
  mutate(Release_ID = as.character(Release_ID)) %>% 
  mutate(Release_ID = fct_relevel(paste0("S", 1:11)),
         Stocked = 150) %>% 
  rename(Reappeared = n)

glm_fit2024 <- brm(formula = Reappeared | trials(Stocked) ~ Release_ID,
                   data = bin_glm2024,
                   family = binomial(link = "logit"),
                   chains = parallel::detectCores() -1,
                   cores = parallel::detectCores() -1,
                   iter = 10000,
                   warmup = 5000,
                   seed = 456)

summary(glm_fit2024)
plot(glm_fit2024)
pp_check(glm_fit2024)

comp_2024 <- emmeans(glm_fit2024, ~Release_ID, type = "response")

pc_2024 <- comp_2024 %>% 
  as.data.frame() %>% 
  mutate(Release_ID = factor(Release_ID, levels = paste0("S", 1:11)),
         Habitat = case_when(Release_ID %in% c("S1", "S7", "S9", "S10", "S11") ~ "Fish Pass",
                             Release_ID %in% c("S2", "S5", "S8") ~ "Mainstream",
                             TRUE ~ "Backwater"),
         Occasion = "2024")

binom_glm_res <- rbind(pc_all, pc_2022, pc_2023, pc_2024) %>% 
  mutate(Occasion = fct_relevel(Occasion, "Overall", "2022", "2023", "2024")) %>% 
  ggplot(aes(x = Release_ID, y = prob, ymin = lower.HPD, ymax = upper.HPD, col = Habitat)) +
  geom_pointrange(size = .6) +
  theme_light(base_size = 14) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.background = element_blank(),
        strip.text.x = element_text(color = "black", face = "bold"),
        axis.line = element_line(color = "black"),
        legend.position = "bottom")+
  facet_wrap(~Occasion) +
  scale_color_manual(values = c("#CC9933", "#669999", "#000033")) +
  labs(x = "Release ID", y = "Posterior mean redetection probability \n with 95 % credible interval")

ggsave(binom_glm_res, filename = "redet_prob.png", height = 15, width = 23, units = "cm", dpi = 600)


re_prob <- rbind(pc_all, pc_2022, pc_2023, pc_2024)

re_prob %>% 
  group_by(Occasion) %>% 
  summarise(range = range(prob))

re_prob %>% 
  ggplot(aes(y = Occasion, x = Release_ID, fill = prob)) +
  geom_tile(col = "white") +
  theme_minimal() +
  scale_fill_scico(palette = "vikO")

re_prob %>% 
  filter(Occasion != "Overall") %>% 
  mutate(Occasion = as.numeric(Occasion)) %>% 
  ggplot(aes(x = Occasion, y = prob, col = Release_ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Habitat, scales = "free_y")
  