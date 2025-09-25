library(tidyverse)
library(rstan)
library(bayesplot)

## Preparing the data ----
# Detection data from 2024-10-31 -> 745 days difference to first stocking

batch1 <- Detec_Nase %>% 
  filter(Release_ID %in% c("S3", "S4")) %>% 
  group_by(Tag_ID, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(days = as.numeric(difftime(date, as.Date("2022-10-17"), units = "days"))) %>% 
  group_by(Tag_ID) %>% 
  arrange(days) %>% 
  mutate(Encounter = row_number()) %>% 
  select(Tag_ID, days, Encounter) %>% 
  mutate(days = case_when(days == 0 ~ 0.5,
                          TRUE ~ days))
  
batch2 <- Detec_Nase %>% 
  filter(Release_ID == "S6") %>% 
  group_by(Tag_ID, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(days = as.numeric(difftime(date, as.Date("2022-10-18"), units = "days"))) %>% 
  group_by(Tag_ID) %>% 
  arrange(days) %>% 
  mutate(Encounter = row_number()) %>% 
  select(Tag_ID, days, Encounter) %>% 
  mutate(days = case_when(days == 0 ~ 0.5,
                          TRUE ~ days))

batch3 <- Detec_Nase %>% 
  filter(Release_ID %in% c("S10", "S11")) %>% 
  group_by(Tag_ID, date) %>% 
  distinct(Tag_ID, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(days = as.numeric(difftime(date, as.Date("2022-10-31"), units = "days"))) %>% 
  group_by(Tag_ID) %>% 
  arrange(days) %>% 
  mutate(Encounter = row_number()) %>% 
  select(Tag_ID, days, Encounter) %>% 
  mutate(days = case_when(days == 0 ~ 0.5,
                          TRUE ~ days))

Init <- M_Nase %>% 
  filter(Release_ID %in% c("S3", "S4", "S6")) %>% 
  select(Tag_ID) %>% 
  mutate(days = 0,
         Encounter = 0)

DH_long <- rbind(Init, batch1, batch2) %>% 
  filter(days <= 720) # Filtering for length of study

DH_wide <- DH_long %>% 
  pivot_wider(names_from = Encounter, values_from = days) %>% 
  select(-c("Tag_ID", "0"))
DH_wide[is.na(DH_wide)] <- 0

det <- unname(as.matrix(DH_wide)) 

# Model parameters
N <- nrow(DH_wide) # Number of individuals
t <- 720 # Length of study
U <- apply(DH_wide, 1, function(row) sum(row > 0)) # Number of detections per individual

## Model ----
# Calculate time difference between detections (including last detection to end of the study)

delta <- matrix(0, nrow = N, ncol = max(U) + 1)
delta[,1] <- det[,1]

for(i in 1:N){
  if(U[i] > 1){
    for (j in 2:U[i]) {
      delta[i, j] <- det[i, j] - det[i, j - 1]
    }
  }
  delta[i, U[i] + 1] <- t - max(det[i,])
}

# Fitting the model in stan
# Save the data as a list
stan_data <- list(N = N,
                  t = t,
                  delta = delta,
                  max_U_plus_one = max(U) + 1,
                  U = U,
                  f = matrix(c(1, 0), 1, 2),
                  ones = matrix(c(1, 1), 2, 1))

# Initial values
init_list <- lapply(1:10, function(i) {
  list(h = runif(1, 0.009, 0.05),
       mu = runif(1, 0.001, 0.01))
})

# Fit the model
fit <- stan(file = "two_state_model.stan",
            pars = c("h", "mu"),
            data = stan_data,
            init = init_list,
            iter = 50000,
            warmup = 25000,
            chains = 10,
            thin = 10)

print(fit, digits_summary = 5)

as.data.frame(fit) %>% 
  summarise(h_lo = quantile(h, probs = .05),
            h_hi = quantile(h, probs = .95),
            mu_lo = quantile(mu, probs = .05),
            mu_hi = quantile(mu, probs = .95))
