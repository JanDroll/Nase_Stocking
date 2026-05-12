# Nase_Stocking
This repository contains the code to reproduce the survival analysis, group comparisons, and figures from the publication "Evaluation of the stocking efficiency of a rheophilic cyprinid in a large river system using spatio-temporal PIT-Tag detections and survival analysis". The repository contains five code files:

- **Data_Exploration.R** contains the R code to import the Datasets and to recreate all the plots of the publication except the map.

- **Group_comparison.R** contains the R code to perform a Chi-square test among redetection groups, as well as the code for a Bayesian binomial GLM with a post hoc test to compare the groups.

- **Map.R** contains the R code to replicate the 2D-Heatmaps on a map.

- **two_state_model.stan** contains the Stan code for a continuous-time survival model as described in Rushing 2023 (https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13902).

- **Survival_model.R** contains the R code to transform the PIT tag detection record into an analysis-ready matrix and the code to perform the Bayesian analysis using Stan.
