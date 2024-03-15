### Purpose of this code is *both* to plot the data and to dump the associated data
###

library(tidyverse)
library(mgcv)
library(gratia)

here::i_am("R/11_plot.R")


model_file <- here::here("working",
                         "senate_tfs_bam_model.rds")

mod <- readRDS(model_file)

sm <- smooth_estimates(mod, smooth = "s(Age)") 

saveRDS(sm, file = here::here("outputs", "age_smooth_plot_data.rds"))
