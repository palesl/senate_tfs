### Purpose of this code is *both* to plot the data and to dump the associated data
###

library(tidyverse)
library(mgcv)
library(gratia)
library(marginaleffects)
library(tictoc)

my_threads <- 8
options(marginaleffects_cores = my_threads)

here::i_am("R/12_contrasts.R")

most_common <- function(x) {
    names(sort(table(x), decreasing = TRUE))[[1]]
}


model_file <- here::here("working",
                         "senate_tfs_bam_model.rds")

mod <- readRDS(model_file)
dat <- readRDS(file = here::here("working",
                                 "senate_model_data.rds"))

### takes about seven seconds
tic()
cmp <- comparisons(mod,
                   newdata = datagrid(),
                   variables = list(Age = c(51, 65)),
                   type = "response")
toc()

tic()
cmp2 <- comparisons(mod,
                    newdata = datagrid(),
                    variables = list(Age = c(65, 75)),
                   type = "response")
toc()

save(cmp, cmp2, file = here::here("working",
                                  "comparisons.RData"))

