# Miscellaneous figures needed for the V3.5.0 release such as
#       - time varying natural n2o
#       -
# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(paletteer)
library(scales)

# Plotting aesthetics
theme_set(theme_bw())
JW <- 0.15

# 1. Time varying natural N2O emissions  ---------------------------------------



