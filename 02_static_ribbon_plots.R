# Braided ribbon plots
# First one compares the 100m splits of Finke vs, Yang's World Record times
# Second one compares Liebmann vs. Finke

# Load packages
library(tidyverse)
library(ggbraid) # extension for ggplot2; download from GitHub - remotes::install_github("nsgrantham/ggbraid")
library(gganimate)

# Read in data
splits_1500 <- read.csv("data/mens_1500_fr_wr.csv", header=TRUE)

