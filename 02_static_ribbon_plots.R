# Braided ribbon plots
# First one compares the 100m splits of Finke vs, Yang's World Record times
# Second one compares Liebmann vs. Finke

# Load packages
library(tidyverse)
library(ggbraid) # extension for ggplot2; download from GitHub - remotes::install_github("nsgrantham/ggbraid")
library(gganimate)

# Read in data
splits_1500 <- read.csv("data/mens_1500_fr_wr.csv", header=TRUE)

# Finke vs Yang
finke_yang_static <-
  ggplot(splits_1500, aes(x = distance)) +
  geom_braid(aes(ymin = Yang, ymax = Finke, fill = Yang < Finke), alpha = 0.4) +
  geom_line(aes(y = Yang), color = "darksalmon", linewidth = 1) +
  geom_line(aes(y = Finke), color = "steelblue", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 1500, by = 300)) +
  scale_fill_manual(values = c("TRUE" = "darksalmon", "FALSE" = "steelblue"),
                    labels = c("TRUE" = "Yang faster split", "FALSE" = "Finke faster split"),
                    name = NULL) +
  labs(x = "Distance (m)", y = "100m split time (s)",
       title = "Finke vs. Yang 1500m FR World Record - 100m split times") +
  theme_minimal()


# Ribbon plot - Liebmann vs Finke
liebmann_finke_static <- 
  ggplot(splits_1500, aes(x = distance)) +
  geom_braid(aes(ymin = Finke, ymax = Liebmann, fill = Finke < Liebmann), alpha = 0.4) +
  geom_line(aes(y = Finke), color = "steelblue", linewidth = 1) +
  geom_line(aes(y = Liebmann), color = "darkgoldenrod1", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 1500, by = 300)) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "darkgoldenrod1"),
                    labels = c("TRUE" = "Finke faster split", "FALSE" = "Liebmann faster split"),
                    name = NULL) +
  labs(x = "Distance (m)", y = "100m split time (s)",
       title = "Liebmann vs. Finke - 100m split times") +
  theme_minimal()


ggsave("finke_yang_1500.png", finke_yang_static,
       width = 10, height = 8, dpi = 400, scale = 0.5)

ggsave("liebmann_finke_1500.png", liebmann_finke_static,
       width = 10, height = 8, dpi = 400, scale = 0.5)
