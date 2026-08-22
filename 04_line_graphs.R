# line graphs

library(tidyverse)


# Read in data

df_long <- read_csv("data/mens_1500_fr_wr.csv")
df_last50_long <- read_csv("data/mens_1500_fr_wr_last50.csv")

#### Opening 100 vs. race-average pace ###
# calculating how much faster their opening 100 was than their avg 100 pace throughout the race
dumbbell_data <- df_long |> 
  group_by(swimmer) |> 
  summarize(
    opening_100 = time[distance == 100],
    race_avg = mean(time),
    .groups = "drop"
  ) |> 
  mutate(
    gap = race_avg - opening_100,
    swimmer = fct_reorder(swimmer, gap)
  )

p_dumbbell <- ggplot(dumbbell_data, aes(y = swimmer)) +
  geom_segment(aes(x = opening_100, xend = race_avg, yend = swimmer),
               color = "darkgray", linewidth = 1) +
  geom_point(aes(x = race_avg, color = "Race average"), size = 4) +
  geom_point(aes(x = opening_100, color = "Opening 100"), size = 4) +
  scale_color_manual(values = c("Opening 100" = "goldenrod", "Race average" = "#457B9D")) +
  labs(
    title = "Opening 100m Pace vs. Race-Average Pace",
    x = "Split time (s)",
    y = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

p_dumbbell











