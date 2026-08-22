# line graphs

library(tidyverse)

# Read in data

df_long <- read_csv("data/mens_1500_fr_wr.csv")
df_last50_long <- read_csv("data/mens_1500_fr_wr_last50.csv")

#### Opening 100 vs. race-average pace - dumbbell chart ###
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


### Flattest pace ###
# "flattest" = lowest variability across the middle 100s (excludes the opening and closing 100s)

mid_race_data <- df_long |> 
  filter(distance > 100, distance < 1500) |> 
  group_by(swimmer) |> 
  summarize(
    mean_split = mean(time),
    sd_split = sd(time),
    range_split = max(time) - min(time),
    .groups = "drop"
  ) |> 
  arrange(sd_split)

mid_race_data

flattest_swimmer <- mid_race_data$swimmer[1]

p_flattest <- df_long |> 
  filter(distance >= 200, distance <= 1400) |> 
  mutate(highlight = if_else(swimmer == flattest_swimmer, swimmer, "other")) |> 
  ggplot(aes(x = distance, y = time, group = swimmer, color = highlight)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(200, 1400, by = 200)) +
  scale_color_manual(values = setNames(c("#457B9D", "gray80"),
                                       c(flattest_swimmer, "other"))) +
  labs(
    title = "Flattest Pacer",
    subtitle = "Lowest split-time variability, 200m-1400m",
    x = "Distance (m)",
    y = "Split time (s)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_flattest


### Closing 50m lollipop chart ###
closing_50_data <- df_last50_long |> 
  arrange(desc(time)) |> # slowest first
  mutate(
    swimmer = fct_inorder(swimmer),
    reveal_order = row_number()
  )

fastest_close_50 <- closing_50_data$swimmer[nrow(closing_50_data)]
fastest_close_time <- closing_50_data$time[closing_50_data$swimmer == fastest_close_50]

p_closing_50 <- ggplot(closing_50_data, aes(x = swimmer, y = time)) +
  geom_hline(yintercept = fastest_close_time, linetype = "dashed", color = "goldenrod", linewidth = 1) +
  geom_segment(aes(xend = swimmer, y= 0, yend = time),
               color = "grey60", linewidth = 1) +
  geom_point(aes(color = swimmer == fastest_close_50), size = 5) +
  scale_color_manual(values = c("TRUE" = "goldenrod", "FALSE" = "gray40"), guide = "none") +
  coord_flip(ylim = c(25, 29)) +
  labs(title = "Fastest Closing 50m", x = NULL, y = "Split time (s)") +
  theme_classic(base_size = 13)

p_closing_50



