# Get 100m splits from the past three World Record holders in the men's 1500 fr lcm

# Source:
#   myswimsplits.com for Bobby Finke and Sun Yang
#   swimswam.com an dd swimmingworldmagazine.com for Johannes Liebmann

### Setup ###
# Packages
library(tidyverse)
library(rvest)
library(janitor)

# mss = myswimsplit, ss = swimswam
url_mss <- "https://myswimsplits.com/mens-1500m-freestyle-long-course/"
url_ss <- "https://swimswam.com/johannes-liebmann-shatters-1500-free-world-record-in-1526-79-first-man-in-history-under-1530/"

webpage_mss <- read_html(url_mss)
webpage_ss <- read_html(url_ss)

table_mss <- webpage_mss |> 
  html_element("#tablepress-78") |> 
  html_table(header = FALSE)

table_ss <- webpage_ss |> 
  html_node("table[style*='width: 83.2039%']") |> 
  html_table(header = FALSE)

# Clean dfs
df_mss_clean <- table_mss |> 
  row_to_names(row_number = 2) |>
  rename("distance" = 1,
         "Finke" = "Bobby Finke",
         "Yang" = "Sun Yang",
         "Hafnaoui" = "Ahmed Hafnaoui",
         "Paltrinieri" = "Gregorio Paltrinieri"
         
         ) |> 
  select(1:5)|> 
  slice(-c(1:6)) |>
  mutate(
    across(where(is.character), ~str_remove(.x, "\\s*,\\s*$")),
    across(everything(), ~na_if(., ""))) |> 
  filter(!if_all(everything(), is.na)) |> 
  filter(!distance %in% c("Strokes", "Split Time:", "Total Time:", "50m PB:",
                          "Time off 50m PB:", "Percentage of 50m PB:")) |> 
  mutate(
    distance = str_remove(distance, "m Split$"),
    across(everything(), as.numeric))|> 
  arrange(distance)

df_mss_last50 <- df_mss_clean |> 
  filter(distance == 1500)

df_mss_wide <- df_mss_clean |>  
  mutate(across(-distance, \(x) x + lag(x))) |> # sum with previous 50m row
  filter(distance %% 100 == 0)

# manual correction to myswimsplits source data
# verified against total race time and cross-checked with swimswam
df_mss_wide <- df_mss_wide |> 
  mutate(
    Finke = case_when(
      distance == 900 ~ 58.19,
      distance == 1400 ~ 58.64,
      TRUE ~ Finke
    ),
    Hafnaoui = case_when(
      distance == 500 ~ 58.69,
      distance == 1200 ~ 58.40,
      TRUE ~ Hafnaoui
    )
  )


df_ss_wide <- table_ss |> 
  select(1,4) |> 
  row_to_names(row_number = 1) |> 
  rename("distance" = 1,
         "Liebmann" = 2) |> 
  slice(-c(1,17)) |> 
  mutate(
    distance = str_remove(distance, "m"),
    Liebmann = str_remove(Liebmann, "\\s*\\(.*?\\)"),
    across(everything(), as.numeric))

df_ss_last50 <- tibble(
  distance = 1500,
  Liebmann = 27.39
)

df_wide <- left_join(df_ss_wide, df_mss_wide)

df_last50_wide <- full_join(df_ss_last50, df_mss_last50, by = "distance")


df_long <- df_wide |> 
  pivot_longer(
    cols = -distance,
    names_to = "swimmer",
    values_to = "time"
  )

df_last50_long <- df_last50_wide |> 
  pivot_longer(
    cols = -distance,
    names_to = "swimmer",
    values_to = "time"
  )


dir.create("data", showWarnings = FALSE, recursive = TRUE)
write_csv(df_long, "data/mens_1500_fr_wr.csv")
write_csv(df_last50_long, "data/mens_1500_fr_wr_last50.csv")

