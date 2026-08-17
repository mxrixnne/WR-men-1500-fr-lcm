# Get 100m splits from the past three World Record holders in the men's 1500 fr lcm

# Source:
#   myswimsplits.com for Bobby Finke and Sun Yang
#   swimswam.com for Johannes Liebmann

### Setup ###
# Packages
library(tidyverse)
library(rvest)
library(ggbraid) # extension for ggplot2; download from GitHub - remotes::install_github("nsgrantham/ggbraid")

# mss = myswimsplit, ss = swimswam
url_mss <- "https://myswimsplits.com/mens-1500m-freestyle-long-course/"
url_ss <- "https://swimswam.com/johannes-liebmann-shatters-1500-free-world-record-in-1526-79-first-man-in-history-under-1530/"

webpage_mss <- read_html(url_mss)
webpage_ss <- read_html(url_ss)

table_mss <- webpage_mss |> 
  html_element("#tablepress-78") |> 
  html_table(header = TRUE)

table_ss <- webpage_ss |> 
  html_node("table[style*='width: 83.2039%']") |> 
  html_table()

