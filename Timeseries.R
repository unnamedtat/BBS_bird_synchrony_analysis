library(stats)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(gridExtra)
library(rnaturalearth)
library(sf)

source("globalPlots.R")

world_map <-  ne_countries(scale = "medium", returnclass = "sf")%>%
  st_transform(4326)

sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

