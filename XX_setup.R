
# Install packages and create folders if missing
#install.packages("pak")
#pak::pkg_install(c("dplyr", "tidyr", "lubridate", "ggplot2",
#                   "sf", "rnaturalearth", "slisovski/SGAT", "slisovski/TwGeos"))

#if(!dir.exists("MOBL_data/fixed")) dir.create("MOBL_data/fixed", recursive = TRUE)
#if(!dir.exists("MOBL_data/final")) dir.create("MOBL_data/final", recursive = TRUE)


# Note that raw data files are expected to be found in /MOBL_data/RawData
source("XX_functions.R")

library(SGAT)
library(TwGeos)
library(lubridate) # Better dates
library(ggplot2)   # Slower but easier plots
library(ggplotify)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(patchwork)
library(rnaturalearth)
library(MASS)
library(sf)


dir <- file.path("Data/Raw/RMA4406 Deployed Data/")
out_dir <- "MOBL_data/fixed/"

# We'll set Kamloops as the release location
geo_location <- c("longitude" = -120.33,
                  "latitude" = 50.67)
options("geo_location" = geo_location)


# Expected migration start
options("geo_start_date" = yday("2020-10-01"))

north_america <- ne_states(country = c("United States of America", "Canada"),
                           returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska")) |>
  st_transform(crs = 3347)
