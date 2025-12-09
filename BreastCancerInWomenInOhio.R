```title: Associations between Socioeconomic Factors and Breast Cancer Incidence in Ohio Counties
author: Olivia Ling and Jojo Yu
date: November 28, 2025
output:
  html_document:
    toc: true
    toc_float: true
  pdf_document: default
```
```
##0. setup

install.packages(c("tidyverse","readxl","ggthemes","maps","mapdata","stringr","ggmap"))

library(tidyverse)
library(readxl)
library(ggthemes)
library(maps)
library(mapdata)
library(stringr)
library(ggmap)

## 1. Read & clean data

county_df <- read_excel("County data.xlsx", skip = 4) |>
  dplyr::rename(
    county      = `County`,
    rural_urban = `Rural/Urban`,
    app_flag    = `...3`          # third column (Appalachian info)
  ) |>
  dplyr::filter(!is.na(county)) |>
  dplyr::mutate(
    county = stringr::str_trim(county),
    rural_urban = factor(
      rural_urban,
      levels = c("Urban", "Partially Rural County", "Rural")
    ),
    # app_flag column is "Appalachian County" or NA
    app_flag = if_else(
      is.na(app_flag),
      "Non-Appalachian",
      "Appalachian"
    ),
    app_flag = factor(app_flag, levels = c("Non-Appalachian","Appalachian"))
  )

glimpse(county_df)
```
