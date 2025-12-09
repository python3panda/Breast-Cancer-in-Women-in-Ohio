# Breast-Cancer-in-Women-in-Ohio 
  - Socioeconomic Predictors, Geographic Disparities, and Mapping Analysis

This repository contains all data, scripts, figures, and documentation for an analysis of breast cancer incidence rates across Ohio’s 88 counties, exploring how incidence is associated with:

Poverty rates

Median household income

Educational attainment

Female population distribution

Rural vs. urban classification

Appalachian vs. non-Appalachian county designation

Statistical modeling and geographic analysis were performed using R, R Markdown, and tidyverse workflows.

Repository Structure
├── main/            # Primary working branch
├── data/            # Cleaned datasets, Excel sources, and derived data files
├── scripts/         # R scripts and R Markdown (.Rmd) files for analysis
├── figs/            # Exported plots and county-level heat maps
├── docs/            # HTML and PDF knitted reports
└── README.md        # Project overview

## Contents
/data

Contains:

County socioeconomic data

Breast cancer incidence (per 100,000)

Rural/urban classification

Appalachian county flags

Source file: County data.xlsx

/scripts

Includes:

analysis.Rmd — full reproducible analysis workflow

Data cleaning + merging procedures

Logistic regression modeling (simple + multivariable)

Geospatial mapping code using ggplot2 and maps

/figs

Automatically generated figures:

Scatterplots of incidence vs. predictors

Poverty, income, education, female population heat maps

Rural/urban classification map

Breast cancer incidence heat map

/docs

Final knitted output (HTML/PDF):

Summary of findings

Statistical results

Visualizations

Interpretation

Research Questions

How does breast cancer incidence vary across Ohio counties?

Which socioeconomic factors show the strongest associations with incidence?

Are rural or Appalachian counties at higher risk?

Do multivariable models identify independent predictors after adjustment?

## Methods Overview
1. Data Cleaning & Integration

Imported county-level socioeconomic datasets

Standardized county names (trim, lowercase, remove "County" suffix)

Converted key variables to numeric formats

Joined all datasets into a master file: full_df

2. Exploratory Data Analysis

Summary statistics

Correlation matrices

Scatterplots with linear trend lines

3. Logistic Regression Modeling

Outcome:

High breast cancer incidence (above median) vs. low

Models:

Unadjusted model: poverty → high incidence

Full model: poverty, income, education, female population, rurality, Appalachian status

4. Geospatial Mapping

Created county-level heat maps using:

map_data("county")

Polygon joins

Centroid labels

Color-scaled choropleths

Maps include:

Poverty %

Income

Education (% bachelor’s+)

Female population

Rural/urban classification

Breast cancer incidence rate

Key Findings (Summary)

(Modify based on your actual statistical results—this is a template)

Poverty and lower income were associated with higher cancer incidence.

Education levels showed moderate correlation patterns.

Rural and Appalachian counties exhibited distinct geographic disparities.

The multivariable model identified poverty and income as significant predictors after adjustment.

Geospatial maps highlighted clusters of high-incidence counties in specific regions of Ohio.

Reproducibility

## To reproduce the full analysis:

1. Install required packages:
install.packages(c("tidyverse", "readxl", "ggthemes", "maps", "mapdata", "stringr", "ggmap"))

2. Knit the R Markdown report:

Open:

scripts/Breast-Cancer-in-Women-in-Ohio.Rmd

Then click Knit → HTML.

## Author

Olivia Ling & Jojo Yu
Ohio State University
PUBHLTH 5015 — Data Analytics
Fall 2025

## Contact

For questions or collaboration inquiries:

yu.3364@osu.edu
