---
title: Associations between Socioeconomic Factors and Breast Cancer Incidence in Ohio Counties
author: Olivia Ling and Jojo Yu
date: November 28, 2025
output:
  html_document:
    toc: true
    toc_float: true
  pdf_document: default
---
##0. setup
```
install.packages(c("tidyverse","readxl","ggthemes","maps","mapdata","stringr","ggmap"))

library(tidyverse)
library(readxl)
library(ggthemes)
library(maps)
library(mapdata)
library(stringr)
library(ggmap)
```
## 1. Read & clean data
```
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
## 2.
```
ru_summary <- county_df |>
dplyr::count(rural_urban) |>
dplyr::mutate(pct = round(100 * n / sum(n), 1))
ru_summary

app_summary <- county_df |>
dplyr::count(app_flag) |>
dplyr::mutate(pct = round(100 * n / sum(n), 1))
app_summary

cross_tab <- county_df |>
dplyr::count(app_flag, rural_urban) |>
dplyr::group_by(app_flag) |>
dplyr::mutate(pct_within_app = round(100 * n / sum(n), 1))
cross_tab
```
## 3. Read other sheets & merge
```
# Each sheet: first column = County, second column = the variable we want.
# We use select(1, 2) so it doesn't matter what the original header text is.

income_df <- read_excel("County data.xlsx", sheet = "Income", skip = 4) %>%
  select(1, 3) %>%
  setNames(c("county", "median_income")) %>%
  mutate(
    county = county %>%
      stringr::str_to_lower() %>%
      stringr::str_remove(" county$") %>%
      stringr::str_trim(),
    median_income = as.numeric(median_income)
  )

education_df <- read_excel("County data.xlsx", sheet = "Education", skip = 4) %>%
  select(1, 3) %>%
  setNames(c("county", "college_plus_pct")) %>%
  mutate(
    county = county %>%
      stringr::str_to_lower() %>%
      stringr::str_remove(" county$") %>%
      stringr::str_trim(),
    college_plus_pct = as.numeric(college_plus_pct)
  )

population_df <- read_excel("County data.xlsx", sheet = "Population", skip = 4) %>%
  select(1, 3) %>%
  setNames(c("county", "female_pop")) %>%
  mutate(
    county = county %>%
      stringr::str_to_lower() %>%
      stringr::str_remove(" county$") %>%
      stringr::str_trim(),
    female_pop = as.numeric(female_pop)
  )

rural_urban_df <- county_df %>%
  mutate(
    county = county %>%
      stringr::str_to_lower() %>%
      stringr::str_remove(" county$") %>%
      stringr::str_trim()
  ) %>%
  select(county, rural_urban, app_flag)

map_df <- ohio_map %>%
  left_join(poverty,        by = "county") %>%
  left_join(income_df,      by = "county") %>%
  left_join(education_df,   by = "county") %>%
  left_join(population_df,  by = "county") %>%
  left_join(rural_urban_df, by = "county") %>%
  arrange(order)

dplyr::glimpse(map_df)
summary(is.na(map_df[, c("poverty_pct","median_income","college_plus_pct",
                         "female_pop","rural_urban","app_flag")]))
```
## 4. Clean numeric variables and summarize
```
full_df <- full_df %>%
  mutate(
    poverty_rate     = as.numeric(poverty_rate),
    median_income    = as.numeric(gsub(",", "", median_income)),
    college_plus_pct = as.numeric(college_plus_pct),
    female_pop       = as.numeric(gsub(",", "", female_pop)),
    cancer_incidence = as.numeric(cancer_incidence)
  )

# After conversion, now summarize correctly
summary(full_df[, c("poverty_rate", "median_income", "college_plus_pct",
                    "female_pop", "cancer_incidence")])
```
## 5. Graphs: Cancer incidence vs predictors
```
### 5.1 Poverty Rate vs Cancer Incidence

ggplot(full_df, aes(x = poverty_rate, y = cancer_incidence)) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
labs(
title = "Breast Cancer Incidence vs Poverty Rate",
x = "Poverty Rate (%)",
y = "Breast Cancer Incidence (per 100,000)"
) +
theme_minimal()

### 5.2 Median Household Income vs Cancer Incidence

ggplot(full_df, aes(x = median_income, y = cancer_incidence)) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
labs(
title = "Breast Cancer Incidence vs Median Household Income",
x = "Median Household Income ($)",
y = "Breast Cancer Incidence (per 100,000)"
) +
theme_minimal()

### 5.3 Educational Attainment (College+) vs Cancer Incidence

ggplot(full_df, aes(x = college_plus_pct, y = cancer_incidence)) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
labs(
title = "Breast Cancer Incidence vs % Adults With College Degree+",
x = "Adults with College Degree or Higher (%)",
y = "Breast Cancer Incidence (per 100,000)"
) +
theme_minimal()

### 5.4 Female Population (65+ or Total Female Population) vs Cancer Incidence

ggplot(full_df, aes(x = female_pop, y = cancer_incidence)) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
labs(
title = "Female Population vs Breast Cancer Incidence",
x = "Female Population (Count or %)",
y = "Breast Cancer Incidence (per 100,000)"
) +
theme_minimal()
```
## 6. Create "High Incidence" Binary Variable
```
# Median cancer incidence across all counties

median_inc <- median(full_df$cancer_incidence, na.rm = TRUE)

full_df <- full_df %>%
mutate(
high_incidence = if_else(cancer_incidence > median_inc, 1L, 0L),
high_incidence = factor(high_incidence, levels = c(0, 1),
labels = c("Low", "High"))
)

# Check distribution

table(full_df$high_incidence)
```
## 7. Simple Logistic Regression (Unadjusted)
```
model1 <- glm(
high_incidence ~ poverty_rate,
data = full_df,
family = binomial(link = "logit")
)

summary(model1)

## Odds ratios (Exponentiated Coefficients)

exp(coef(model1))

## 95% Confidence Intervals for ORs

exp(confint(model1))
```
# full-data
```
full_df %>%
  select(county, poverty_rate, median_income, college_plus_pct, female_pop) %>%
  head(10)
```
# Correlation
```
cor(full_df[, c("poverty_rate", "median_income", "college_plus_pct", "female_pop")],
    use = "complete.obs")
```
## 8. Multivariable Logistic Regression Model
```
model_full <- glm(
high_incidence ~ poverty_rate +
median_income +
college_plus_pct +
female_pop +
rural_urban +
app_flag,
data = full_df,
family = binomial(link = "logit")
)

summary(model_full)

## Odds ratios for the full model

exp(coef(model_full))

## 95% CI for the ORs

exp(confint(model_full))
```
## 10. Load Ohio county polygons
```
ohio_map <- map_data("county") %>%
dplyr::filter(region == "ohio") %>%
dplyr::mutate(county = tolower(subregion))
```
## 11. Load Poverty data from County data.xlsx
```
## (sheet named exactly "Poverty"; header row is 4, so skip = 4)

poverty <- read_excel(
  "County data.xlsx",
  sheet = "Poverty",
  skip = 4   # THIS is the correct skip
) %>%
  transmute(
    county = stringr::str_to_lower(stringr::str_remove(County, " County$")),
    poverty_pct = as.numeric(`Value (Percent)`)
  )

# quick checks (printed in knitted doc)
head(poverty)
head(ohio_map)
```
## 12. Merge map polygons with poverty data
```
rural_urban_df <- county_df %>%
  mutate(
    county = county %>%
      stringr::str_to_lower() %>%
      stringr::str_remove(" county$") %>%
      stringr::str_trim()
  ) %>%
  dplyr::select(county, rural_urban, app_flag)

map_df <- ohio_map %>%
  dplyr::left_join(poverty,    by = "county") %>%
  dplyr::left_join(income_df,     by = "county") %>%
  dplyr::left_join(education_df,  by = "county") %>%
  dplyr::left_join(population_df, by = "county") %>%
  dplyr::left_join(rural_urban_df, by = "county") %>%
  dplyr::arrange(order)

dplyr::glimpse(map_df)
```
```
#rural_urban data check
dplyr::glimpse(rural_urban_df)
```
## 13. Compute centroids for county labels
```
centroids <- map_df %>%
dplyr::group_by(county) %>%
dplyr::summarise(
clong = mean(long, na.rm = TRUE),
clat = mean(lat, na.rm = TRUE),
.groups = "drop"
)
```
## 14. Draw the heat map for poverty
```
ggplot() +
geom_polygon(
data = map_df,
aes(x = long, y = lat, group = group, fill = poverty_pct),
color = "black",
linewidth = 0.2
) +
coord_fixed(1.3) +
geom_text(
data = centroids,
aes(x = clong, y = clat, label = county),
size = 2.0,
color = "black"
) +
scale_fill_distiller(palette = "Spectral", direction = 1) +
labs(
title = "Family Poverty (%) by County in Ohio",
fill = "Poverty %"
) +
theme_void() +
theme(
legend.position = "top",
plot.title = element_text(hjust = 0.5)
)
```
## 15. Draw the heat map for income
```
ggplot() +
  geom_polygon(
    data = map_df,
    aes(x = long, y = lat, group = group, fill = median_income),
    color = "black",
    linewidth = 0.2
  ) +
  coord_fixed(1.3) +
  geom_text(
    data = centroids,
    aes(x = clong, y = clat, label = county),
    size = 2.0,
    color = "black"
  ) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  labs(
    title = "Income by County in Ohio",
    fill = "Income"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
```
## 16. Draw the heat map for Education
```
ggplot() +
  geom_polygon(
    data = map_df,
    aes(x = long, y = lat, group = group, fill = college_plus_pct),
    color = "black",
    linewidth = 0.2
  ) +
  coord_fixed(1.3) +
  geom_text(
    data = centroids,
    aes(x = clong, y = clat, label = county),
    size = 2.0,
    color = "black"
  ) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  labs(
    title = "Percent with Bachelor's Degree or Higher by County in Ohio",
    fill = "College+ (%)"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
```
## 17. Draw the heat map for female population
```
ggplot() +
  geom_polygon(
    data = map_df,
    aes(x = long, y = lat, group = group, fill = female_pop),
    color = "black",
    linewidth = 0.2
  ) +
  coord_fixed(1.3) +
  geom_text(
    data = centroids,
    aes(x = clong, y = clat, label = county),
    size = 2.0,
    color = "black"
  ) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  labs(
    title = "Female Population by County in Ohio",
    fill = "Female Pop"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
```
## 19. Draw the heat map for rural vs urban classification (categorical)
```
ggplot() +
  geom_polygon(
    data = map_df,
    aes(x = long, y = lat, group = group, fill = rural_urban),
    color = "black",
    linewidth = 0.2
  ) +
  coord_fixed(1.3) +
  geom_text(
    data = centroids,
    aes(x = clong, y = clat, label = county),
    size = 2.0,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Rural / Urban Classification by County in Ohio",
    fill = "Rural/Urban"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
```
```
## 21. Logistic regression model

#high_incidence = 1 → higher-than-median cancer incidence
#high_incidence = 0 → lower-than-median cancer incidence
# Simple logistic regression
model1 <- glm(
  high_incidence ~ poverty_rate,
  data = full_df,
  family = binomial
)

summary(model1)

# Odds ratios
exp(coef(model1))

# 95% confidence intervals
exp(confint(model1))
```
## 18. Draw the heat map for the cancer incidence rate
```
ggplot() +
  geom_polygon(
    data = map_df,
    aes(x = long, y = lat, group = group, fill = cancer_incidence),
    color = "black",
    linewidth = 0.2
  ) +
  coord_fixed(1.3) +
  geom_text(
    data = centroids,
    aes(x = clong, y = clat, label = county),
    size = 2.0,
    color = "black"
  ) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  labs(
    title = "Breast Cancer Incidence per 100,000 Women (Ohio)",
    fill = "Incidence"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
```
##22.
```
model_full <- glm(
  high_incidence ~ poverty_rate + median_income + college_plus_pct +
    female_pop + rural_urban + app_flag,
  data = full_df,
  family = binomial
)

summary(model_full)
```
