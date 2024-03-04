smartphone_quality_markdown
================
2024-03-04

Aim of this project is to use ordinal logistic regression to look for
associations between data quality and smartphone characteristics (age,
storage, ram etc).

Libraries:

``` r
library(readr)
library(dplyr)
```

Load the data:

``` r
combined_data <- read_csv("../combined_data.csv")
combined_data <- combined_data %>% select(-'ram5...9') %>% rename("ram5" = 'ram5...8')
```

The variables:

- cores - how many cores in the CPU (used for parallel computing, it is
  good for opening several apps at the same time)

- storageX - disk size, X different storages depending on model etc.

- ramX - memory ram (it measures the smartphone capacity for handling
  multiple tasks)

- battery capacity - this is the physical battery capacity at
  manufacture measured in mAh, this decreases with the usage so we don’t
  really know the actual capacity when the patients got the app

- GHz_min and GHz_max - processor frequency (how strong is the cpu)

- release_year: should correlate with age of phone (transform to years
  since release)

- quality : main outcome, GOOD, BAD, MEDIUM

- foreground_app_version : bd4qol app version (categorical)

- os_version : version of android os (categorical)

- sdk: sdk app version (categorical)

- brand: phone brand

\#Explore data:
