Marketing Analytics Regression
================
Jose Alberto Martinez Morales
2026-07-31

**Marketing Analytics (DAMO-520-21)**

**Instructor:** Prof. Mohamad Makki

# Introduction

This notebook applies regression modeling to
`marketing_analytics_dataset_5000.csv` to analyze customer behavior,
following the assignment structure:

- **Part A**: Exploratory analysis and three regression models
  (baseline, multiple regression, improved/alternative form)
- **Part B**: Diagnostic testing (linearity, normality,
  homoscedasticity, multicollinearity, influential observations)
- **Part C**: Business implications and limitations (written up
  separately in the final report)

We’ll build this up step by step, one chunk at a time.

# Step 0: Load Packages and Data

``` r
library(tidyverse)   # dplyr, ggplot2, etc.
library(car)         # VIF, component+residual plots
library(lmtest)      # Breusch-Pagan test
library(GGally)      # correlation matrix / pairs plots
library(readxl)      # for reading .xlsx files
```

``` r
df <- read_excel("C:/Users/piuze/OneDrive/Documentos/marketing-analytics-regression/data/marketing_analytics_dataset_5000.xlsx")
dim(df)
```

    ## [1] 5000   15

``` r
str(df)
```

    ## tibble [5,000 × 15] (S3: tbl_df/tbl/data.frame)
    ##  $ CustomerID       : num [1:5000] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Age              : num [1:5000] 56 69 46 32 60 25 38 56 36 40 ...
    ##  $ Gender           : chr [1:5000] "Male" "Male" "Female" "Female" ...
    ##  $ Income           : num [1:5000] 48828 105115 73105 50711 70155 ...
    ##  $ Region           : chr [1:5000] "North" "West" "East" "South" ...
    ##  $ WebsiteVisits    : num [1:5000] 5 4 10 14 8 14 11 7 8 8 ...
    ##  $ TimeOnSite       : num [1:5000] 4.28 7.1 5.26 1.6 2.57 ...
    ##  $ PagesViewed      : num [1:5000] 9 9 7 2 7 9 18 12 7 19 ...
    ##  $ AdImpressions    : num [1:5000] 137 476 280 95 451 56 122 230 289 85 ...
    ##  $ Clicks           : num [1:5000] 2 2 2 2 3 2 7 4 2 1 ...
    ##  $ EmailOpens       : num [1:5000] 11 24 5 10 5 19 4 12 0 15 ...
    ##  $ PromoUsed        : num [1:5000] 0 0 0 0 0 1 1 0 1 0 ...
    ##  $ SatisfactionScore: num [1:5000] 3.32 3.02 4.26 3.73 4.28 4.73 3.85 2.58 1.85 2.08 ...
    ##  $ PurchaseAmount   : num [1:5000] 50 56.6 77.3 63.1 66 ...
    ##  $ Churn            : num [1:5000] 0 0 1 0 0 0 1 0 0 0 ...

# Step 1: Exploratory Analysis

*(To fill in together 014 choosing the dependent variable, at least four
independent variables, correlation matrix, and distribution/relationship
plots.)*

# Step 2: Model Development

*(To fill in together 014 Model 1 baseline, Model 2 multiple regression,
Model 3 improved/alternative form.)*

# Part B: Diagnostic Testing

*(To fill in together 014 linearity, normality, homoscedasticity,
multicollinearity, influential observations.)*
