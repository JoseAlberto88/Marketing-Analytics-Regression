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

Before selecting a dependent variable and building any regression
models, we first need to understand the dataset itself. This section
performs a basic diagnostic pass over
`marketing_analytics_dataset_5000.xlsx` to answer four questions:

- **Are the data types consistent?** Numeric-looking columns should
  actually be read as numeric, not accidentally stored as text, and vice
  versa for categorical variables.
- **Are there missing values?** If so, how many, and in which columns?
- **Are there duplicate rows?** Repeated customer records would bias any
  model built on top of them.
- **Which variables are continuous, and which are categorical?** This
  distinction matters immediately: continuous variables are candidates
  for direct numeric predictors in a regression model, while categorical
  variables (like `Region` or `Gender`) will need to be encoded before
  they can be used, and binary flags like `PromoUsed` and `Churn` behave
  more like categories than true continuous measurements even though
  they’re stored as 0/1 numbers.

Once we understand the shape and quality of the data, we’ll visualize
the distribution of every variable, histograms for continuous variables
and bar charts for categorical ones, to get a first look at skewness,
spread, and class imbalance. These visual patterns will directly inform
how we justify our choice of dependent variable and regression type in
the next step, rather than relying on assumptions from theory alone.

**Step 1a: Data Types Check**

``` r
dtypes <- sapply(df, function(x) class(x)[1])
table(dtypes)
```

    ## dtypes
    ## character   numeric 
    ##         2        13

**Step 1b: Missing Values**

``` r
colSums(is.na(df))
```

    ##        CustomerID               Age            Gender            Income 
    ##                 0                 0                 0                 0 
    ##            Region     WebsiteVisits        TimeOnSite       PagesViewed 
    ##                 0                 0                 0                 0 
    ##     AdImpressions            Clicks        EmailOpens         PromoUsed 
    ##                 0                 0                 0                 0 
    ## SatisfactionScore    PurchaseAmount             Churn 
    ##                 0                 0                 0

**Step 1c: Duplicates**

``` r
cat("Duplicate rows:", sum(duplicated(df)), "\n")
```

    ## Duplicate rows: 0

**Step 1d: Split Continuous versus Categorical**

``` r
id_vars <- c("CustomerID")
categorical_vars <- c("Gender", "Region", "PromoUsed", "Churn")
continuous_vars <- setdiff(names(df), c(id_vars, categorical_vars))

cat("ID variable (excluded from analysis):", paste(id_vars, collapse = ", "), "\n")
```

    ## ID variable (excluded from analysis): CustomerID

``` r
cat("Categorical variables:", paste(categorical_vars, collapse = ", "), "\n")
```

    ## Categorical variables: Gender, Region, PromoUsed, Churn

``` r
cat("Continuous variables:", paste(continuous_vars, collapse = ", "), "\n")
```

    ## Continuous variables: Age, Income, WebsiteVisits, TimeOnSite, PagesViewed, AdImpressions, Clicks, EmailOpens, SatisfactionScore, PurchaseAmount

**Step 1e: Histograms for continuous variables**

``` r
df_long <- df %>% select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(df_long, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.9) +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Continuous Variables",
       x = NULL, y = "Count")
```

![](Marketing-Analytics-Regression_files/figure-gfm/histograms-plain-1.png)<!-- -->
**Step 1f: Bar Charts for all Categorical variables**

``` r
df_long_cat <- df %>% select(all_of(categorical_vars)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(df_long_cat, aes(x = Value, fill = Value)) +
  geom_bar(color = "white", alpha = 0.9) +
  facet_wrap(~Variable, scales = "free_x") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Categorical Variables", x = NULL, y = "Count")
```

![](Marketing-Analytics-Regression_files/figure-gfm/barcharts-1.png)<!-- -->

# Step 2: Model Development

*(To fill in together 014 Model 1 baseline, Model 2 multiple regression,
Model 3 improved/alternative form.)*

# Part B: Diagnostic Testing

*(To fill in together 014 linearity, normality, homoscedasticity,
multicollinearity, influential observations.)*
