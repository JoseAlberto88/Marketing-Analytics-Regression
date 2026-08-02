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
library(patchwork) 
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

## Key Insights from Exploratory Analysis

- **Data quality is clean**: no missing values across all 15 columns, no
  duplicate rows, and data types are internally consistent. Of the 4
  categorical variables, only `Gender` and `Region` are stored as
  character columns; `Churn` and `PromoUsed` are categorical in nature
  (binary yes/no flags) but stored as numeric 0/1 values, so a dtypes
  check alone doesn’t fully capture the categorical/continuous split. It
  has to be judged by what each variable actually represents.

- **Right-skewed variables**: `Clicks`, `EmailOpens`, `WebsiteVisits`,
  and `TimeOnSite` all show a classic right-skew shape, a peak near low
  values with a long tail extending right. This is typical of
  count/engagement data and suggests these variables are **not normally
  distributed**, a pattern worth citing when justifying a non-linear or
  transformed model later.

- **Roughly uniform (flat) variables**: `Age`, `AdImpressions`, and
  `PagesViewed` show almost no concentration around a central value,
  every range is nearly equally likely, unlike a bell curve.

- **Closer to normal**: `Income` and `PurchaseAmount` both show a
  visibly bell-shaped, single-peaked distribution, the strongest
  candidates for variables that behave well under standard linear
  regression assumptions.

- **Class imbalance in `Churn`**: the majority of customers have
  `Churn = 0`, with far fewer `Churn = 1` cases, important to flag if
  this variable is ever used as a predictor or outcome.

- **Balanced categories elsewhere**: `Gender`, `PromoUsed`, and `Region`
  are all reasonably evenly distributed across their categories, so none
  of these raise a class-imbalance concern.

# Step 2: Business Questions

With the data validated and the basic distributions understood from Step
1, this section moves from describing the data to actually *using* it to
answer questions a marketing team would care about. Five questions were
selected, each chosen because the straightforward average or correlation
hides a more interesting story underneath, a hidden tension, a decoupled
relationship, or an interaction effect that only shows up once you look
at the right combination of variables.

Each question is paired with one supporting visual and a short
interpretation of what it reveals. We’ll build these one at a time,
verifying each visualization actually surfaces the underlying pattern
before moving to the next.

**The five questions:**

1.  Are promotions attracting genuine customers, or just bargain hunters
    who churn anyway?
2.  Is more time on the website actually worth more money, or is there a
    point of diminishing returns?
3.  Which region converts marketing exposure into revenue most
    efficiently?
4.  Are the happiest customers also the most valuable, or are
    satisfaction and spending decoupled?
5.  Is there a specific age × income combination quietly driving most of
    the revenue?

**Question 1: Are promotions attracting genuine customers, or just
bargain hunters who churn anyway ?**

``` r
summary_promo <- df %>%
  group_by(PromoUsed) %>%
  summarise(avg_purchase = mean(PurchaseAmount), churn_rate = mean(Churn) * 100) %>%
  mutate(PromoUsed = factor(PromoUsed, labels = c("No Promo", "Used Promo")))

p1 <- ggplot(summary_promo, aes(x = PromoUsed, y = avg_purchase, fill = PromoUsed)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(avg_purchase, 1))), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#8FBFE0", "#F4A99B")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Average Purchase Amount", x = NULL, y = "Purchase Amount ($)") +
  ylim(0, max(summary_promo$avg_purchase) * 1.2)

p2 <- ggplot(summary_promo, aes(x = PromoUsed, y = churn_rate, fill = PromoUsed)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(round(churn_rate, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#8FBFE0", "#F4A99B")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Churn Rate", x = NULL, y = "Churn Rate (%)") +
  ylim(0, max(summary_promo$churn_rate) * 1.3)

p1 + p2 + plot_annotation(title = "Do Promo Users Spend More, But Also Churn More?")
```

![](Marketing-Analytics-Regression_files/figure-gfm/q1-promo-churn-1.png)<!-- -->

**Question 1: Insights**

Promo users spend noticeably more on average (\$99.10 vs. \$80.40, a 23%
lift), while their churn rate is actually slightly lower, not higher
(24.2% vs. 24.6%). This directly answers the tension the question was
built around: the data does not support the “bargain hunter” concern. If
promotions were mainly attracting price-sensitive customers who churn
right after redeeming a discount, we’d expect the churn bar to be taller
for promo users, instead it’s marginally shorter.

This is a genuinely useful marketing finding: **promo usage looks like a
clean win**, correlating with both higher spend and (if anything)
slightly better retention, rather than trading short-term revenue for
long-term churn risk. Worth flagging as a caveat in your write-up
though. This is a **correlation, not a causal claim**: customers who
were already going to spend more and stay longer might simply be more
likely to notice and use promos in the first place (reverse causation /
selection effect), rather than the promo itself causing the higher
spend.

**Question2: Is more time on the website actually worth more money or is
there a point of diminishing returns ?**

``` r
decile_summary <- df %>%
  mutate(TimeOnSite_decile = ntile(TimeOnSite, 10)) %>%
  group_by(TimeOnSite_decile) %>%
  summarise(avg_time = mean(TimeOnSite), avg_purchase = mean(PurchaseAmount))

ggplot(decile_summary, aes(x = TimeOnSite_decile, y = avg_purchase)) +
  geom_line(color = "#4A9D5B", linewidth = 1.1) +
  geom_point(size = 3, color = "#4A9D5B") +
  geom_text(aes(label = paste0("$", round(avg_purchase, 0))), vjust = -1.2, size = 3.3) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  labs(
    title = "Is There a Point of Diminishing Returns on Time-on-Site?",
    subtitle = "Average Purchase Amount by TimeOnSite Decile (1 = lowest engagement, 10 = highest)",
    x = "TimeOnSite Decile", y = "Average Purchase Amount ($)"
  )
```

![](Marketing-Analytics-Regression_files/figure-gfm/q2-timeonsite-deciles-1.png)<!-- -->

**Question 2: Insights**

The relationship is essentially **flat**, average purchase amount barely
moves across `TimeOnSite` deciles, staying in a narrow band between
about \$88.6 and \$92 (roughly a **4% spread** from lowest to highest
decile). That’s a meaningfully different pattern than the
steadily-climbing curve we saw on the synthetic test data.

This actually answers the business question directly, just not the way
we expected: **there’s no “point of diminishing returns” to find,
because there’s no real returns curve here to begin with.** Time spent
on the site doesn’t appear to meaningfully predict how much a customer
spends. The small bump at decile 6 (\$92) is most likely just sampling
noise, each decile only contains about 1/10th of your customers, so a
single group landing a few dollars higher than its neighbors isn’t
strong evidence of a real effect, especially when deciles 5, 7, 8, and 9
all sit right back around \$89.

# Part B: Diagnostic Testing

*(To fill in together 014 linearity, normality, homoscedasticity,
multicollinearity, influential observations.)*
