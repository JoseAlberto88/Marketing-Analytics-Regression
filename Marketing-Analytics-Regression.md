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

**Question 3: Which region converts marketing expose into revenue most
efficiently ?**

``` r
region_summary <- df %>%
  group_by(Region) %>%
  summarise(
    avg_purchase = mean(PurchaseAmount),
    revenue_per_impression = mean(PurchaseAmount) / mean(AdImpressions)
  ) %>%
  mutate(Region = factor(Region, levels = Region[order(revenue_per_impression)]))

p1 <- ggplot(region_summary, aes(x = Region, y = avg_purchase, fill = Region)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(avg_purchase, 0))), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Raw Average Purchase Amount", x = NULL, y = "Purchase Amount ($)") +
  ylim(0, max(region_summary$avg_purchase) * 1.2)

p2 <- ggplot(region_summary, aes(x = Region, y = revenue_per_impression, fill = Region)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(revenue_per_impression, 3))), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Revenue per Ad Impression", x = NULL, y = "Revenue / Impression ($)") +
  ylim(0, max(region_summary$revenue_per_impression) * 1.3)

p1 + p2 + plot_annotation(title = "Which Region Is Actually the Most Marketing-Efficient?")
```

![](Marketing-Analytics-Regression_files/figure-gfm/q3-region-efficiency-1.png)<!-- -->

**Question 3: Insights**

Here, both panels tell essentially the **same story**, just at different
resolutions. Raw purchase amounts are nearly identical across all four
regions (\$89–\$90), and revenue-per-impression is similarly tight
(\$0.353–\$0.370), but there’s a consistent, if modest, ordering in both
panels: **North is lowest**, **South is highest**, with East and West
sitting in between.

This is the “no hidden story” case we flagged as a possibility: the
region generating the most raw revenue (South) is also the most
marketing-efficient one, and the region with the lowest raw revenue
(North) is also the least efficient. There’s no reordering between the
two panels, meaning current regional ad allocation isn’t obviously
*misallocated* relative to where conversion actually happens.

**What this actually tells a marketing team**: the regional differences
here are small in absolute terms (a spread of about \$0.02 per
impression, roughly a 5% gap between North and South), not the dramatic
story the “hidden efficiency” framing was hoping to surface. That’s a
legitimate finding worth stating plainly rather than overselling:
**region alone isn’t a strong lever for revenue optimization in this
dataset**, since all four markets convert ad exposure at roughly
comparable rates. If you want a stronger regional story, it might be
worth checking whether the *variables driving* purchase amount (like
`TimeOnSite` or `Income`) themselves vary meaningfully by region. That
could explain why South edges out North even though ad efficiency alone
doesn’t look dramatically different.

**Question 4: Are the happiest customers also the most valuable or are
satisfaction and spending decoupled ?**

``` r
ggplot(df, aes(x = SatisfactionScore, y = PurchaseAmount, color = factor(Churn))) +
  geom_point(alpha = 0.4, size = 1.3) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("0" = "#5B8FB9", "1" = "#D9534F"),
                      labels = c("Retained", "Churned"), name = "Customer Status") +
  theme_minimal() +
  labs(
    title = "Are Happy Customers Also the Most Valuable — or Are They Decoupled?",
    subtitle = "Purchase Amount vs. Satisfaction, colored by Churn status",
    x = "Satisfaction Score", y = "Purchase Amount ($)"
  )
```

![](Marketing-Analytics-Regression_files/figure-gfm/q4-satisfaction-churn-1.png)<!-- -->

**Question 4: Insights**

This is a clear **“decoupled”** result, both the blue (retained) and red
(churned) loess lines are essentially **flat across the entire
satisfaction range**, hovering around \$85–\$90 regardless of whether
satisfaction is 1 or 5. There’s no meaningful upward trend for either
group, and the cloud of points is uniformly scattered with no visible
funnel or shift as satisfaction increases.

This directly answers the question: **satisfaction and spending are
largely decoupled** in this dataset. A customer who rates their
satisfaction a 5 isn’t spending meaningfully more than one who rates it
a 1, and this holds true whether they eventually churned or not. That’s
actually a fairly important finding, because it pushes back against a
natural assumption: it’s tempting to treat `SatisfactionScore` as a
proxy for customer value, but this chart shows that assumption doesn’t
hold here.

**Worth flagging carefully, though**: the two lines do diverge very
*slightly* at the high-satisfaction end, churned customers trend a touch
lower (~\$85) while retained customers trend a touch higher (~\$90)
around satisfaction = 5. Given how flat and close together both lines
are across the rest of the range, I’d treat this small gap as within
noise rather than a real signal, unless it’s backed up by something more
rigorous later (like a regression coefficient with a tight confidence
interval). Better to state the finding as “no meaningful relationship
between satisfaction and spending” rather than reaching for a subtler
story that the data doesn’t strongly support.

**Marketing implication**: if satisfaction doesn’t predict spend, then
investing purely in “keep customers happy” initiatives may not move
revenue on its own, retention and upsell strategies might need to target
behavioral signals (like `TimeOnSite`, `WebsiteVisits`, or `PromoUsed`,
which *did* show real relationships with `PurchaseAmount`) rather than
satisfaction scores.

**Question 5 Is there a specific age x income combination quietly
driving most of the revenue ?**

``` r
heat_data <- df %>%
  mutate(
    age_bracket = cut(Age, breaks = 6, dig.lab = 5),
    income_bracket = cut(Income, breaks = 6, dig.lab = 6)
  ) %>%
  group_by(age_bracket, income_bracket) %>%
  summarise(avg_purchase = mean(PurchaseAmount), n = n(), .groups = "drop")

ggplot(heat_data, aes(x = age_bracket, y = income_bracket, fill = avg_purchase)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0("$", round(avg_purchase, 0))), size = 3.2, color = "black") +
  scale_fill_gradient(low = "#FDEBD0", high = "#B9481F", name = "Avg Purchase ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(
    title = "Is There a Hidden Age x Income Interaction Driving Revenue?",
    subtitle = "Average Purchase Amount by Age Bracket x Income Bracket",
    x = "Age Bracket", y = "Income Bracket"
  )
```

![](Marketing-Analytics-Regression_files/figure-gfm/q5-age-income-heatmap-1.png)<!-- -->

**Question 5: Insights**

This is a clean, clear pattern: **strong vertical gradient, flat
horizontal gradient**. Moving up any single column (increasing income)
takes you from ~\$68 to ~\$130+, a massive, consistent climb. Moving
across any single row (changing age, holding income roughly constant)
barely moves the number at all, most rows vary by only \$1–3 across all
six age brackets.

**This answers Question 5 directly, but not with a “yes” to the
hidden-interaction hypothesis**: there’s no special age × income
combination secretly driving revenue. Instead, the heatmap reveals
something simpler and just as useful, **income is doing essentially all
the work, and age is doing almost none**. This lines up with what we
already suspected from Step 1’s insight that `Age` showed a flat,
uniform distribution with near-zero correlation to `PurchaseAmount` on
its own; this chart confirms that flatness holds true even *within*
every income level, not just in aggregate.

**One cell worth a second look, but with a caveat**: the top-right cell
(\$137, for ages 55.3–64.7 in the highest income bracket) sits
noticeably above its row-neighbors, which are otherwise tightly
clustered around \$124. That could hint at a genuine “older,
high-income” premium segment, but the top income bracket almost
certainly has the smallest sample size of any cell in this grid (high
earners are rarer), so a single elevated cell there could easily be
noise rather than a real interaction.

**Marketing implication**: since income is clearly the dominant driver
and age contributes essentially nothing, segmentation and targeting
strategies should prioritize income tier over age group, age-based
marketing personas may not be adding much predictive value here.

# Step 3: Outlier Detection

Before moving into regression modeling, it’s worth taking a closer, more
formal look at outliers, since they matter more for a linear regression
model than almost any other analysis step so far. Ordinary least squares
regression fits a line by *minimizing squared residuals*, which means a
single extreme value can pull the entire fitted line toward it far more
aggressively than it would under a method that doesn’t square its
errors. In practice, this shows up in several ways:

- **Inflated or deflated coefficients**, a handful of extreme points can
  shift a slope enough to misrepresent the relationship for the other
  99% of the data
- **Violated assumptions**, outliers are a common cause of
  heteroscedasticity (non-constant error variance) and non-normal
  residuals, both of which we’ll test formally in Part B
- **Misleading R-squared**, a model can appear to fit well overall while
  badly misfitting the bulk of ordinary observations, simply because a
  few extreme points anchor the fit
- **Reduced generalizability**, a model overly influenced by rare
  extreme cases may perform poorly on new, typical customers, which is
  the opposite of what a business wants from a predictive model

Because of this, choosing the *right way* to detect outliers matters
almost as much as detecting them at all. Outlier detection methods
generally fall into two families, and which one is appropriate depends
on the shape of each variable’s distribution:

- **Z-score method**, flags a value as an outlier if it falls more than
  a fixed number of standard deviations (typically ±3) from the mean.
  This method implicitly assumes the variable is **approximately
  normally distributed**, since it relies on the mean and standard
  deviation as meaningful reference points. Applying it to a skewed
  variable can badly under, or over-flag outliers, since the mean itself
  is pulled by the skew.
- **IQR (interquartile range) method**, flags a value as an outlier if
  it falls more than 1.5 times the interquartile range below the first
  quartile or above the third. This method is **distribution- agnostic**
  and far more robust to skewed data, since it’s built on quartiles
  rather than the mean.

This is exactly why **skewness** is checked first in this section,
essentially a numeric extension of the visual distribution shapes we
already saw in Step 1’s histograms (recall the clear right-skew in
`TimeOnSite`, `Clicks`, `WebsiteVisits`, and `EmailOpens`, versus the
closer-to-normal shape of `Income` and `PurchaseAmount`). A skewness
value near 0 is a strong indicator a variable is close to normally
distributed, making the z-score method appropriate; a skewness value
meaningfully above or below 0 signals the distribution is lopsided, in
which case the **IQR method** is the safer, more defensible choice.

The workflow for this section, then, is:

1.  Calculate skewness for every continuous variable
2.  For variables close to normal (low skewness), apply the **z-score**
    method for outlier detection
3.  For variables that are meaningfully skewed, apply the **IQR** method
    instead
4.  Report outlier counts per variable, and flag (without necessarily
    removing) any variable with a high proportion of outliers before
    moving into regression modeling

**Skewness Calculation plus Conditional Method Selection**

``` r
library(e1071)

skew_vals <- sapply(df[, continuous_vars], skewness)

count_outliers_zscore <- function(x) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  sum(abs(z) > 3, na.rm = TRUE)
}

count_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE); q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr), na.rm = TRUE)
}

skew_threshold <- 0.5  # |skewness| below this is treated as "near-normal"

outlier_results <- data.frame(Variable = continuous_vars, Skewness = round(skew_vals, 3)) %>%
  mutate(
    Method = ifelse(abs(Skewness) < skew_threshold, "Z-score (near-normal)", "IQR (skewed)"),
    Outliers = mapply(function(v, m) {
      x <- df[[v]]
      if (m == "Z-score (near-normal)") count_outliers_zscore(x) else count_outliers_iqr(x)
    }, Variable, Method),
    Pct = round(Outliers / nrow(df) * 100, 2)
  ) %>%
  arrange(desc(Outliers))

print(outlier_results, row.names = FALSE)
```

    ##           Variable Skewness                Method Outliers  Pct
    ##             Clicks    0.577          IQR (skewed)       53 1.06
    ##      WebsiteVisits    0.295 Z-score (near-normal)       16 0.32
    ##     PurchaseAmount    0.032 Z-score (near-normal)       10 0.20
    ##             Income    0.059 Z-score (near-normal)        6 0.12
    ##         TimeOnSite    0.117 Z-score (near-normal)        6 0.12
    ##                Age   -0.013 Z-score (near-normal)        0 0.00
    ##        PagesViewed    0.034 Z-score (near-normal)        0 0.00
    ##      AdImpressions   -0.008 Z-score (near-normal)        0 0.00
    ##         EmailOpens    0.020 Z-score (near-normal)        0 0.00
    ##  SatisfactionScore   -0.034 Z-score (near-normal)        0 0.00

**Visualization: Skewness plus Resulting Outlier Counts, side by sid**

``` r
library(patchwork)

results_sorted1 <- outlier_results %>% mutate(Variable = factor(Variable, levels = Variable[order(Skewness)]))

p1 <- ggplot(results_sorted1, aes(x = Skewness, y = Variable, color = Method)) +
  geom_segment(aes(x = 0, xend = Skewness, y = Variable, yend = Variable), linewidth = 1.1, alpha = 0.7) +
  geom_point(size = 4) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "grey60", linewidth = 0.4) +
  scale_color_manual(values = c("Z-score (near-normal)" = "#A7D3A0", "IQR (skewed)" = "#F4B8B0")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Skewness by Variable", subtitle = "Dashed lines mark the |0.5| near-normal threshold",
       x = "Skewness", y = NULL, color = "Method Triggered")

results_sorted2 <- outlier_results %>% mutate(Variable = factor(Variable, levels = Variable[order(Outliers)]))

p2 <- ggplot(results_sorted2, aes(x = Outliers, y = Variable, fill = Method)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = Outliers), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Z-score (near-normal)" = "#A7D3A0", "IQR (skewed)" = "#F4B8B0")) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, max(results_sorted2$Outliers) * 1.25) +
  labs(title = "Outliers Detected", subtitle = "Using the method matched to each distribution shape",
       x = "Outlier Count", y = NULL)

p1 + p2 + plot_annotation(
  title = "Step 3: Outlier Detection. Kewness-Guided Method Selection",
  theme = theme(plot.title = element_text(size = 15, face = "bold"))
)
```

![](Marketing-Analytics-Regression_files/figure-gfm/step3-outlier-viz-1.png)<!-- -->

## Step 3: Insights from Outlier Detection

**Almost every variable in this dataset is close to symmetric.** Only
one variable, `Clicks` (skewness = 0.577), crossed the ±0.5 threshold
into “skewed” territory and triggered the IQR method. Every other
variable, including `TimeOnSite` (skewness = 0.117), which showed a
visibly right-skewed histogram back in Step 1, turned out to be much
closer to normal than expected once measured numerically. This is a
useful reminder that a histogram’s *visual* shape can look more skewed
than the actual skewness statistic supports, especially with a large
sample size, which is exactly why we calculate skewness formally instead
of relying on eyeballing the shape alone.

**Overall outlier prevalence is very low across the board.** The highest
outlier count belongs to `Clicks` at just 53 rows (1.06% of the
dataset), and five variables (`Age`, `PagesViewed`, `AdImpressions`,
`EmailOpens`, `SatisfactionScore`) show **zero** outliers under either
method. This is a genuinely reassuring result heading into regression
modeling: with no variable showing more than roughly 1% extreme values,
outliers are unlikely to be a dominant force distorting model
coefficients later in Part A.

**A caution on `PurchaseAmount` specifically.** Even though its outlier
count is small (10 rows, 0.20%), this is the **dependent variable**, any
outlier here isn’t just a data quirk to note in passing, it directly
shapes the regression line itself, since OLS minimizes *squared*
residuals and a handful of extreme purchase values can pull the fitted
line disproportionately toward them. Before deciding whether to keep,
transform, or investigate these 10 rows, it’s worth looking at them
individually: are they legitimately large purchases from real high-value
customers, or do they look like data entry errors (e.g., an extra zero)?
If they’re genuine, they likely represent real revenue behavior worth
preserving in the model rather than treating as noise.

**A similar caution applies to `Clicks` and `WebsiteVisits`.** These
outliers may not be errors at all; they could represent a small segment
of unusually engaged users, the “power users” who click and browse far
more than a typical customer. Removing them purely because they’re
statistical outliers risks discarding exactly the behavioral signal a
marketing team might care about most (e.g., identifying what drives high
engagement). The safer approach, consistent with the reasoning from Step
3’s introduction, is to **flag these counts and inspect the affected
rows, not to delete them automatically**, a decision to exclude any of
them should be justified by evidence of a data error, not just by the
statistical rule that flagged them.

**Net takeaway for Part A:** with such low outlier prevalence overall,
this dataset does not appear to have a systemic outlier problem that
would force an early transformation or exclusion strategy. Any decision
to treat specific rows differently should be targeted and well-justified
(especially for `PurchaseAmount`), rather than applying a blanket
removal rule across the dataset.

# Part B: Diagnostic Testing

*(To fill in together 014 linearity, normality, homoscedasticity,
multicollinearity, influential observations.)*
