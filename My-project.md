My project
================
Rainbow
2024-10-11

``` r
library(haven)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(bruceR)
```

    ## 
    ## bruceR (v2024.6)
    ## Broadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## ✔ data.table ✔ emmeans
    ## ✔ dplyr      ✔ lmerTest
    ## ✔ tidyr      ✔ effectsize
    ## ✔ stringr    ✔ performance
    ## ✔ ggplot2    ✔ interactions
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## For full functionality, please install all dependencies:
    ## install.packages("bruceR", dep=TRUE)
    ## 
    ## Online documentation:
    ## https://psychbruce.github.io/bruceR
    ## 
    ## To use this package in publications, please cite:
    ## Bao, H.-W.-S. (2024). bruceR: Broadly useful convenient and efficient R functions (Version 2024.6) [Computer software]. https://CRAN.R-project.org/package=bruceR

    ## 
    ## These packages are dependencies of `bruceR` but not installed:
    ## - pacman, openxlsx, ggtext, lmtest, vars, phia, MuMIn, GGally
    ## 
    ## ***** Install all dependencies *****
    ## install.packages("bruceR", dep=TRUE)

``` r
library(ggsci)
library(see)
```

    ## 
    ## Attaching package: 'see'

    ## The following objects are masked from 'package:ggsci':
    ## 
    ##     scale_color_material, scale_colour_material, scale_fill_material

``` r
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:psych':
    ## 
    ##     logit

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

``` r
dataset <- read.csv("/Users/rainbow/Documents/project\ dataset.csv")
```

\#Recode variables

``` r
dataset <- dataset %>%
  mutate_at(c('gender'),funs(str_replace(., "1", "Men")))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
dataset <- dataset %>%
  mutate_at(c('gender'),funs(str_replace(., "2", "Women")))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

\#Normality \##Normality plots

``` r
ggplot(dataset, aes(x = Self_esteem)) + geom_histogram(binwidth = 5) + theme_classic()
```

![](My-project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(dataset, aes(x = Self_esteem)) + geom_density(adjust = 2)  + theme_classic()
```

![](My-project_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
qq<-ggplot(dataset, aes(sample = Self_esteem)) + geom_qq()  + theme_classic()

qq+ geom_qq_line()
```

![](My-project_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->
\###Normality plots by gender

``` r
ggplot(dataset, aes(x = Self_esteem)) + geom_histogram(binwidth = 5) + theme_classic() + facet_wrap(~gender) + theme_classic()
```

![](My-project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(dataset, aes(x = Self_esteem)) + geom_density(adjust = 2)  + theme_classic() + facet_wrap(~gender) + theme_classic()
```

![](My-project_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
qq<-ggplot(dataset, aes(sample = Self_esteem)) + geom_qq()  + theme_classic() + facet_wrap(~gender) + theme_classic()

qq+ geom_qq_line()+ facet_wrap(~gender) 
```

![](My-project_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->
\##Normality tests

``` r
describe(dataset$Self_esteem)
```

    ##    vars   n  mean   sd median trimmed  mad min max range skew kurtosis  se
    ## X1    1 104 38.34 5.08     38    38.4 5.93  27  49    22 -0.1    -0.74 0.5

``` r
shapiro.test(dataset$Self_esteem)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dataset$Self_esteem
    ## W = 0.98194, p-value = 0.1684

\###Normality tests by gender

``` r
?describeBy()

describeBy(Self_esteem ~ gender, data = dataset)
```

    ## 
    ##  Descriptive statistics by group 
    ## gender: Men
    ##             vars  n mean   sd median trimmed  mad min max range  skew kurtosis
    ## Self_esteem    1 46 38.7 5.26     38   38.74 6.67  27  47    20 -0.02    -1.03
    ##               se
    ## Self_esteem 0.78
    ## ------------------------------------------------------------ 
    ## gender: Women
    ##             vars  n  mean   sd median trimmed  mad min max range  skew kurtosis
    ## Self_esteem    1 58 38.05 4.96     38   38.17 4.45  27  49    22 -0.18     -0.6
    ##               se
    ## Self_esteem 0.65

``` r
dataset %>%
  group_by(gender) %>%
  summarize(W = shapiro.test(Self_esteem)$statistic, p_value = shapiro.test(Self_esteem)$p.value)
```

    ## # A tibble: 2 × 3
    ##   gender     W p_value
    ##   <chr>  <dbl>   <dbl>
    ## 1 Men    0.957  0.0875
    ## 2 Women  0.986  0.729

# Equal Variance between Groups

## Descrptive Variance

``` r
data_clean<-drop_na(dataset)

var(data_clean$Self_esteem)
```

    ## [1] 25.81768

``` r
data_clean %>%
  group_by(gender) %>%
  summarize(variance = var(Self_esteem))
```

    ## # A tibble: 2 × 2
    ##   gender variance
    ##   <chr>     <dbl>
    ## 1 Men        27.6
    ## 2 Women      24.6

## Equal Variance Test

``` r
leveneTest(Self_esteem~gender, data_clean)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##        Df F value Pr(>F)
    ## group   1  0.6845   0.41
    ##       102

``` r
MANOVA(data_clean, dv = "Self_esteem", between = "gender")
```

    ## 
    ## ====== ANOVA (Between-Subjects Design) ======
    ## 
    ## Descriptives:
    ## ───────────────────────────
    ##  "gender"   Mean    S.D.  n
    ## ───────────────────────────
    ##     Men   38.696 (5.257) 46
    ##     Women 38.052 (4.965) 58
    ## ───────────────────────────
    ## Total sample size: N = 104
    ## 
    ## ANOVA Table:
    ## Dependent variable(s):      Self_esteem
    ## Between-subjects factor(s): gender
    ## Within-subjects factor(s):  –
    ## Covariate(s):               –
    ## ──────────────────────────────────────────────────────────────────────
    ##             MS    MSE df1 df2     F     p     η²p [90% CI of η²p]  η²G
    ## ──────────────────────────────────────────────────────────────────────
    ## gender  10.637 25.967   1 102 0.410  .524       .004 [.000, .048] .004
    ## ──────────────────────────────────────────────────────────────────────
    ## MSE = mean square error (the residual variance of the linear model)
    ## η²p = partial eta-squared = SS / (SS + SSE) = F * df1 / (F * df1 + df2)
    ## ω²p = partial omega-squared = (F - 1) * df1 / (F * df1 + df2 + 1)
    ## η²G = generalized eta-squared (see Olejnik & Algina, 2003)
    ## Cohen’s f² = η²p / (1 - η²p)
    ## 
    ## Levene’s Test for Homogeneity of Variance:
    ## ─────────────────────────────────────────────
    ##                  Levene’s F df1 df2     p    
    ## ─────────────────────────────────────────────
    ## DV: Self_esteem       0.807   1 102  .371    
    ## ─────────────────────────────────────────────

\#Summary Descriptive Statistics

``` r
data_clean %>%
  group_by(gender) %>%
  dplyr::summarize(mean_Self_esteem    = mean(Self_esteem),
      mean_relationship_satis    = mean(relationship_satis),
      std_dev_Self_esteem = sd(Self_esteem),
      std_dev_relationship_satis = sd(relationship_satis),
      corr_Self_esteem_relationship_satis  = cor(Self_esteem, relationship_satis))
```

    ## # A tibble: 2 × 6
    ##   gender mean_Self_esteem mean_relationship_satis std_dev_Self_esteem
    ##   <chr>             <dbl>                   <dbl>               <dbl>
    ## 1 Men                38.7                    25.9                5.26
    ## 2 Women              38.1                    26.2                4.96
    ## # ℹ 2 more variables: std_dev_relationship_satis <dbl>,
    ## #   corr_Self_esteem_relationship_satis <dbl>
