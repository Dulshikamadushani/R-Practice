19th Feb inclass
================
Dulshika Dassanayake
2026-02-19

Endophytic fungi paper (Noel et al. 2022):
<https://doi.org/10.1094/PDIS-06-21-1253-RE>

## R Markdown

1.  Explain the following

<!-- -->

1.  YAML header The YAML header is the metadata block at the very top of
    an R Markdown file (between — lines). It controls document settings
    such as the title, author(s), date, output format (HTML/PDF/Word),
    and options like a table of contents

2.  Literate programming Literate programming means writing code
    together with explanations (in text) so the document reads like a
    report. In R Markdown, mix narrative text + code chunks, and when
    knit, the code runs and outputs (plots/tables) are embedded
    automatically.

### 2.

``` r
library(tidyverse)   
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.1     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggpubr)      
library(rstatix)
```

    ## 
    ## Attaching package: 'rstatix'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
toxin.data <- read.csv("MycotoxinData.csv", na.strings = "na")
toxin.data <- toxin.data[!is.na(toxin.data$DON), ]
toxin.data$Treatment <- factor(toxin.data$Treatment)
toxin.data$Cultivar  <- factor(toxin.data$Cultivar)
toxin.data$Treatment <- factor(
  toxin.data$Treatment,
  levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70")
)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cultivar_cols <- cbbPalette[c(2, 3)]
```

``` r
p_DON <- ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    shape = 21,
    color = "black",
    size = 2,
    alpha = 0.6,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
  ) +
  scale_fill_manual(values = cultivar_cols) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)


p_DON
```

![](Coding-challenge-4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### p_seedmass

``` r
p_15ADON <- ggplot(toxin.data, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    shape = 21,
    color = "black",
    size = 2,
    alpha = 0.6,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
  ) +
  scale_fill_manual(values = cultivar_cols) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)

p_15ADON
```

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Coding-challenge-4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
p_seedmass <- ggplot(toxin.data, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    shape = 21,
    color = "black",
    size = 2,
    alpha = 0.6,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
  ) +
  scale_fill_manual(values = cultivar_cols) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)

p_seedmass
```

![](Coding-challenge-4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### combine all three figures

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Coding-challenge-4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
library(ggplot2)
library(ggpubr)
```

``` r
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
```

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Coding-challenge-4_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
