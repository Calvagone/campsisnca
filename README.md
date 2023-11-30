
# campsisnca

Analyse your simulation output using non-compartmental analysis.

## Installation

Install the latest stable release with the authentication token you have
received:

``` r
devtools::install_github("Calvagone/campsisnca", ref="main", auth_token="AUTH_TOKEN", force=TRUE)
```

## Basic use

First import the `campsisnca` package:

``` r
library(campsisnca)
```

### Example 1: PK metrics at Day 1 and Day 7

Assume some results were simulated with CAMPSIS (see `campsis`
dataframe) :

``` r
campsis
```

    ## # A tibble: 5,000 × 18
    ##       ID  TIME   ARM    KA    CL    V2    V3     Q    S2     F    CP OBS_CP
    ##    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1     1     0     0 0.842  5.89  94.8  21.3  3.62  94.8  0     0      0   
    ##  2     1     1     0 0.842  5.89  94.8  21.3  3.62  94.8  5.68  5.68   6.33
    ##  3     1     2     0 0.842  5.89  94.8  21.3  3.62  94.8  7.62  7.62   8.55
    ##  4     1     4     0 0.842  5.89  94.8  21.3  3.62  94.8  7.81  7.81   8.16
    ##  5     1     6     0 0.842  5.89  94.8  21.3  3.62  94.8  6.93  6.93   9.24
    ##  6     1     8     0 0.842  5.89  94.8  21.3  3.62  94.8  6.05  6.05   5.63
    ##  7     1    12     0 0.842  5.89  94.8  21.3  3.62  94.8  4.72  4.72   4.86
    ##  8     1    16     0 0.842  5.89  94.8  21.3  3.62  94.8  3.78  3.78   4.52
    ##  9     1    24     0 0.842  5.89  94.8  21.3  3.62  94.8  2.52  2.52   2.87
    ## 10     1    48     0 0.842  5.89  94.8  21.3  3.62  94.8  3.33  3.33   3.75
    ## # ℹ 4,990 more rows
    ## # ℹ 6 more variables: Y <dbl>, A_DEPOT <dbl>, A_CENTRAL <dbl>,
    ## #   A_PERIPHERAL <dbl>, A_OUTPUT <dbl>, BW <dbl>

Let’s calculate PK metrics at Day 1 and Day 7 as follows:

``` r
# Day 1
ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
  add(c(Auc(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL"))) %>%
  calculate()

# Day 7 
ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
  add(c(Auc(), Cmax(), Tmax(), Ctrough())) %>%
  calculate()
```

These 2 metrics may be imported into a metrics table object, as follows:

``` r
table <- NCAMetricsTable(unitLineBreak=TRUE)  
table <- table %>%
  add(c(ncaD1, ncaD7))
```

This table can be exported:

1.  To a dataframe using the `export` function:

``` r
table %>% export(dest="dataframe")
```

    ## # A tibble: 8 × 5
    ##   metric     low    med     up day  
    ##   <chr>    <dbl>  <dbl>  <dbl> <chr>
    ## 1 AUC     102.   134.   168.   Day 1
    ## 2 Cmax      7.37  10.1   13.4  Day 1
    ## 3 tmax      1      4      6    Day 1
    ## 4 Ctrough   1.42   2.88   4.49 Day 1
    ## 5 AUC     131.   197.   297.   Day 7
    ## 6 Cmax     10.9   14.7   19.2  Day 7
    ## 7 tmax      1      2      6    Day 7
    ## 8 Ctrough   1.90   4.11   8.59 Day 7

2.  To a HTML table using `kable` and format argument `html`:

``` r
table %>% export(dest="kable", format="html")
```

<table class=" lightable-paper lightable-striped table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
</th>
<th style="text-align:center;">
AUC<br>(ng/mL*h)
</th>
<th style="text-align:center;">
Cmax<br>(ng/mL)
</th>
<th style="text-align:center;">
tmax<br>(h)
</th>
<th style="text-align:center;">
Ctrough<br>(ng/mL)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Day 1
</td>
<td style="text-align:center;">
134 [102-168]
</td>
<td style="text-align:center;">
10.1 [7.37-13.4]
</td>
<td style="text-align:center;">
4 [1-6]
</td>
<td style="text-align:center;">
2.88 [1.42-4.49]
</td>
</tr>
<tr>
<td style="text-align:center;">
Day 7
</td>
<td style="text-align:center;">
197 [131-297]
</td>
<td style="text-align:center;">
14.7 [10.9-19.2]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
4.11 [1.9-8.59]
</td>
</tr>
</tbody>
</table>

3.  To a LaTeX table using `kable` and format argument `latex`:

``` r
table %>% export(dest="kable", format="latex")
```

Please note the individual metrics can also be exported to a dataframe
using the `export` function as follows:

``` r
table %>% export(dest="dataframe", type="individual")
```

    ## # A tibble: 1,600 × 4
    ##    metric    id value day  
    ##    <chr>  <int> <dbl> <chr>
    ##  1 AUC        1 129.  Day 1
    ##  2 AUC        2  95.4 Day 1
    ##  3 AUC        3 115.  Day 1
    ##  4 AUC        4 124.  Day 1
    ##  5 AUC        5 165.  Day 1
    ##  6 AUC        6 152.  Day 1
    ##  7 AUC        7 137.  Day 1
    ##  8 AUC        8 143.  Day 1
    ##  9 AUC        9 110.  Day 1
    ## 10 AUC       10 135.  Day 1
    ## # ℹ 1,590 more rows

### Example 2: PK metrics at Day 1 and Day 7 for different body weight ranges

``` r
library(dplyr)
campsis_bw_50_75 <- campsis %>% filter(BW > 50 & BW < 75)
campsis_bw_75_100 <- campsis %>% filter(BW >= 75 & BW < 100)

scenarioD1_a <- c(day="Day 1", bw_range="BW range: 50-75")
ncaD1_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(0, 24), variable="Y", scenario=scenarioD1_a) %>% 
  add(c(Auc(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL"))) %>%
  calculate()

scenarioD7_a <- c(day="Day 7", bw_range="BW range: 50-75")
ncaD7_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(144, 168, rebase=T), variable="Y", scenario=scenarioD7_a) %>%
  add(c(Auc(), Cmax(), Tmax(), Ctrough())) %>%
  calculate()

scenarioD1_b <- c(day="Day 1", bw_range="BW range: 75-100")
ncaD1_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(0, 24), variable="Y", scenario=scenarioD1_b) %>%
  add(c(Auc(), Cmax(), Tmax(), Ctrough())) %>%
  calculate()

scenarioD7_b <- c(day="Day 7", bw_range="BW range: 75-100")
ncaD7_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(144, 168, rebase=T), variable="Y", scenario=scenarioD7_b) %>%
  add(c(Auc(), Cmax(), Tmax(), Ctrough())) %>%
  calculate()

table <- NCAMetricsTable(unitLineBreak=TRUE) %>%
  add(c(ncaD1_a, ncaD7_a, ncaD1_b, ncaD7_b))
table %>% export(dest="kable", format="html")
```

<table class=" lightable-paper lightable-striped table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
</th>
<th style="text-align:center;">
AUC<br>(ng/mL*h)
</th>
<th style="text-align:center;">
Cmax<br>(ng/mL)
</th>
<th style="text-align:center;">
tmax<br>(h)
</th>
<th style="text-align:center;">
Ctrough<br>(ng/mL)
</th>
</tr>
</thead>
<tbody>
<tr grouplength="2">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Day 1</strong>
</td>
</tr>
<tr>
<td style="text-align:center;padding-left: 2em;" indentlevel="1">
BW range: 50-75
</td>
<td style="text-align:center;">
147 [122-172]
</td>
<td style="text-align:center;">
10.4 [7.77-12.8]
</td>
<td style="text-align:center;">
4 [2-6]
</td>
<td style="text-align:center;">
3.46 [1.94-4.82]
</td>
</tr>
<tr>
<td style="text-align:center;padding-left: 2em;" indentlevel="1">
BW range: 75-100
</td>
<td style="text-align:center;">
123 [98.8-151]
</td>
<td style="text-align:center;">
10.1 [7.09-13.5]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
2.39 [1.24-3.68]
</td>
</tr>
<tr grouplength="2">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Day 7</strong>
</td>
</tr>
<tr>
<td style="text-align:center;padding-left: 2em;" indentlevel="1">
BW range: 50-75
</td>
<td style="text-align:center;">
227 [167-311]
</td>
<td style="text-align:center;">
16.4 [12.9-19.8]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
5.5 [2.72-9.28]
</td>
</tr>
<tr>
<td style="text-align:center;padding-left: 2em;" indentlevel="1">
BW range: 75-100
</td>
<td style="text-align:center;">
169 [124-231]
</td>
<td style="text-align:center;">
13 [10.5-17.5]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
3.13 [1.51-6.19]
</td>
</tr>
</tbody>
</table>

### Example 3: Calculate 2-compartment half-life metrics

``` r
nca <- NCAMetrics(x=campsis %>% mutate(DOSE=1000, TAU=24), variable="Y", scenario=c(xx="Theoritical half-lives")) %>%
  add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z())) %>%
  calculate()

table <- NCAMetricsTable() %>%
  add(nca)
table %>% export(dest="kable", format="html")
```

<table class=" lightable-paper lightable-striped table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
</th>
<th style="text-align:center;">
t1/2dist
</th>
<th style="text-align:center;">
t1/2eff
</th>
<th style="text-align:center;">
t1/2z
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Theoritical half-lives
</td>
<td style="text-align:center;">
2.57 [1.91-3.48]
</td>
<td style="text-align:center;">
13.4 [8.38-20.8]
</td>
<td style="text-align:center;">
14.6 [9.64-22.2]
</td>
</tr>
</tbody>
</table>

### Example 4: Compute terminal half-live based on data

``` r
nca <- NCAMetrics(x=campsis, variable="Y", scenario=c(xx="Half-live computed on data")) %>%
  add(c(Thalf(x=campsis %>% timerange(7*24, 10*24)))) %>%
  calculate()

table <- NCAMetricsTable() %>%
  add(nca)
table %>% export(dest="kable", format="html")
```

<table class=" lightable-paper lightable-striped table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
</th>
<th style="text-align:center;">
t1/2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Half-live computed on data
</td>
<td style="text-align:center;">
14.7 [9.79-23.4]
</td>
</tr>
</tbody>
</table>
