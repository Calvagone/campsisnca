
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

    ## # A tibble: 5,000 x 18
    ##       ID  TIME   ARM    KA    CL    V2    V3     Q    S2     F    CP OBS_CP
    ##    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1     1     0     0 0.842  5.89  94.8  21.3  3.62  94.8  0     0      0   
    ##  2     1     1     0 0.842  5.89  94.8  21.3  3.62  94.8  5.68  5.68   4.71
    ##  3     1     2     0 0.842  5.89  94.8  21.3  3.62  94.8  7.62  7.62   7.86
    ##  4     1     4     0 0.842  5.89  94.8  21.3  3.62  94.8  7.81  7.81   6.01
    ##  5     1     6     0 0.842  5.89  94.8  21.3  3.62  94.8  6.93  6.93   6.40
    ##  6     1     8     0 0.842  5.89  94.8  21.3  3.62  94.8  6.05  6.05   7.73
    ##  7     1    12     0 0.842  5.89  94.8  21.3  3.62  94.8  4.72  4.72   4.63
    ##  8     1    16     0 0.842  5.89  94.8  21.3  3.62  94.8  3.78  3.78   3.46
    ##  9     1    24     0 0.842  5.89  94.8  21.3  3.62  94.8  2.52  2.52   2.17
    ## 10     1    48     0 0.842  5.89  94.8  21.3  3.62  94.8  3.33  3.33   3.69
    ## # ... with 4,990 more rows, and 6 more variables: Y <dbl>, A_DEPOT <dbl>,
    ## #   A_CENTRAL <dbl>, A_PERIPHERAL <dbl>, A_OUTPUT <dbl>, BW <dbl>

Let’s calculate PK metrics at Day 1 and Day 7 as follows:

``` r
# Day 1
ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1"))
ncaD1 <- ncaD1 %>% add(c(Auc(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL")))
ncaD1 <- ncaD1 %>% calculate()

# Day 7 
ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7"))
ncaD7 <- ncaD7 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough()))
ncaD7 <- ncaD7 %>% calculate()
```

These 2 metrics may be imported into a metrics table object, as follows:

``` r
table <- NCAMetricsTable(unitLineBreak=TRUE)  
table <- table %>% add(c(ncaD1, ncaD7))
```

This table can be exported:

1.  To a dataframe using the `export` function:

``` r
table %>% export(dest="dataframe")
```

    ## # A tibble: 8 x 5
    ##   metric     low    med     up day  
    ##   <chr>    <dbl>  <dbl>  <dbl> <chr>
    ## 1 AUC     104.   135.   172.   Day 1
    ## 2 Cmax      7.46  10.3   14.4  Day 1
    ## 3 tmax      1      2      6    Day 1
    ## 4 Ctrough   1.60   2.85   4.59 Day 1
    ## 5 AUC     133.   197.   292.   Day 7
    ## 6 Cmax     10.9   14.9   20.0  Day 7
    ## 7 tmax      1      2      6    Day 7
    ## 8 Ctrough   1.95   4.04   8.53 Day 7

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
135 [104-172]
</td>
<td style="text-align:center;">
10.3 [7.46-14.4]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
2.85 [1.6-4.59]
</td>
</tr>
<tr>
<td style="text-align:center;">
Day 7
</td>
<td style="text-align:center;">
197 [133-292]
</td>
<td style="text-align:center;">
14.9 [10.9-20]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
4.04 [1.95-8.53]
</td>
</tr>
</tbody>
</table>

3.  To a LaTeX table using `kable` and format argument `latex`:

``` r
table %>% export(dest="kable", format="latex")
```

### Example 2: PK metrics at Day 1 and Day 7 for different body weight ranges

``` r
library(dplyr)
campsis_bw_50_75 <- campsis %>% filter(BW > 50 & BW < 75)
campsis_bw_75_100 <- campsis %>% filter(BW >= 75 & BW < 100)

scenarioD1_a <- c(day="Day 1", bw_range="BW range: 50-75")
ncaD1_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(0, 24), variable="Y", scenario=scenarioD1_a)
ncaD1_a <- ncaD1_a %>% add(c(Auc(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL")))
ncaD1_a <- ncaD1_a %>% calculate()

scenarioD7_a <- c(day="Day 7", bw_range="BW range: 50-75")
ncaD7_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(144, 168, rebase=T), variable="Y", scenario=scenarioD7_a)
ncaD7_a <- ncaD7_a %>% add(c(Auc(), Cmax(), Tmax(), Ctrough()))
ncaD7_a <- ncaD7_a %>% calculate()

scenarioD1_b <- c(day="Day 1", bw_range="BW range: 75-100")
ncaD1_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(0, 24), variable="Y", scenario=scenarioD1_b)
ncaD1_b <- ncaD1_b %>% add(c(Auc(), Cmax(), Tmax(), Ctrough()))
ncaD1_b <- ncaD1_b %>% calculate()

scenarioD7_b <- c(day="Day 7", bw_range="BW range: 75-100")
ncaD7_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(144, 168, rebase=T), variable="Y", scenario=scenarioD7_b)
ncaD7_b <- ncaD7_b %>% add(c(Auc(), Cmax(), Tmax(), Ctrough()))
ncaD7_b <- ncaD7_b %>% calculate()

table <- NCAMetricsTable(unitLineBreak=TRUE)  
table <- table %>% add(c(ncaD1_a, ncaD7_a, ncaD1_b, ncaD7_b))
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
147 [124-174]
</td>
<td style="text-align:center;">
10.4 [8.08-14.7]
</td>
<td style="text-align:center;">
4 [1.1-6]
</td>
<td style="text-align:center;">
3.32 [2.21-4.96]
</td>
</tr>
<tr>
<td style="text-align:center;padding-left: 2em;" indentlevel="1">
BW range: 75-100
</td>
<td style="text-align:center;">
123 [98.7-154]
</td>
<td style="text-align:center;">
10.2 [7.12-13.3]
</td>
<td style="text-align:center;">
2 [1-4]
</td>
<td style="text-align:center;">
2.25 [1.3-3.89]
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
234 [166-304]
</td>
<td style="text-align:center;">
16.1 [12.1-21.1]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
5.33 [2.74-9.3]
</td>
</tr>
<tr>
<td style="text-align:center;padding-left: 2em;" indentlevel="1">
BW range: 75-100
</td>
<td style="text-align:center;">
169 [125-235]
</td>
<td style="text-align:center;">
13.2 [10.2-19.4]
</td>
<td style="text-align:center;">
2 [1-6]
</td>
<td style="text-align:center;">
3.18 [1.8-5.44]
</td>
</tr>
</tbody>
</table>

### Example 3: Calculate 2-compartment half-life metrics

``` r
nca <- NCAMetrics(x=campsis %>% mutate(DOSE=1000, TAU=24), variable="Y", scenario=c(xx="Theoritical half-lives"))
nca <- nca %>% add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z()))
nca <- nca %>% calculate()

table <- NCAMetricsTable()  
table <- table %>% add(nca)
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
nca <- NCAMetrics(x=campsis, variable="Y", scenario=c(xx="Half-live computed on data"))
nca <- nca %>% add(c(Thalf(x=campsis %>% timerange(7*24, 10*24))))
nca <- nca %>% calculate()

table <- NCAMetricsTable()  
table <- table %>% add(nca)
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
14.7 [9.72-22.4]
</td>
</tr>
</tbody>
</table>
