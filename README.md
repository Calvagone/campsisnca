
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

    ## # A tibble: 8,450 x 18
    ##       id  time    KA    CL    V2    V3     Q    S2   ARM     F    CP OBS_CP
    ##    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1     1     0  1.07  5.81  72.5  21.3  3.62  72.5     0  0     0      0   
    ##  2     1     1  1.07  5.81  72.5  21.3  3.62  72.5     0  8.40  8.40   9.30
    ##  3     1     2  1.07  5.81  72.5  21.3  3.62  72.5     0 10.3  10.3    9.05
    ##  4     1     3  1.07  5.81  72.5  21.3  3.62  72.5     0 10.2  10.2   10.6 
    ##  5     1     4  1.07  5.81  72.5  21.3  3.62  72.5     0  9.48  9.48   9.78
    ##  6     1     5  1.07  5.81  72.5  21.3  3.62  72.5     0  8.67  8.67   8.56
    ##  7     1     6  1.07  5.81  72.5  21.3  3.62  72.5     0  7.91  7.91   8.58
    ##  8     1     7  1.07  5.81  72.5  21.3  3.62  72.5     0  7.23  7.23   7.48
    ##  9     1     8  1.07  5.81  72.5  21.3  3.62  72.5     0  6.64  6.64   5.92
    ## 10     1     9  1.07  5.81  72.5  21.3  3.62  72.5     0  6.12  6.12   6.81
    ## # ... with 8,440 more rows, and 6 more variables: Y <dbl>, A_DEPOT <dbl>,
    ## #   A_CENTRAL <dbl>, A_PERIPHERAL <dbl>, A_OUTPUT <dbl>, BW <dbl>

Let’s calculate PK metrics at Day 1 and Day 7 as follows:

``` r
# Day 1
ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1"))
ncaD1 <- ncaD1 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
ncaD1 <- ncaD1 %>% calculate()

# Day 7 
ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168), variable="Y", scenario=c(day="Day 7"))
ncaD7 <- ncaD7 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
ncaD7 <- ncaD7 %>% calculate()
```

These 2 metrics may be imported into a metrics table object, as follows:

``` r
table <- NCAMetricsTable()  
table <- table %>% add(c(ncaD1, ncaD7))
```

This table can be exported to a dataframe using the `export` function:

``` r
table %>% export(dest="dataframe")
```

    ## # A tibble: 8 x 5
    ##   metric     low    med     up day  
    ##   <chr>    <dbl>  <dbl>  <dbl> <chr>
    ## 1 AUC      99.1  132.   165.   Day 1
    ## 2 Cmax      8.54  10.4   14.0  Day 1
    ## 3 tmax      1.45   3      5.55 Day 1
    ## 4 Ctrough   1.34   2.60   3.91 Day 1
    ## 5 AUC     125.   184.   263.   Day 7
    ## 6 Cmax     11.3   14.9   18.7  Day 7
    ## 7 tmax    145    147    150.   Day 7
    ## 8 Ctrough   1.57   4.09   6.69 Day 7

Or to a HTML table using `kable`:

``` r
table %>% export(dest="kable")
```

<table class=" lightable-paper lightable-striped" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
AUC
</th>
<th style="text-align:left;">
Cmax
</th>
<th style="text-align:left;">
tmax
</th>
<th style="text-align:left;">
Ctrough
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Day 1
</td>
<td style="text-align:left;">
132 [99-165]
</td>
<td style="text-align:left;">
10 [9-14]
</td>
<td style="text-align:left;">
3 [1-6]
</td>
<td style="text-align:left;">
3 [1-4]
</td>
</tr>
<tr>
<td style="text-align:left;">
Day 7
</td>
<td style="text-align:left;">
184 [125-263]
</td>
<td style="text-align:left;">
15 [11-19]
</td>
<td style="text-align:left;">
147 [145-150]
</td>
<td style="text-align:left;">
4 [2-7]
</td>
</tr>
</tbody>
</table>

### Example 2: PK metrics at Day 1 and Day 7 for different body weight ranges

``` r
library(dplyr)
campsis_bw_50_75 <- campsis %>% filter(BW > 50 & BW < 75)
campsis_bw_75_100 <- campsis %>% filter(BW >= 75 & BW < 100)

ncaD1_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1", bw_range="BW range: 50-75"))
ncaD1_a <- ncaD1_a %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
ncaD1_a <- ncaD1_a %>% calculate()

ncaD7_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(144, 168), variable="Y", scenario=c(day="Day 7", bw_range="BW range: 50-75"))
ncaD7_a <- ncaD7_a %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
ncaD7_a <- ncaD7_a %>% calculate()

ncaD1_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1", bw_range="BW range: 75-100"))
ncaD1_b <- ncaD1_b %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
ncaD1_b <- ncaD1_b %>% calculate()

ncaD7_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(144, 168), variable="Y", scenario=c(day="Day 7", bw_range="BW range: 75-100"))
ncaD7_b <- ncaD7_b %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
ncaD7_b <- ncaD7_b %>% calculate()

table <- NCAMetricsTable()  
table <- table %>% add(c(ncaD1_a, ncaD7_a, ncaD1_b, ncaD7_b))
table %>% export(dest="kable")
```

<table class=" lightable-paper lightable-striped" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
AUC
</th>
<th style="text-align:left;">
Cmax
</th>
<th style="text-align:left;">
tmax
</th>
<th style="text-align:left;">
Ctrough
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
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
BW range: 50-75
</td>
<td style="text-align:left;">
144 [119-169]
</td>
<td style="text-align:left;">
11 [9-14]
</td>
<td style="text-align:left;">
4 [2-6]
</td>
<td style="text-align:left;">
3 [2-4]
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
BW range: 75-100
</td>
<td style="text-align:left;">
126 [97-146]
</td>
<td style="text-align:left;">
10 [8-13]
</td>
<td style="text-align:left;">
3 [1-5]
</td>
<td style="text-align:left;">
2 [1-3]
</td>
</tr>
<tr grouplength="2">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Day 7</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
BW range: 50-75
</td>
<td style="text-align:left;">
210 [161-274]
</td>
<td style="text-align:left;">
17 [14-19]
</td>
<td style="text-align:left;">
147 [145-150]
</td>
<td style="text-align:left;">
5 [3-7]
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
BW range: 75-100
</td>
<td style="text-align:left;">
170 [120-214]
</td>
<td style="text-align:left;">
14 [11-17]
</td>
<td style="text-align:left;">
147 [145-149]
</td>
<td style="text-align:left;">
3 [2-5]
</td>
</tr>
</tbody>
</table>

### Example 3: Calculate 2-compartment half-life metrics

``` r
nca <- NCAMetrics(x=campsis %>% mutate(DOSE=1000, TAU=24), variable="Y", scenario=c(xx="Half-lives"))
nca <- nca %>% add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z()))
nca <- nca %>% calculate()

table <- NCAMetricsTable()  
table <- table %>% add(nca)
table %>% export(dest="kable")
```

<table class=" lightable-paper lightable-striped" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
t1/2dist
</th>
<th style="text-align:left;">
t1/2eff
</th>
<th style="text-align:left;">
t1/2z
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Half-lives
</td>
<td style="text-align:left;">
3 [2-3]
</td>
<td style="text-align:left;">
13 [8-17]
</td>
<td style="text-align:left;">
14 [10-19]
</td>
</tr>
</tbody>
</table>
