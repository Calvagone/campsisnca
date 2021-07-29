---
pagetitle: "CAMPSIS NCA"
date: ""
author: ""
output: github_document
always_allow_html: true
knit: (knitr::knit)
---

# campsisnca
Analyse your simulation output using non-compartmental analysis.

## Installation
Install the latest stable release with the authentication token you have received:
  

```r
devtools::install_github("Calvagone/campsisnca", ref="main", auth_token="AUTH_TOKEN", force=TRUE)
```

## Some examples



First import the `campsisnca` package:


```r
library(campsisnca)
```

Assume some results were simulated with CAMPSIS (see `campsis` dataframe) :


```r
campsis
```

```
## # A tibble: 8,450 x 17
##       id  time    KA    CL    V2    V3     Q    S2   ARM     F    CP OBS_CP
##    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
##  1     1     0  1.07  5.37  72.5  21.3  3.62  72.5     0  0     0      0   
##  2     1     1  1.07  5.37  72.5  21.3  3.62  72.5     0  8.43  8.43   9.30
##  3     1     2  1.07  5.37  72.5  21.3  3.62  72.5     0 10.4  10.4   10.0 
##  4     1     3  1.07  5.37  72.5  21.3  3.62  72.5     0 10.3  10.3    9.52
##  5     1     4  1.07  5.37  72.5  21.3  3.62  72.5     0  9.66  9.66  11.6 
##  6     1     5  1.07  5.37  72.5  21.3  3.62  72.5     0  8.88  8.88   6.65
##  7     1     6  1.07  5.37  72.5  21.3  3.62  72.5     0  8.15  8.15   6.48
##  8     1     7  1.07  5.37  72.5  21.3  3.62  72.5     0  7.49  7.49   7.02
##  9     1     8  1.07  5.37  72.5  21.3  3.62  72.5     0  6.91  6.91   5.71
## 10     1     9  1.07  5.37  72.5  21.3  3.62  72.5     0  6.40  6.40   6.69
## # ... with 8,440 more rows, and 5 more variables: Y <dbl>, A_DEPOT <dbl>,
## #   A_CENTRAL <dbl>, A_PERIPHERAL <dbl>, A_OUTPUT <dbl>
```

Let's calculate PK metrics at Day 1 and Day 7 as follows:


```r
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


```r
table <- NCAMetricsTable()  
table <- table %>% add(c(ncaD1, ncaD7))
```

This table can be exported to a dataframe using the `export` function:


```r
table %>% export(dest="dataframe")
```

```
## # A tibble: 8 x 5
##   metric     low    med     up day  
##   <chr>    <dbl>  <dbl>  <dbl> <chr>
## 1 AUC     108.   137.   158.   Day 1
## 2 Cmax      8.03  10.7   14.5  Day 1
## 3 tmax      1.45   3      5    Day 1
## 4 Ctrough   1.78   2.95   4.00 Day 1
## 5 AUC     149.   198.   244.   Day 7
## 6 Cmax     12.2   15.3   18.2  Day 7
## 7 tmax    146    147    149    Day 7
## 8 Ctrough   2.28   4.45   6.26 Day 7
```

Or to a HTML table using `kable`:


```r
table %>% export(dest="kable")
```

<table class=" lightable-paper lightable-striped" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> AUC </th>
   <th style="text-align:left;"> Cmax </th>
   <th style="text-align:left;"> Ctrough </th>
   <th style="text-align:left;"> tmax </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Day 1 </td>
   <td style="text-align:left;"> 137 [108-158] </td>
   <td style="text-align:left;"> 11 [8-15] </td>
   <td style="text-align:left;"> 3 [2-4] </td>
   <td style="text-align:left;"> 3 [1-5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Day 7 </td>
   <td style="text-align:left;"> 198 [149-244] </td>
   <td style="text-align:left;"> 15 [12-18] </td>
   <td style="text-align:left;"> 4 [2-6] </td>
   <td style="text-align:left;"> 147 [146-149] </td>
  </tr>
</tbody>
</table>
