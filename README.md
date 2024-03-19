
# campsisnca

Analyse your simulation output using non-compartmental analysis.

## Installation

Install the latest stable release as follows:

``` r
devtools::install_github("Calvagone/campsisnca")
```

## Basic use

First import the `campsisnca` and `gtsummary` packages as follows:

``` r
library(campsisnca)
library(gtsummary)
```

### Example 1: PK metrics at Day 1 and Day 7

Assume some results were simulated with Campsis (see `campsis`
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

    ## # A tibble: 24 × 4
    ##    metric  stat    value day  
    ##    <chr>   <chr>   <dbl> <chr>
    ##  1 AUC     median 134.   Day 1
    ##  2 AUC     p5     102.   Day 1
    ##  3 AUC     p95    168.   Day 1
    ##  4 Cmax    median  10.1  Day 1
    ##  5 Cmax    p5       7.37 Day 1
    ##  6 Cmax    p95     13.4  Day 1
    ##  7 tmax    median   4    Day 1
    ##  8 tmax    p5       1    Day 1
    ##  9 tmax    p95      6    Day 1
    ## 10 Ctrough median   2.88 Day 1
    ## # ℹ 14 more rows

2.  To a HTML table using `gt`:

``` r
table %>% export(dest="gt", subscripts=TRUE)
```

<div id="dgabwknrsa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dgabwknrsa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dgabwknrsa thead, #dgabwknrsa tbody, #dgabwknrsa tfoot, #dgabwknrsa tr, #dgabwknrsa td, #dgabwknrsa th {
  border-style: none;
}
&#10;#dgabwknrsa p {
  margin: 0;
  padding: 0;
}
&#10;#dgabwknrsa .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dgabwknrsa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#dgabwknrsa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#dgabwknrsa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#dgabwknrsa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#dgabwknrsa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dgabwknrsa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dgabwknrsa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#dgabwknrsa .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dgabwknrsa .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#dgabwknrsa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#dgabwknrsa .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dgabwknrsa .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dgabwknrsa .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#dgabwknrsa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dgabwknrsa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#dgabwknrsa .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dgabwknrsa .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dgabwknrsa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dgabwknrsa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dgabwknrsa .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dgabwknrsa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dgabwknrsa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dgabwknrsa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#dgabwknrsa .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dgabwknrsa .gt_left {
  text-align: left;
}
&#10;#dgabwknrsa .gt_center {
  text-align: center;
}
&#10;#dgabwknrsa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dgabwknrsa .gt_font_normal {
  font-weight: normal;
}
&#10;#dgabwknrsa .gt_font_bold {
  font-weight: bold;
}
&#10;#dgabwknrsa .gt_font_italic {
  font-style: italic;
}
&#10;#dgabwknrsa .gt_super {
  font-size: 65%;
}
&#10;#dgabwknrsa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dgabwknrsa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dgabwknrsa .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dgabwknrsa .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dgabwknrsa .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dgabwknrsa .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dgabwknrsa .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Metric&lt;/strong&gt;"><strong>Metric</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Day 1&lt;/strong&gt;, N = 200&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Day 1</strong>, N = 200<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Day 7&lt;/strong&gt;, N = 200&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Day 7</strong>, N = 200<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">AUC (ng/mL*h)</td>
<td headers="stat_1" class="gt_row gt_center">134 [102-168]</td>
<td headers="stat_2" class="gt_row gt_center">197 [131-297]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">C<sub>max</sub> (ng/mL)</td>
<td headers="stat_1" class="gt_row gt_center">10.1 [7.4-13.4]</td>
<td headers="stat_2" class="gt_row gt_center">14.7 [10.9-19.2]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">t<sub>max</sub> (h)</td>
<td headers="stat_1" class="gt_row gt_center">4.00 [1.00-6.00]</td>
<td headers="stat_2" class="gt_row gt_center">2.00 [1.00-6.00]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">C<sub>trough</sub> (ng/mL)</td>
<td headers="stat_1" class="gt_row gt_center">2.88 [1.42-4.49]</td>
<td headers="stat_2" class="gt_row gt_center">4.11 [1.90-8.59]</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median [5%-95%]</td>
    </tr>
  </tfoot>
</table>
</div>

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
table %>% export(dest="gt", subscripts=TRUE)
```

<div id="lywxfzgsjf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lywxfzgsjf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lywxfzgsjf thead, #lywxfzgsjf tbody, #lywxfzgsjf tfoot, #lywxfzgsjf tr, #lywxfzgsjf td, #lywxfzgsjf th {
  border-style: none;
}
&#10;#lywxfzgsjf p {
  margin: 0;
  padding: 0;
}
&#10;#lywxfzgsjf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lywxfzgsjf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#lywxfzgsjf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#lywxfzgsjf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#lywxfzgsjf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#lywxfzgsjf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lywxfzgsjf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lywxfzgsjf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#lywxfzgsjf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lywxfzgsjf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#lywxfzgsjf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#lywxfzgsjf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lywxfzgsjf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lywxfzgsjf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#lywxfzgsjf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lywxfzgsjf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#lywxfzgsjf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lywxfzgsjf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lywxfzgsjf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lywxfzgsjf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lywxfzgsjf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lywxfzgsjf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lywxfzgsjf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lywxfzgsjf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lywxfzgsjf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lywxfzgsjf .gt_left {
  text-align: left;
}
&#10;#lywxfzgsjf .gt_center {
  text-align: center;
}
&#10;#lywxfzgsjf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lywxfzgsjf .gt_font_normal {
  font-weight: normal;
}
&#10;#lywxfzgsjf .gt_font_bold {
  font-weight: bold;
}
&#10;#lywxfzgsjf .gt_font_italic {
  font-style: italic;
}
&#10;#lywxfzgsjf .gt_super {
  font-size: 65%;
}
&#10;#lywxfzgsjf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lywxfzgsjf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lywxfzgsjf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lywxfzgsjf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lywxfzgsjf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lywxfzgsjf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lywxfzgsjf .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Metric&lt;/strong&gt;"><strong>Metric</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;BW range: 50-75&lt;/strong&gt;, N = 103&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>BW range: 50-75</strong>, N = 103<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;BW range: 75-100&lt;/strong&gt;, N = 97&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>BW range: 75-100</strong>, N = 97<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Day 1">Day 1</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Day 1  label" class="gt_row gt_left">AUC (ng/mL*h)</td>
<td headers="Day 1  stat_1" class="gt_row gt_center">147 [122-172]</td>
<td headers="Day 1  stat_2" class="gt_row gt_center">123 [99-151]</td></tr>
    <tr><td headers="Day 1  label" class="gt_row gt_left">C<sub>max</sub> (ng/mL)</td>
<td headers="Day 1  stat_1" class="gt_row gt_center">10.35 [7.77-12.76]</td>
<td headers="Day 1  stat_2" class="gt_row gt_center">10.08 [7.09-13.50]</td></tr>
    <tr><td headers="Day 1  label" class="gt_row gt_left">t<sub>max</sub> (h)</td>
<td headers="Day 1  stat_1" class="gt_row gt_center">4.00 [2.00-6.00]</td>
<td headers="Day 1  stat_2" class="gt_row gt_center">2.00 [1.00-6.00]</td></tr>
    <tr><td headers="Day 1  label" class="gt_row gt_left">C<sub>trough</sub> (ng/mL)</td>
<td headers="Day 1  stat_1" class="gt_row gt_center">3.46 [1.94-4.82]</td>
<td headers="Day 1  stat_2" class="gt_row gt_center">2.39 [1.24-3.68]</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Day 7">Day 7</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Day 7  label" class="gt_row gt_left">AUC (ng/mL*h)</td>
<td headers="Day 7  stat_1" class="gt_row gt_center">227 [167-311]</td>
<td headers="Day 7  stat_2" class="gt_row gt_center">169 [124-231]</td></tr>
    <tr><td headers="Day 7  label" class="gt_row gt_left">C<sub>max</sub> (ng/mL)</td>
<td headers="Day 7  stat_1" class="gt_row gt_center">16.45 [12.89-19.81]</td>
<td headers="Day 7  stat_2" class="gt_row gt_center">13.05 [10.51-17.49]</td></tr>
    <tr><td headers="Day 7  label" class="gt_row gt_left">t<sub>max</sub> (h)</td>
<td headers="Day 7  stat_1" class="gt_row gt_center">2.00 [1.00-6.00]</td>
<td headers="Day 7  stat_2" class="gt_row gt_center">2.00 [1.00-6.00]</td></tr>
    <tr><td headers="Day 7  label" class="gt_row gt_left">C<sub>trough</sub> (ng/mL)</td>
<td headers="Day 7  stat_1" class="gt_row gt_center">5.50 [2.72-9.28]</td>
<td headers="Day 7  stat_2" class="gt_row gt_center">3.13 [1.51-6.19]</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median [5%-95%]</td>
    </tr>
  </tfoot>
</table>
</div>

### Example 3: Calculate 2-compartment half-life metrics

``` r
nca <- NCAMetrics(x=campsis %>% mutate(DOSE=1000, TAU=24), variable="Y") %>%
  add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z())) %>%
  calculate()

table <- NCAMetricsTable() %>%
  add(nca)
table %>% export(dest="gt", subscripts=TRUE)
```

<div id="fqigwsvrzp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fqigwsvrzp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#fqigwsvrzp thead, #fqigwsvrzp tbody, #fqigwsvrzp tfoot, #fqigwsvrzp tr, #fqigwsvrzp td, #fqigwsvrzp th {
  border-style: none;
}
&#10;#fqigwsvrzp p {
  margin: 0;
  padding: 0;
}
&#10;#fqigwsvrzp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#fqigwsvrzp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#fqigwsvrzp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#fqigwsvrzp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#fqigwsvrzp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#fqigwsvrzp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#fqigwsvrzp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#fqigwsvrzp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#fqigwsvrzp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#fqigwsvrzp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#fqigwsvrzp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#fqigwsvrzp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#fqigwsvrzp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#fqigwsvrzp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#fqigwsvrzp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fqigwsvrzp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#fqigwsvrzp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#fqigwsvrzp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#fqigwsvrzp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fqigwsvrzp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#fqigwsvrzp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fqigwsvrzp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#fqigwsvrzp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fqigwsvrzp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fqigwsvrzp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fqigwsvrzp .gt_left {
  text-align: left;
}
&#10;#fqigwsvrzp .gt_center {
  text-align: center;
}
&#10;#fqigwsvrzp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#fqigwsvrzp .gt_font_normal {
  font-weight: normal;
}
&#10;#fqigwsvrzp .gt_font_bold {
  font-weight: bold;
}
&#10;#fqigwsvrzp .gt_font_italic {
  font-style: italic;
}
&#10;#fqigwsvrzp .gt_super {
  font-size: 65%;
}
&#10;#fqigwsvrzp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#fqigwsvrzp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#fqigwsvrzp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#fqigwsvrzp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#fqigwsvrzp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#fqigwsvrzp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#fqigwsvrzp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Metric&lt;/strong&gt;"><strong>Metric</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 200&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 200</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">t<sub>½,dist</sub></td>
<td headers="stat_0" class="gt_row gt_center">2.57 [1.91-3.48]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">t<sub>½,eff</sub></td>
<td headers="stat_0" class="gt_row gt_center">13.4 [8.4-20.8]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">t<sub>½,z</sub></td>
<td headers="stat_0" class="gt_row gt_center">14.6 [9.6-22.2]</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median [5%-95%]</td>
    </tr>
  </tfoot>
</table>
</div>

### Example 4: Compute terminal half-live based on data

``` r
nca <- NCAMetrics(x=campsis, variable="Y") %>%
  add(c(Thalf(x=campsis %>% timerange(7*24, 10*24)))) %>%
  calculate()

table <- NCAMetricsTable() %>%
  add(nca)
table %>% export(dest="gt", subscripts=TRUE)
```

<div id="pulgxuvhas" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pulgxuvhas table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#pulgxuvhas thead, #pulgxuvhas tbody, #pulgxuvhas tfoot, #pulgxuvhas tr, #pulgxuvhas td, #pulgxuvhas th {
  border-style: none;
}
&#10;#pulgxuvhas p {
  margin: 0;
  padding: 0;
}
&#10;#pulgxuvhas .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#pulgxuvhas .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#pulgxuvhas .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#pulgxuvhas .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#pulgxuvhas .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#pulgxuvhas .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#pulgxuvhas .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#pulgxuvhas .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#pulgxuvhas .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#pulgxuvhas .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#pulgxuvhas .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#pulgxuvhas .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#pulgxuvhas .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#pulgxuvhas .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#pulgxuvhas .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pulgxuvhas .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#pulgxuvhas .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#pulgxuvhas .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#pulgxuvhas .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pulgxuvhas .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#pulgxuvhas .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pulgxuvhas .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#pulgxuvhas .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pulgxuvhas .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#pulgxuvhas .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pulgxuvhas .gt_left {
  text-align: left;
}
&#10;#pulgxuvhas .gt_center {
  text-align: center;
}
&#10;#pulgxuvhas .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#pulgxuvhas .gt_font_normal {
  font-weight: normal;
}
&#10;#pulgxuvhas .gt_font_bold {
  font-weight: bold;
}
&#10;#pulgxuvhas .gt_font_italic {
  font-style: italic;
}
&#10;#pulgxuvhas .gt_super {
  font-size: 65%;
}
&#10;#pulgxuvhas .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#pulgxuvhas .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#pulgxuvhas .gt_indent_1 {
  text-indent: 5px;
}
&#10;#pulgxuvhas .gt_indent_2 {
  text-indent: 10px;
}
&#10;#pulgxuvhas .gt_indent_3 {
  text-indent: 15px;
}
&#10;#pulgxuvhas .gt_indent_4 {
  text-indent: 20px;
}
&#10;#pulgxuvhas .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Metric&lt;/strong&gt;"><strong>Metric</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 200&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 200</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">t<sub>½</sub></td>
<td headers="stat_0" class="gt_row gt_center">14.7 [9.8-23.4]</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median [5%-95%]</td>
    </tr>
  </tfoot>
</table>
</div>
