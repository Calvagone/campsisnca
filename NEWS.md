# campsisnca 1.5.2

* Tests should not be based on results including RUV #71
* Review tests #72

# campsisnca 1.5.1

* Set up code coverage report with codecov.io #67
* Add logo and badges to README #68
* Add NEWS file to repository #69

# campsisnca 1.5.0

* Time above/below a certain limit #62
* Line breaks for summary statistics in gt/gtsummary tables #63
* README should be generated with the same data as the tests #64
* The symbol argument of style_percent() is deprecated as of gtsummary 2.0.3 #65

# campsisnca 1.4.0

* Last version of gtsummary (v2.0.0) introduces breaking changes in campsisnca #55
* Remove unused 'level' argument in calculate method #56
* Add argument 'quantile_type' to the calculate function #57
* Set quantile type default value to 2 #58
* Dynamic computation of quantiles #59

# campsisnca 1.3.0

* Ambiguous levels in summary with categorical variables #45
* Show first strata variable in columns or rows #50
* Add new dataframe export types for convenience: "summary_wide" and "summary_pretty" #51
* Don't use rxode2 anymore to generate the residual variability on the test data #52
* Code review #53

# campsisnca 1.2.0

* Export function removes ID column when type=individual_wide #44
* Show all dichotomous levels on demand when table is exported to gt/gtsummary #46
* Allow string as return value of the custom function #47
* Auto-cast column when 'individual_wide' format is requested and data is logical #48

# campsisnca 1.1.0

* Add custom statistics (geomean, geocv, cv, se) #36
* Add metrics Avg, Max, Min, Last, Clast, CAt, ValueAt #37
* Auto-replace known NCA metrics in custom function #38
* Use AUC() instead of Auc() #39
* Change default stat display #40

# campsisnca 1.0.0

* Major refactoring of the package with gt/gtsummary

# campsisnca 0.4.0

* Add GPL-v3 license to package #13
* Repair unit tests due to changes in model suite #14
* Configure continuous integration (CI) #7

# campsisnca 0.3.0

* Update package version #11
* Get rid of RxODE #10

# campsisnca 0.2.2

* Export individual parameters #8
