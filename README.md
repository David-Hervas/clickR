[![Build Status](https://travis-ci.org/David-Hervas/clickR.svg?branch=master)](https://travis-ci.org/David-Hervas/clickR)
[![Coverage Status](https://codecov.io/github/David-Hervas/clickR/coverage.svg?branch=master)](https://codecov.io/github/David-Hervas/clickR?branch=master) 
[![CRAN Version](http://www.r-pkg.org/badges/version/clickR)](https://CRAN.R-project.org/package=clickR)
[![](http://cranlogs.r-pkg.org/badges/clickR)](https://CRAN.R-project.org/package=clickR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/clickR)](https://CRAN.R-project.org/package=clickR)

# Overview
Tools for assessing data quality, performing exploratory analysis, and semi-automatic preprocessing of messy data with change tracking for integral dataset cleaning.

# Installation
``` r
# The stable version of clickR can be installed from CRAN with:
install.packages("clickR")

# Alternatively, the development version can be installed from Github:
remotes::install_github("David-hervas/clickR")
```

# Usage
clickR functions are divided in three groups:

1.  Functions for assessing data quality and performing exploratory analyses:
`peek()`
`descriptive()`
`cluster_var()`
`mine.plot()`
`outliers()`
`bivariate_outliers()`  
  
2.  Functions for detecting and correcting errors:
`nice_names()`
`fix_factors()`
`fix_dates()`
`fix_numerics()`
`fix_levels()`
`fix_NA()`
`fix_concat()`
`remove_empty()`

3.  Functions for reviewing and, potentially, restoring the changes applied to the data.
`track_changes()`
`restore_changes()`
`manual_fix()`
 
Each function has its corresponding help page, which can be accessed by the standard procedure in R: typing `?name_of_function` in the console or, in R Studio, by clicking on the function name and pressing F1.

A simplified usage procedure would be:

``` r
# Explore the data with some of the exploratory analysis functions:
descriptive(mtcars_messy)

# Data frame with 32 observations and 13 variables.

# Numeric variables (7)
#           Min  1st Q. Median 3rd Q.   Max    Mean      SD Kurtosis Skewness Modes NAs                   Distribution
# cyl       4.0   4.000   6.00    8.0   8.0   6.188   1.786   -1.762   -0.175    2    0 [#############:##############]
# disp     71.1 120.825 196.30  326.0 472.0 230.722 123.939   -1.207    0.382    2    0 |---[####:########]----------|
# hp       52.0  96.500 123.00  180.0 335.0 146.688  68.563   -0.136    0.726    1    0 |----[#:#####]---------------|
# qsec     14.5  16.892  17.71   18.9  22.9  17.849   1.787    0.335    0.369    1    0 |-------[##:###]-------------|
# am        0.0   0.000   0.00    1.0   1.0   0.406   0.499   -1.925    0.364    2    0 :############################]
# nº Gears  3.0   3.000   4.00    4.0   5.0   3.688   0.738   -1.070    0.529    3    0 [#############:--------------|
# carb      1.0   2.000   2.00    4.0   8.0   2.812   1.615    1.257    1.051    2    0 |---:#######]----------------|
#
# Categorical variables (6)
#       N. Classes              Classes       Mode Prop. mode  Anti-mode Prop. Anti-mode NAs
# Mpg           28 10.4/15.2/21/30.4/19       10.4      0.062       19.2           0.031   0
# drat          22 3.07/3.92/2.76/3.08/       3.07      0.094          -           0.031   0
# wt            29 3.44/3.57/1.513/1.61       3.44      0.094      1.513           0.031   0
# vs             4           0/1/?/NULL          0        0.5          ?           0.031   0
# date          32 06/25/1974/12/22/73/ 06/25/1974      0.031 06/25/1974           0.031   0
# maker         26 Mrc/Ft/AMC/Cdllc/Cmr       Merc      0.188        AMC           0.031   0

# Use some of the fix functions to correct errors:
mtcars_messy <- fix_NA(mtcars_messy)
mtcars_messy <- fix_dates(mtcars_mesy)
mtcars_messy <- fix_numerics(mtcars_messy)

# Review the performed changes
track_changes(mtcars_messy)

# variable         observation  original     new          fun
#      drat          Duster 360              <NA>       fix_NA
#      drat         Honda Civic         -    <NA>       fix_NA
#        vs  Cadillac Fleetwood         ?    <NA>       fix_NA
#        vs       Porsche 914-2      NULL    <NA>       fix_NA
#       Mpg                 all character numeric fix_numerics
#      drat                 all character numeric fix_numerics
#        wt                 all character numeric fix_numerics
#       Mpg          Datsun 710      22,8    22.8 fix_numerics
#       Mpg      Hornet 4 Drive     21.,4    21.4 fix_numerics
#       Mpg          Duster 360  14.3 mpg    14.3 fix_numerics
#       Mpg            Merc 280      19.2    19.2 fix_numerics
#       Mpg           Merc 280C   1.78e01    17.8 fix_numerics
#        wt Lincoln Continental     5,424   5.424 fix_numerics
#        wt   Chrysler Imperial     5,345   5.345 fix_numerics

# Id needed, restore the unwanted changes:
mtcars_messy <- restore_changes(track_changes(mtcars_messy, fun == "fix_numerics" & variable == "wt"))
```

# Parallelization of data-cleaning tasks
New versions of clickR provide parallelization for some of the data-cleaning functions via the future package. 

``` r
library(future)
plan(multisession(workers=2))  #Set number of workers
fix_dates(mtcars_messy)
```
This functionality still needs some optimization. So be aware that, in some (rare) specific cases, parallelized tasks might take more time than non-parallelized tasks. 
