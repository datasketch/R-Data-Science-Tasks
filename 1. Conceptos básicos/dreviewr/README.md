
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dreviewr

<!-- badges: start -->
<!-- badges: end -->

dreviewr is a package that aims to help data scientists to review their
data. It provides a set of functions that can be used to check the
quality of the data. With dreviewr you can easily get a detailed summary
of any data frame. This summary includes the number of observations,
number of variables, variable names, variable types, and some useful
statistics for each variable. It also includes information about
possible inconsistencies in the data, such as missing values, duplicated
rows, and outliers. These inconsistencies are marked with a warning
sign, so they can be easily identified.

Main function is `review`, which performs all the checks and returns a
summary of the data as a data frame, and also prints it to the console,
coupled with a summary of the inconsistencies found as a readable list.
You can also access to individual summaries for each consult, as
dreveiwr provides all of its review functions as individual functions.

## Example

This is a basic example of how to use dreviewr to review a data frame:

``` r
library(dreviewr)

review(iris)
#> *──────────────────────────────*
#>   Data review
#> *──────────────────────────────*
#>   General information
#> 
#>  data           value
#>  Rows           150  
#>  Columns          5  
#>  Duplicate rows   2  
#> *──────────────────────────────*
#>   Column information
#> 
#>   column       class  
#> 1 Petal.Length numeric
#> 2 Petal.Width  numeric
#> 3 Sepal.Length numeric
#> 4 Sepal.Width  numeric
#> 5 Species      factor 
#> *──────────────────────────────*
#>   Factor columns summary
#> 
#>   column  missing ordered unique
#> 5 Species 0       FALSE   3     
#> *──────────────────────────────*
#>   Numeric columns summary
#> 
#>   column       missing mean sd   min q1  median q3  max
#> 1 Petal.Length 0       3.76 1.77 1.0 1.6 4.35   5.1 6.9
#> 2 Petal.Width  0       1.20 0.76 0.1 0.3 1.30   1.8 2.5
#> 3 Sepal.Length 0       5.84 0.83 4.3 5.1 5.80   6.4 7.9
#> 4 Sepal.Width  0       3.06 0.44 2.0 2.8 3.00   3.3 4.4
#> *──────────────────────────────*
#> *──────────────────────────────*
#>   Warnings
#> *──────────────────────────────*
#>   Duplicates:
#> 
#>    - The data contains 2 duplicated rows.
#> 
#> *──────────────────────────────*
#>   Outliers:
#> 
#>    - The data contains 4 outlier(s) in the Sepal.Width column.
#> *──────────────────────────────*
```

It is also possible to review a data frame by specifying the path to an
external file. It also supports URLs!

``` r
review("https://raw.githubusercontent.com/ismayc/pnwflights14/master/data/flights.csv")
#> *──────────────────────────────*
#>   Data review
#> *──────────────────────────────*
#>   General information
#> 
#>  data           value 
#>  Rows           162049
#>  Columns            16
#>  Duplicate rows      2
#> *──────────────────────────────*
#>   Column information
#> 
#>    column    class    
#> 1  air_time  integer  
#> 2  arr_delay integer  
#> 3  arr_time  integer  
#> 4  carrier   character
#> 5  day       integer  
#> 6  dep_delay integer  
#> 7  dep_time  integer  
#> 8  dest      character
#> 9  distance  integer  
#> 10 flight    integer  
#> 11 hour      integer  
#> 12 minute    integer  
#> 13 month     integer  
#> 14 origin    character
#> 15 tailnum   character
#> 16 year      integer  
#> *──────────────────────────────*
#>   Numeric columns summary
#> 
#>    column    missing mean    sd      min  q1   median q3   max 
#> 1  air_time  1301     152.59   72.55   18  103  129    199  422
#> 2  arr_delay 1301       2.24   31.19  -67  -12   -4      7 1539
#> 3  arr_time   988    1482.50  523.96    1 1127 1517   1918 2400
#> 5  day          0      15.75    8.79    1    8   16     23   31
#> 6  dep_delay  857       6.13   29.11  -37   -5   -2      5 1553
#> 7  dep_time   857    1278.28  522.58    1  831 1217   1721 2400
#> 9  distance     0    1204.51  653.15   93  689  991   1660 2724
#> 10 flight       0    1357.36 1495.27    2  408  694   1726 6527
#> 11 hour       857      12.48    5.23    0    8   12     17   24
#> 12 minute     857      30.33   18.06    0   14   30     47   59
#> 13 month        0       6.61    3.32    1    4    7      9   12
#> 16 year         0    2014.00    0.00 2014 2014 2014   2014 2014
#> *──────────────────────────────*
#>   Character columns summary
#> 
#>    column  missing distinct
#> 4  carrier 0         11    
#> 8  dest    0         71    
#> 14 origin  0          2    
#> 15 tailnum 0       3023    
#> *──────────────────────────────*
#> *──────────────────────────────*
#>   Warnings
#> *──────────────────────────────*
#>   Missing values:
#> 
#>    - The data contains 857 missing value(s) in the dep_time column.
#>    - The data contains 857 missing value(s) in the dep_delay column.
#>    - The data contains 988 missing value(s) in the arr_time column.
#>    - The data contains 1301 missing value(s) in the arr_delay column.
#>    - The data contains 1301 missing value(s) in the air_time column.
#>    - The data contains 857 missing value(s) in the hour column.
#>    - The data contains 857 missing value(s) in the minute column.
#> *──────────────────────────────*
#>   Outliers:
#> 
#>    - The data contains 19009 outlier(s) and 11559 of them are far outliers in the dep_delay column.
#>    - The data contains 11903 outlier(s) and 5366 of them are far outliers in the arr_delay column.
#>    - The data contains 16686 outlier(s) and 1333 of them are far outliers in the flight column.
#>    - The data contains 2145 outlier(s) in the air_time column.
#> *──────────────────────────────*
```
