
# `qtils`: Utility package 


<!-- badges: start -->
[![R-CMD-check](https://github.com/qpmnguyen/qtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qpmnguyen/qtils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Personal utility package. Functionality includes: 

* Generating tables from standardized models  
    * ANCOVA for a primary treatment effect.    
* Generating common plots. 
    * Observed longitudinal trajectories stratified by factors.    
* Precision-based sample size estimation for:  
    * Change-from-baseline analysis incorporating log-transformations or unknown standard deviation of paired differences. 

## Installation

You can install the development version of qtils from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("qpmnguyen/qtils")
```

## TODOs:  

* Write unit tests for the precision functions. 