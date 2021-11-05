The woorc Package
========================

This package for R entails the code to execute a simulation study and
the code used to analyse the output of the simulation study,
accompanying the manuscript “Regression calibration: when to use it and when not to use it”. The aim of the simulation study is to illustrate the window of opportunity of regression calibration over a range of different scenarios (specifically, adjusting the reliablity of the error-prone variable).


Installation
============

The package can be installed via

``` r
devtools::install_github("LindaNab/woorc", ref = "main")
```

Quick demo
==========

``` r
library(woorc)
# view input data
data(input)
# run scenenario no. 1 of the simulation study with 100 replicates
run_sim(nrep = 100, scen_no = 1)
```
