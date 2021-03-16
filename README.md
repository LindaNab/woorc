
# The simexvsmecor Package

This package for R entails the simulation code to execute the simulation
study and the analysis code used to produce the results from the
simulation study, accompanying the manuscript “Random measurement error
correction in an exposure by means of simulation-extrapolation and
regression calibration”. The simulation study is set up to compare the
simex and mecor package for measurement error correction.

# Installation

The package can be installed via

``` r
devtools::install_github("LindaNab/simexvsmecor", ref = "main")
```

# Quick demo

``` r
library(simexvsmecor)
# view input data
data(input)
# run scenenario no. 1 of the simulation study with 100 replicates
run_sim(nrep = 100, scen_no = 1)
```
