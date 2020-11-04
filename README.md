
# packDAMipd
<!-- badges: start -->
   [![Build Status](https://travis-ci.org/sheejamk/valueEQ5D.svg?branch=master)](https://travis-ci.org/sheejamk/valueEQ5D)
<!-- badges: end -->
<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/sheejamk/packDAMipd/branch/master/graph/badge.svg)](https://codecov.io/gh/sheejamk/packDAMipd?branch=master)
<!-- badges: end -->

"packDAMipd" is a package that allows to do decision analysis using Markov model. It allows estimating incremental cost effectiveness ratio and net monetary benefit for the strategies that you are trying to compare. For the Markov modelling, the parameters like transition probabilities, costs and qualys can be given either directly, or in  a data file or the package will estimate the parameter using regression models. In such instances, the individual patient level data should be provided. This also allows estimating the qalys using a package in CRAN "valueEQ5D". Moreover to cost effectiveness analysis, the package can also be used to estimate the sensitivity analysis, both deterministic and probabilistic sensitivity analysis.

## Usage
See the User Guide Vignette for examples on using the package. 

For a simple toy model please see [`toy_model.R`](https://github.com/sheejamk/packDAMipd/blob/master/R/toy_model.R)

## Installation
You can install the released version from CRAN with the command:

install.packages("packDAMipd")

Alternatively, the latest release can be installed from www.github.com/sheejamk/packDAMipd

