
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidystats
=========

[![Coverage Status](https://img.shields.io/codecov/c/github/paleolimbot/tidystats/master.svg)](https://codecov.io/github/paleolimbot/tidystats?branch=master) [![Travis build status](https://travis-ci.org/paleolimbot/tidystats.svg?branch=master)](https://travis-ci.org/paleolimbot/tidystats)

The goal of tidystats is to provide a "tidy-in, tidy-out" interface to statistical tests, including implementations for many tests in the **stats** package. Generally, a test object will get created using columns from a (tidy) `data.frame`. For example, a t-test using the `sleep` dataset would look like this:

``` r
head(sleep)
#>   extra group ID
#> 1   0.7     1  1
#> 2  -1.6     1  2
#> 3  -0.2     1  3
#> 4  -1.2     1  4
#> 5  -0.1     1  5
#> 6   3.4     1  6
test <- tidy_t_test(sleep, values = extra, groups = group)
```

Installation
------------

You can install tidystats from github with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/tidystats")
```

Philosophy
----------

The (currently loose) philosphy of this package is to make statistics easier to teach by using consistent terminology and syntax across functions. So far, the loose set of rules is as follows:

-   Functions should be prefixed with `tidy_`, unless somebody can think of something better.

-   All functions and argument names should use `tidy_style`.

-   Statistical tests should be created with `.data` (a data.frame) as an optional first argument. If `.data` is provided, arguments that create the data for the test should be evaluated using `dplyr::transmute()`. If not, they should be evaluated using `tibble::tibble()`. This lets quick one-off calls that let the user skip creating the initial `data.frame`. This is currently implemented in `data_eval()`. Some statistical tests are probably better suited to the `dplyr::select()` syntax (e.g., PCA), where the `everything()` or `starts_with()` selectors might be useful. This could be implemented using something like the `dplyr::vars()` wrapper, but isn't yet. Tidy evaluation is a must!

-   The output of a test function (like `tidy_t_test()`) should be an S3 classed list. The current approach is to return the same object that the **stats** equivalent would return with classes tacked on to the front, meaning it can be used like the **stats** class.

-   A `print()` method (preferably using pretty **cli** and **crayon** output), a `broom::tidy()` method (multi-row summary of the result), a `broom::glance()` method (one-row summary of fit quality), a `ggplot2::autoplot()` method should be implemented for the test class.

-   If the concept of applying the model to a new dataset is relevant (predicting), `predict.s3_class_name()` should be implemented in the following way:

    -   The first argument is the model (it has to be this way for S3 dispatch to work...another function called something like `tidy_predict()` could put the `.data` first to work slightly better with the pipe).

    -   The (optional) `.data` argument and `...` are evaluated using `data_eval()` to create what is usually called `newdata` in the **stats** implementation.

    -   The output should be a `tibble` with the same number of rows as the input `.data`. If `.data` is given (there was an input data frame from which variables were taken), the input variables should not be included in the output. If `.data` was not given (i.e., the use created the data from within `...`), the input variables should be included in the output. This facilitates quick one-off predicting for interactive use, and `df %>% bind_cols(., predict(test, .))` for adding predicted data to an existing data frame.

-   Probably more rules to come as more writing is done...

Contributing
------------

Please contribute to this package! I imagine there are a lot of ideas of how to go about this, and now is a great time to try all of them.
