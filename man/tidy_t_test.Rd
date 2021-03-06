% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t_test.R
\name{tidy_t_test}
\alias{tidy_t_test}
\alias{tidy_t_test_paired}
\title{Tidy Student's t-test}
\usage{
tidy_t_test(.data = NULL, values, groups = NULL,
  alternative = c("two.sided", "less", "greater"), mu = 0,
  var_equal = FALSE, conf_level = 0.95)

tidy_t_test_paired(.data = NULL, values_x, values_y,
  alternative = c("two.sided", "less", "greater"), mu = 0,
  var_equal = FALSE, conf_level = 0.95)
}
\arguments{
\item{.data}{data.frame or tibble from which to source variables.}

\item{values}{A vector of data values}

\item{groups}{A vector with exactly two unique values for a two-sample
t-test, or NULL for a one-sample t-test}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of "two.sided" (default), "greater" or "less". You can specify
just the initial letter.}

\item{mu}{a number indicating the true value of the mean (or difference in
means if you are performing a two sample test).}

\item{var_equal}{a logical variable indicating whether to treat the two
variances as being equal. If TRUE then the pooled variance is used to
estimate the variance otherwise the Welch (or Satterthwaite) approximation
to the degrees of freedom is used.}

\item{conf_level}{confidence level of the interval.}

\item{values_x, values_y}{Paired vectors of data values for the paired t-test.}
}
\description{
Tidy Student's t-test
}
\seealso{
\link[stats]{t.test}
}
