
#' Tidy Student's t-test
#'
#' @param .data data.frame or tibble from which to source variables.
#' @param values A vector of data values
#' @param values_x,values_y Paired vectors of data values for the paired t-test.
#' @param groups A vector with exactly two unique values for a two-sample
#'   t-test, or NULL for a one-sample t-test
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of "two.sided" (default), "greater" or "less". You can specify
#'   just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in
#'   means if you are performing a two sample test).
#' @param var_equal a logical variable indicating whether to treat the two
#'   variances as being equal. If TRUE then the pooled variance is used to
#'   estimate the variance otherwise the Welch (or Satterthwaite) approximation
#'   to the degrees of freedom is used.
#' @param conf_level confidence level of the interval.
#'
#' @seealso \link[stats]{t.test}
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @importFrom rlang .data
#'
tidy_t_test <- function(
  .data = NULL,
  values, groups = NULL,
  alternative = c("two.sided", "less", "greater"),
  mu = 0,
  var_equal = FALSE,
  conf_level = 0.95
) {
  alternative <- match.arg(alternative)
  assert_that(
    is.numeric(mu),
    is.logical(var_equal),
    is.numeric(conf_level),
    conf_level < 1,
    conf_level > 0
  )

  data <- data_eval(.data, values = !!enquo(values), groups = !!enquo(groups), .allow_null = TRUE)
  data_names <- attr(data, "tidy_data_names")
  assert_that(is.numeric(data$values))

  if("groups" %in% colnames(data)) {
    assert_that(
      length(unique(data$groups)) == 2,
      msg = "groups be a vector with exactly two unique values"
    )

    grouped <- dplyr::group_by(data, .data$groups)
    indices <- attr(grouped, "indices")
    test <- stats::t.test(
      x = data$values[indices[[1]] + 1],
      y = data$values[indices[[2]] + 1],
      alternative = alternative,
      paired = FALSE,
      conf.level = conf_level,
      mu = mu,
      var.equal = var_equal,
      na.action = stats::na.fail
    )
    test$data.name <- sprintf("%s by %s", data_names[1], data_names[2])
    names(test$estimate) <- paste("mean in group", attr(grouped, "labels")$group)

    class(test) <- c("tidy_two_sample_t_test", "tidy_t_test", class(test))
  } else {
    # one-sample t-test
    test <- stats::t.test(
      x = data$values,
      y = NULL,
      paired = FALSE,
      var.equal = FALSE,
      conf.level = conf_level,
      alternative = alternative,
      mu = mu,
      na.action = stats::na.fail
    )
    test$data.name <- unname(data_names)
    names(test$estimate) <- paste("mean of", data_names)

    class(test) <- c("tidy_one_sample_t_test", "tidy_t_test", class(test))
  }

  test
}

#' @rdname tidy_t_test
#' @export
tidy_t_test_paired <- function(
  .data = NULL,
  values_x, values_y,
  alternative = c("two.sided", "less", "greater"),
  mu = 0,
  var_equal = FALSE,
  conf_level = 0.95
) {
  alternative <- match.arg(alternative)
  assert_that(
    is.numeric(mu),
    is.logical(var_equal),
    is.numeric(conf_level),
    conf_level < 1,
    conf_level > 0
  )

  data <- data_eval(.data, values_x = !!enquo(values_x), values_y = !!enquo(values_y),
                    .allow_null = FALSE)
  data_names <- attr(data, "tidy_data_names")
  assert_that(is.numeric(data$values_x), is.numeric(data$values_y))

  test <- stats::t.test(
    x = data$values_x,
    y = data$values_y,
    paired = TRUE,
    var.equal = FALSE,
    conf.level = conf_level,
    alternative = alternative,
    mu = mu,
    na.action = stats::na.fail
  )
  test$data.name <- sprintf("%s and %s", data_names[1], data_names[2])
  names(test$estimate) <- "mean of the differences"

  class(test) <- c("tidy_paired_t_test", "tidy_t_test", class(test))

  test
}

