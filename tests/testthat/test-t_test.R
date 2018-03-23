context("test-t_test.R")

test_that("t_test gives the same answer as t.test", {
  ex_sleep2 <- t.test(extra ~ group, data = sleep)
  tidy_ex_sleep2 <- tidy_t_test(sleep, values = extra, groups = group)
  expect_identical(unclass(ex_sleep2), unclass(tidy_ex_sleep2))

  ex_sleep <- t.test(sleep$extra)
  tidy_ex_sleep <- tidy_t_test(values = sleep$extra)

  # $estimate name is always "x" for a one-sample t-test in stats::t.test
  # and we are aiming higher than stats::t.test
  expect_identical(
    unclass(ex_sleep)[setdiff(names(ex_sleep), "estimate")],
    unclass(tidy_ex_sleep)[setdiff(names(ex_sleep), "estimate")]
  )

  # probably a better example somewhere
  diamonds <- ggplot2::diamonds
  ex_paired <- t.test(diamonds$x, diamonds$y, paired = TRUE)
  tidy_ex_paired <- tidy_t_test_paired(values_x = diamonds$x, values_y = diamonds$y)
  expect_identical(unclass(ex_paired), unclass(tidy_ex_paired))
})
