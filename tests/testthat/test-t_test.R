context("test-t_test.R")

test_that("t_test gives the same answer as t.test", {
  ex_sleep2 <- t.test(extra ~ group, data = sleep)
  tidy_ex_sleep2 <- tidy_t_test(sleep, values = extra, groups = group)
  expect_identical(unclass(ex_sleep2), unclass(tidy_ex_sleep2))

  ex_sleep <- t.test(sleep$extra)
  tidy_ex_sleep <- tidy_t_test(values = sleep$extra)
  expect_identical(
    unclass(ex_sleep)[setdiff(names(ex_sleep), "estimate")],
    unclass(tidy_ex_sleep)[setdiff(names(ex_sleep), "estimate")]
  )
})
