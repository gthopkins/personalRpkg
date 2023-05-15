test_that("finite-output", {
  expect_type(object = log_summed_exps(1:2000),
              type = "double")
})

test_that("take-one-number", {
  expect_type(object = log_summed_exps(6),
              type = "double")
})

test_that("calculate-1:2000", {
  expect_equal(object = log_summed_exps(1:2000),
               expected = 2000.459,
               tolerance = 1e-4)
})

test_that("direct-calc-1:30", {
  expect_equal(object = log_summed_exps(1:30),
               expected = log(sum(exp(1:30))),
               tolerance = 1e-4)
})

test_that("calculate-1:17", {
  expect_equal(object = log_summed_exps(1:17),
               expected = log(sum(exp(1:17))),
               tolerance = 1e-4)
})
