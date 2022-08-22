test_that("format_p() works", {
  p_vec <- c(0.00001, 0.01111, 0.0500000, 0.14, 0.34, 0.99999)
  format_p(p_vec)
  format_p(p_vec, 2)
  format_p(p_vec, 3)

  expect_equal(
    format_p(p_vec),
    c("<0.001", "0.011", "0.050", "0.14", "0.3", ">0.9")
  )
  expect_equal(
    format_p(p_vec, 2),
    c("<0.001", "0.011", "0.050", "0.14", "0.34", ">0.99")
  )
  expect_equal(
    format_p(p_vec, 3),
    c("<0.001", "0.011", "0.050", "0.140", "0.340", ">0.999")
  )

  expect_error(format_p(p_vec, digits = 17))
})
