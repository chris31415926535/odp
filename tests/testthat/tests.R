testthat::test_that("text_p_list()", {
  # simple one
  expected_p1 <- list()
  expected_p1$type <- "text:p"
  expected_p1$attributes <- c(`text:style-name` = "style")
  expected_p1$children <- list("hello")

  testthat::expect_equal(
    odp::text_p_list(text = "hello", text_style_name = "style"),
    list(expected_p1)
  )

  # with line break
  expected_p2 <- list()
  expected_p2$type <- "text:p"
  expected_p2$attributes <- c(`text:style-name` = "style")
  expected_p2$children <- list("there")

  testthat::expect_equal(
    odp::text_p_list(text = "hello\nthere", text_style_name = "style"),
    list(expected_p1, expected_p2)
  )
})
