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


testthat::test_that("create_manifest_img_xml()", {
  # png image works properly
  png_xml <- create_manifest_img_xml("test.png")
  png_xml_attrs <- xml2::xml_attrs(png_xml)

  testthat::expect_equal(png_xml_attrs["full-path"], "test.png", ignore_attr = TRUE)
  testthat::expect_equal(png_xml_attrs["media-type"], "image/png", ignore_attr = TRUE)

  # svg image works properly
  svg_xml <- create_manifest_img_xml("test.svg")
  svg_xml_attrs <- xml2::xml_attrs(svg_xml)

  testthat::expect_equal(svg_xml_attrs["full-path"], "test.svg", ignore_attr = TRUE)
  testthat::expect_equal(svg_xml_attrs["media-type"], "image/svg+xml", ignore_attr = TRUE)


  # file extension other than svg, png throws  error
  testthat::expect_error(create_manifest_img_xml("test.txt"))
})
