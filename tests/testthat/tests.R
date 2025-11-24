testthat::test_that("text_span()", {
  # unstyled span works
  expected_t1 <- list()
  expected_t1$type <- "text:span"
  expected_t1$children <- c("unstyled!")
  actual_t1 <- text_span("unstyled!")
  testthat::expect_equal(expected_t1, actual_t1)
  testthat::expect_null(expected_t1$attributes)


  # styled span works
  expected_t2 <- list()
  expected_t2$type <- "text:span"
  expected_t2$children <- c("unstyled!")
  expected_t2$attributes <- c(`text:style-name` = "stylish")
  actual_t2 <- text_span("unstyled!", style_name = "stylish")
  testthat::expect_equal(expected_t2, actual_t2)
})

testthat::test_that("text_p()", {
  # simple one
  expected_p1 <- list()
  expected_p1$type <- "text:p"
  expected_p1$children <- list(text_span("hello"))
  expected_p1$attributes <- c(`text:style-name` = "style")

  actual_p1 <- text_p(text_span("hello"), text_style_name = "style")
  testthat::expect_equal(actual_p1, expected_p1)

  # FIXME TODO this was syntactic sugar that I removed for now.
  # with line break
  # expected_p2 <- list()
  # expected_p2$type <- "text:p"
  # expected_p2$children <- list("there")
  # expected_p2$attributes <- c(`text:style-name` = "style")

  # testthat::expect_equal(
  #   text_p(text = "hello\nthere", text_style_name = "style"),
  #   list(expected_p1, expected_p2)
  # )
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

testthat::test_that("new_custom_shape()", {
  # can create and parse shapes without error
  testthat::expect_no_error({
    rect <- new_custom_shape(
      type = "rectangle",
      width = "1cm",
      height = "1cm",
      x = "1cm",
      y = "1cm",
      draw_style_name = "gr1",
      text_style_name = "P1",
      text = "some text",
      alt_text = "some alt text",
      rect_radius = 0
    )
    ellipse <- new_custom_shape(
      type = "ellipse",
      width = "1cm",
      height = "1cm",
      x = "1cm",
      y = "1cm",
      draw_style_name = "gr1",
      text_style_name = "P1",
      text = "some text",
      alt_text = "some alt text",
      rect_radius = 0
    )
    round_rect <- new_custom_shape(
      type = "round-rectangle",
      width = "1cm",
      height = "1cm",
      x = "1cm",
      y = "1cm",
      draw_style_name = "gr1",
      text_style_name = "P1",
      text = "some text",
      alt_text = "some alt text",
      rect_radius = 0
    )

    list_item_to_xml(rect)
    list_item_to_xml(ellipse)
    list_item_to_xml(round_rect)
  })
})

testthat::test_that("text_box()", {
  # minimal condition, we can make simple text box and convert to xml without error
  testthat::expect_no_error({
    test_text_box <- text_box(text = list(text_p(text_span("hello!"))), width = "1cm", height = "1cm", x = "1cm", y = "1cm")
    list_item_to_xml(test_text_box)
  })
  # minimal condition, we can put several paragraphs in  a text box and convert to xml without error
  testthat::expect_no_error({
    text <- text_p(text_span("howdy!"))
    list_text_box <- text_box(list(text, text), width = "1cm", height = "1cm", x = "1cm", y = "1cm")
    list_item_to_xml(list_text_box)
  })
})

testthat::test_that("new_list()", {
  # minimal condition: we can make a list and convert it to xml without error
  testthat::expect_no_error({
    # passing list item as ... argument
    test_list <- new_list(text_p(text_span("list item 1")))
    list_item_to_xml(test_list)

    # passing list item as contents_list argument
    test_list <- new_list(contents_list = list(text_p(text_span("list item 1"))))
    list_item_to_xml(test_list)
  })

  # and that it handles sub-lists
  testthat::expect_no_error({
    test_list <- new_list(
      text_p(text_span("some regular text")),
      new_list(
        text_p(text_span("sub-bullet 1")),
        text_p(text_span("sub-bullet 2"))
      )
    )
    list_item_to_xml(test_list)
  })
})
