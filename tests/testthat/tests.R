testthat::test_that("new_slide()", {
  actual <- new_slide(name = "TEST!")
  expected <- list()
  expected$type <- "draw:page"
  expected$attributes <- c(`draw:name` = "TEST!")
  expected$children <- list()
  testthat::expect_equal(actual, expected)
})

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
      text = list(
        text_p(text_span("some text in a span in a p")),
        text_p(text_span("some more text in a span in a p"))
      ),
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
      text = text_p(text_span("some text in a span in a p")),
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
  # test failure conditions
  # contents_list must be a list
  testthat::expect_error(new_list(contents_list = c("abc", "123")))

  # not a list off text_p objects
  testthat::expect_error({
    new_list(contents_list = list("abra", "cadabra"))
  })

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


testthat::test_that("field_page_num()", {
  testthat::expect_no_error({
    list_item_to_xml(field_page_num())
    list_item_to_xml(text_p(text_span(field_page_num())))
  })
})

testthat::test_that("list_item_to_xml()", {
  # fails if input is not a list
  testthat::expect_error(list_item_to_xml(1:10))

  # fails if list does not have $type property
  testthat::expect_error(list_item_to_xml(list(typo = "note-type")))

  # succeeds if list does have $type property
  testthat::expect_no_error(list_item_to_xml(list(type = "note-type")))
})

testthat::test_that("maybe_add_to_vector()", {
  v0 <- c()
  v1 <- v0 |>
    maybe_add_to_vector("name", "value")

  testthat::expect_equal(v1, c("name" = "value"))

  v2 <- v1 |>
    maybe_add_to_vector("name2", "value2")
  testthat::expect_equal(v2, c("name" = "value", "name2" = "value2"))

  v3 <- v2 |>
    maybe_add_to_vector("name3", NA)
  testthat::expect_equal(v3, c("name" = "value", "name2" = "value2"))
})

testthat::test_that("new_graphic_style()", {
  # minimal condition: can create a style and turn it into XML without error
  testthat::expect_no_error({
    style <- new_graphic_style(name = "test")
    list_item_to_xml(style)
  })
})

testthat::test_that("new_paragraph_style()", {
  # minimal condition: can create a style and turn it into XML without error
  testthat::expect_no_error({
    style <- new_paragraph_style(name = "test")
    list_item_to_xml(style)
  })
})

testthat::test_that("new_text_style()", {
  # minimal condition: can create a style and turn it into XML without error
  testthat::expect_no_error({
    style <- new_text_style(name = "test")
    list_item_to_xml(style)
  })
})

testthat::test_that("new_text_style_minimal()", {
  # minimal condition: can create a style and turn it into XML without error
  testthat::expect_no_error({
    style_something <- new_text_style_minimal(name = "test", font_weight = "bold")
    style_empty <- new_text_style_minimal(name = "test")
    list_item_to_xml(style_something)
    list_item_to_xml(style_empty)
  })

  # no style means no attributes
  style_empty <- new_text_style_minimal(name = "test")
  testthat::expect_null(style_empty$children[[1]]$attributes)

  # bold style means bold attribute and only bold attribute
  style_bold <- new_text_style_minimal(name = "test", font_weight = "bold")
  testthat::expect_equal(
    style_bold$children[[1]]$attributes["fo:font-weight"],
    c("fo:font-weight" = "bold")
  )
  testthat::expect_equal(
    length(style_bold$children[[1]]$attributes["fo:font-weight"]),
    1
  )
})
