# scratchwork for unordered (maybe ordered?) lists
devtools::load_all()

# text_style_name <- "L1"
# text_span

# text_span("hello")
# text_box("sadf", width = "1cm", height = "1cm", x = "1cm", y = "1cm")


deck <- odp::new_pres()

style_center <- new_paragraph_style("center", text_align = "center")
style_t_bold <- new_text_style_minimal(name = "bold", font_weight = "bold")
style_t_italic <- new_text_style_minimal(name = "italic", font_style = "italic")
style_18pt <- new_text_style_minimal(name = "18pt", font_size = "18pt")
style_8pt <- new_paragraph_style(name = "8pt", font_size = "8pt")
styles <- list(style_center, style_t_bold, style_t_italic, style_18pt, style_8pt)
styles[[3]]
# test_text_list <- text_box(
#   text = list(
#     "Item number 1",
#     "Item #2. These should be small and bold!"
#   ),
#   width = "10cm",
#   height = "3cm",
#   x = "1cm",
#   y = "1cm",
#   draw_text_style_name = "style_pg_bold"
# )
# test_text_normal <- text_box(
#   text = "normal text",
#   width = "10cm",
#   height = "10cm",
#   x = "1cm",
#   y = "5cm",
#   draw_text_style_name = "style_pg_bold"
# )



# test_text_list |>
# list_item_to_xml()
list_of_spans <- list(
  text_span("sorry ", style_name = "bold"),
  text_span("dave", style_name = "italic"),
  text_span("!!!!!", style_name = "18pt")
)
devtools::load_all()
test_list <- new_list(text_p(contents_list = list(text_span("hello", style_name = "bold"))))


text_p(
  text_span("hello", style_name = "bold"),
  text_span("hello", style_name = "italic")
)
p_of_spans <- text_p(contents_list = list_of_spans, text_style_name = "center")

text_box_of_spans <- text_box(list(p_of_spans, p_of_spans, p_of_spans), width = "10cm", height = "1cm", x = "1cm", y = "1cm")

text_box_of_list <- text_box(list(test_list), width = "10cm", height = "1cm", x = "1cm", y = "5")
text_box_of_list |>
  list_item_to_xml() |>
  as.character()
really_good_one <- list(
  text_p(
    text_style_name = "8pt",
    text_span("hello"),
    text_span(" my friends!", style_name = "bold"),
    text_span(" Check this out:", style_classes = " bold italic ")
  ),
  new_list(
      text_p(text_span("One"), text_style_name = "8pt"),
      text_p(text_span("Two"), text_style_name = "8pt"),
      text_p(text_span("Three", style_name = "bold"), text_style_name = "8pt"),
      text_p(text_span("Fifteen", style_name = "italic"), text_style_name = "8pt"),
    text_style_name = "8pt", list_style_name = "8pt"
  )
) |>
  text_box(width = "10cm", height = "10cm", x = "5cm", y = "8cm")

list_item_to_xml(really_good_one) |> as.character()
slide1 <- new_slide() |>
  add_to_slide(text_box_of_spans) |>
  add_to_slide(text_box_of_list) |>
  add_to_slide(really_good_one)
# add_to_slide(test_text_list)


slides <- list(slide1)

deck |>
  write_styles(styles) |>
  write_slides(slides) |>
  save_pres(filename = "testlist.odp")
