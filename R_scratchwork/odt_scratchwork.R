devtools::load_all()

# Create a new presentation. This has invisible side effects!
# The function returns an XML object and ALSO
# initializes a new document in a temporary folder.
# Note that for now you can only work on one presentation at a time!
deck <- new_pres()

# Set up an empty list for our slides
slides <- list()

# Initialize a new font
fonts <- list(new_font_list(name = "FreeSerif"))

# Define some styles. Give them memorable names, we will refer back to them.
# This is kind of like defining CSS classes.
style_pg_chris <- new_paragraph_style_list(
  name = "chris", color = "#6502ff", font_weight = "bold",
  font_name = "FreeSerif", text_align = "end"
)
style_pg_center <- new_paragraph_style_list(
  name = "style_pg_center", text_align = "center"
)

style_pg_pagenum <- new_paragraph_style_list(
  name = "style_pg_pagenum", font_size = "8pt"
)



style_gr_pink <- new_graphic_style_list(
  name = "style_gr_pink", fill_type = "solid",
  fill_color = "#FF00FF", decorative = FALSE
)

style_gr_blue <- new_graphic_style_list(
  name = "style_gr_blue", fill_type = "solid", fill_color = "#3c26ff",
  stroke_color = "#ff0000", decorative = FALSE
)

# Put all of our styles in a list.
styles <- list(
  style_pg_chris, style_pg_center, style_pg_pagenum,
  style_gr_pink, style_gr_blue
)

# Create our first slide. Give it a catchy title.
slide1 <- slide_list(name = "A Great Slide (with title for accessibility)")

# Creating a text box and apply some styling.
text_box_1 <- text_box_list(
  text = "Hello\nfriends!", width = "10cm",
  height = "2cm", x = "1cm", y = "5cm",
  draw_text_style_name = "chris"
)

# Another text box, different styling.
text_box_2 <- text_box_list(
  text = "Happy centered text!",
  width = "10cm", height = "3cm", x = "1cm", y = "8cm",
  draw_text_style_name = "style_pg_center"
)

# Create a pink ellipse. Note that it's pink because we're applying a style we
# defined above.
pink_ellipse <- new_custom_shape_list(
  type = "ellipse", width = "5cm", height = "9cm", x = "10cm", y = "3cm",
  draw_style_name = "style_gr_pink", text = "ELLIPSE!!!"
)

# Create a blue rectangle.
blue_rectangle <- new_custom_shape_list(
  type = "rectangle", width = "15cm", height = "2cm", x = "4cm", y = "12cm",
  draw_style_name = "style_gr_blue", text = "RECTANGLE!!!"
)

# Create a styled page number. This is a field and so can be added on each page.
page_num <- text_box_list(
  draw_text_style_name = "style_pg_pagenum",
  width = "1cm", height = "1cm", x = "26.9cm", y = "14.65cm",
  text = field_page_num_list()
)

# Now we add all of our items to our current list.
slide1 <- slide1 |>
  add_to_slide(text_box_1) |>
  add_to_slide(text_box_2) |>
  add_to_slide(pink_ellipse) |>
  add_to_slide(blue_rectangle) |>
  add_to_slide(page_num)

# Then we append the current slide to the list of slides.
# Note! For now you need to ensure the slide is in a list itself.
# Once the api stabilizes it would be good to refactor this.
slides <- append(slides, list(slide1))

# Add a joke slide. Here we define the slide and then pipe a text box straight into it.
# We also add the slide number. (You might want to add it later for accessibility reasons.)
slide_sin <- slide_list("Sine wave") |>
  add_to_slide(
    text_box_list(
      text = "A spoooooky sine wave!",
      height = "1cm", width = "10cm", x = "1cm", y = "1cm"
    )
  ) |>
  add_to_slide(page_num)

# Let's set up a repeating list of letters.
sin_letters <- paste0(rep(x = "HAPPY HALLOWE'EN ", times = 10), collapse = "") |>
  strsplit(split = "") |>
  unlist()

# Then we'll put 50 sequential letters on the slide, with linearly increasing x and
# sine-wavy y coordinates.
for (x in seq(from = 1, to = 50, by = 1)) {
  text_box <- text_box_list(
    text = sin_letters[[x]],
    width = "1cm", height = "1cm",
    x = paste0(x / 2, "cm"),
    y = paste0(10 + 5 * sin(x / 10), "cm")
  )
  slide_sin <- add_to_slide(slide_sin, text_box)
}

# add this slide to the slides.
slides <- append(slides, list(slide_sin))

# set up a timestamped filename
filename <- paste0("test-", Sys.time(), ".odp") |>
  gsub(pattern = ":", replacement = "-")


# Save the fonts, styles, and slides, and save an odp file in our current working folder.
# Up until this point, `fonts`, `styles`, and `slides` have all been simple R lists.
# Here, at the last possible moment, we convert them to XML invisibly.
deck |>
  write_fonts(fonts) |>
  write_styles(styles) |>
  write_slides(slides) |>
  write_manifest() |>
  save_pres(filename)




### manifest
