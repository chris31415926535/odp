# odp

<!-- badges: start -->
<!-- badges: end -->

The goal of odp is to create work-ready Open Document Presentation files with no external dependencies and that works well with open-source software like LibreOffice.

At present this project is **not** intended to implement the full Open Document technical specification. It is pragmatically focused on what is needed to produce work-ready office documents, namely:

- Slides with titles;
- Text boxes with styling (size, location, colour, font, text size);
- Images with alt text; and,
- Shapes (lines, boxes, etc.) with styling (outline, fill colour, etc.).

## Motivation

The R package [_officer_](https://ardata-fr.github.io/officeverse/index.html) is very nice but only works with Microsoft Powerpoint .pptx files. _officer_ is not able to work with .pptx files saved by LibreOffice, and LibreOffice is also seemingly not able to properly edit and add placeholders to .pptx master slides.

## Installation

You can install the development version of odp like so:

```r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

```r
library(odp)

# Create a new presentation. This has invisible side effects!
# The function returns an XML object and ALSO
# initializes a new document in a temporary folder.
# Note that for now you can only work on one presentation at a time!
deck <- new_pres()

# initialize a new font
fonts <- list(new_font_list(name = "FreeSerif"))

# define a new style, and name it "chris"
chris_style <- new_paragraph_style_list(
  name = "chris", color = "#6502ff", font_weight = "bold", font_name = "FreeSerif"
)

# our set of custom styles needs to live in a list
styles <- list(chris_style)

# create slide
slide1 <- slide_list(name = "SLIDE TITLE FOR ACCESSIBILITY")

# create two text boxes, one styled and the other generic
text_box_1 <- text_box_list(
  text = "happy chris", width = "10cm",
  height = "2cm", x = "1cm", y = "5cm",
  draw_text_style_name = "chris"
)

text_box_2 <- text_box_list(text = "happy generic", width = "10cm", height = "3cm", x = "1cm", y = "8cm")

# add the two text boxes to our slide
slide1 <- slide1 |>
  add_to_slide(text_box_1) |>
  add_to_slide(text_box_2)

# our final set of slides must be a list.
# Here we'll repeat slide 1 three times.
slides <- list(slide1, slide1, slide1)

# set up a timestamped filename
filename <- paste0("test-", Sys.time(), ".odp") |> stringr::str_replace_all(":", "-")

# take our document, and save the fonts, styles, and slides, then save to our current working folder
deck |>
  write_fonts(fonts) |>
  write_styles(styles) |>
  write_slides(slides) |>
  save_pres(filename)

```
