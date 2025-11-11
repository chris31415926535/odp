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

## Design Principles

_This package is in its very early stages and all of this may change._

I want this package to be simple to use and easy to reason about. This is harder than it seems, because an .odt file is surprisingly complex: it is actually a zip file containing a number of .xml files and other things. And .xml files are particularly awful to work with in R, because they are loaded as a complex extremely un-R-like data structure with lots of modify-in-place side effects and pointers. They are difficult to reason about.

So in _odt_ we take a different tack. We define our presentation primarily using in-memory R objects (lists, but they should eventually be typed classes), and then only convert them to XML after we are done defining them.

At present, however, _odt_ also uses some side-effects. When a new deck is initialized, a temporary folder is created and a blank presentation (from LibreOffice) is unzipped to provide an empty scaffold. This is mostly unused, except that any images added to the presentation are copied into the "Pictures" folder within this template. Then, when the user is ready to save their presentation, they write styles, fonts, and slides to disk and save a local copy of the presentation by compressing the temporary folder.

This has a few implications:

- Only one deck can be loaded per R session. If you try to start another deck, it will mess with/erase the old one.

The process is intended to be:

- Initialize a new deck that returns an R object and creates a working temporary folder;
- Define any styles (think of this like setting your CSS);
- Define any fonts;
- Create a set of slides;
- Write styles, fonts, and slides to disk in the temporary folder;
- Create an odt file by compressing the temporary folder.

## Known issues

- Adding images will create a "broken" file that LibreOffice can fix. I believe this is because I'm not updating manifest.xml

## TODO

- Alt text for images
- 'Decorative' tag for images?
- Shapes
- Links
- manifest.xml
- Refactor in a clever way to do all image copying at write time, so we don't actually need the temp folder the whole time?

## Installation

You can install the development version of odp like so:

```r
devtools::install_github("chris31415926535/odp")
```

## Example

This is a basic example which shows you how to create a simple presentation:

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
  name = "chris", color = "#6502ff", font_weight = "bold", font_name = "FreeSerif", text_align="end"
)

# our set of custom styles needs to live in a list
styles <- list(chris_style)

# create slide
slide1 <- slide_list(name = "SLIDE TITLE FOR ACCESSIBILITY")

# create two text boxes, one styled and the other generic
text_box_1 <- text_box_list(
  text = "Lovely formatted text!", width = "10cm",
  height = "2cm", x = "1cm", y = "5cm",
  draw_text_style_name = "chris"
)

text_box_2 <- text_box_list(text = "Generic uninteresting text.", width = "10cm", height = "3cm", x = "1cm", y = "8cm")

# add the two text boxes to our slide
slide1 <- slide1 |>
  add_to_slide(text_box_1) |>
  add_to_slide(text_box_2)

# Create a joke slide with text in a sine wave pattern
slide_sin <- slide_list("Sine wave") |>
  add_to_slide(text_box_list(text = "A spoooooky sine wave!", height = "1cm", width = "10cm", x = "1cm", y = "1cm"))

sin_letters <- paste0(rep(x = "HAPPY HALLOWE'EN ", times = 10), collapse = "") |>
  stringr::str_split("") |>
  unlist()

for (x in seq(from = 1, to = 50, by = 1)) {
  text_box <- text_box_list(
    text = sin_letters[[x]],
    width = "1cm", height = "1cm",
    x = paste0(x / 2, "cm"),
    y = paste0(10 + 5 * sin(x / 10), "cm")
  )
  slide_sin <- add_to_slide(slide_sin, text_box)
}

# our final set of slides must be a list.
slides <- list(slide1, slide_sin)

# set up a timestamped filename
filename <- paste0("test-", Sys.time(), ".odp") |> stringr::str_replace_all(":", "-")

# take our document, and save the fonts, styles, and slides, then save to our current working folder
deck |>
  write_fonts(fonts) |>
  write_styles(styles) |>
  write_slides(slides) |>
  save_pres(filename)

```
