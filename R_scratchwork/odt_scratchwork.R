# zip -r ../test2.odt *
library(XML)
library(xml2)
# source("/home/christopher/datascience/R/ewhs2024/analyses/10.demographic_analyses/R/odt_functions.R", encoding = "UTF-8")

# setwd("/home/christopher/datascience/R/ewhs2024/analyses/10.demographic_analyses/test")
# filepath <- "~/datascience/R/ewhs2024/analyses/10.demographic_analyses/test/test1/"
# filename <- paste0(filepath, "content.xml")
# content_xml <- xml2::read_xml(filename)

# content_xml


# body_node <- xml2::xml_child(content_xml, search = 4)

# presentation_node <- xml2::xml_child(body_node, search = 1)

# page_node <- presentation_node |>
#   xml2::xml_child(search = 1)

# # create a temp xml so we can edit this node in peace
# temp <- xml2::read_xml("<xml/>")
# xml2::xml_add_child(temp, .value = page_node)

# temp_page <- xml2::xml_child(temp)

# xml2::xml_set_attr(temp_page, attr = "draw:name", value = "an incredible new slide")



# # add the temp node to the document
# xml2::xml_add_child(
#   presentation_node,
#   #  .value = "page",
#   .value = temp_page # ,
#   # name = "page2"
# )

# # save the updated document
# xml2::write_xml(x = content_xml, file = filename)

# # THIS IS HOW WE HAVE TO SAVE
# cwd <- getwd()
# setwd("analyses/10.demographic_analyses/test/test1/")
# # system("cd analyses/10.demographic_acnalyses/test/test1/")
# system("zip -r ../test5.odp * -x *.odt *.odp")
# setwd(cwd)

# temp
# as.character(temp_page)
# ?read_xml
# presentation_node
# xml2::xml_child(presentation_node, search = 1)
# xml2::xml_child(presentation_node, search = 2)
# xml2::xml_child(presentation_node, search = 3)
# xml2::xml_child(presentation_node, search = 4)


# xml2::xml_child(temp_page, 3)

devtools::load_all()



deck <- new_pres()

pres_node <- xml2::xml_find_first(deck, ".//office:presentation")

# create list
page1 <- slide_list()

# test creating a text box
text_box_1 <- text_box_list(text = "happy", width = "10cm", height = "2cm", x = "1cm", y = "5cm")
page1$children <- append(page1$children, list(text_box_1))

# test something wacky
i <- 1
num <- 20
for (i in 1:20) {
  mid_x <- 12
  mid_y <- 8
  r <- 5

  x <- sprintf("%.2fcm", mid_y + sin(2 * 3.14159 / num * i) * r)
  y <- sprintf("%.2fcm", mid_y + cos(2 * 3.14159 / num * i) * r)
  tb <- text_box_list(text = "X", width = "1cm", height = "1cm", x = x, y = y)
  page1$children <- append(page1$children, list(tb))
}

# page1$children


## OPTONAL, ADD A PICTURE

# img <- image_list(
#   img_filepath = "~/datascience/R/ewhs2024/analyses/10.demographic_analyses/output/fig/svg/burnout_exhaustion_fct_hilo-demo_ee_black-1-overall-2025-10-30.svg",
#   alt_text = "testing",
#   width = "10cm", height = "20cm", x = "5cm", y = "5cm"
# )

# page1$children <- append(page1$children, list(img))

# make simple page 2

page2 <- slide_list(name = "slide the second")
text_box_2 <- text_box_list(text = "happy page 2!!!", width = "10cm", height = "2cm", x = "1cm", y = "5cm")
page2$children <- append(page2$children, list(text_box_2))

# text_box_2 <- text_box_list(text = "happy page 2!", width = "10cm", height = "2cm", x = "1cm", y = "5cm")
# page2$children <- append(page2$children, list(text_box_2))

### TEST KEEPING PAGES AS LIST AND THEN CHANGE TO XML AND APPEND ONLY AT END?
pages <- list(page1, page2)

list_item_to_xml(page1)
list_item_to_xml(page2)

purrr::map(pages, list_item_to_xml) |>
  purrr::map(\(node) xml2::xml_add_child(pres_node, node))


# convert page list to page xml node, add as child to presentation node
# page_xml <- list_item_to_xml(page1)
# xml2::xml_add_child(pres_node, page_xml)

####
# create one page per iamge?

dir <- "~/datascience/R/ewhs2024/analyses/10.demographic_analyses/output/fig/2025-11-06/svg/"

files <- list.files(dir)

file_paths <- paste0(dir, files)


# setwd("/home/christopher/datascience/R/ewhs2024/analyses/10.demographic_analyses/test/test1")

i <- 1
file_ids <- seq_along(files)
# file_ids <- 1:10
for (i in file_ids) {
  message(i)

  file <- files[[i]]
  file_path <- file_paths[[i]]

  page <- slide_list(name = file)

  tb <- text_box_list(text = file, width = "28cm", height = "1.5cm", x = "0cm", y = "0cm")

  img <- image_list(
    img_filepath = file_path,
    alt_text = file,
    width = "28cm", height = "14.25cm", x = "0cm", y = "1.5cm"
  )

  page$children <- append(page$children, list(tb))
  page$children <- append(page$children, list(img))

  page_xml <- list_item_to_xml(page)
  xml2::xml_add_child(pres_node, page_xml)
}

list(page, page) |> str()

deck

# list.files(path = tempdir())


### SETUP FOR SAVE

# temp_dir <<- sprintf("%s/pres", tempdir())
# dir.create(temp_dir)
# file.copy(from = paste0(getwd(), "/empty_presentation.odp"), to = temp_dir)
# old_wd <- getwd()
# setwd(temp_dir)
# system("unzip -o empty_presentation.odp")
# setwd(old_wd)

# filepath <- "~/datascience/R/ewhs2024/analyses/10.demographic_analyses/test/test1/"
# filename <- paste0(filepath, "content.xml")


filename <- paste0("test-", Sys.time(), ".odp") |> stringr::str_replace_all(":", "-")

save_pres(deck, filename)



# getwd()
# setwd("/home/christopher/datascience/R/ewhs2024/analyses/10.demographic_analyses/test/test1")
# system("zip -r ../test6.odp * -x *.odt -x *.odp")



# empty_xml_template <- '<document-content version="1.4" xmlns:anim="urn:oasis:names:tc:opendocument:xmlns:animation:1.0" xmlns:smil="urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0" xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:css3t="http://www.w3.org/TR/css3-text/" xmlns:grddl="http://www.w3.org/2003/g/data-view#" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:drawooo="http://openoffice.org/2010/draw" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" xmlns:tableooo="http://openoffice.org/2009/table" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:rpt="http://openoffice.org/2005/report" xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:officeooo="http://openoffice.org/2009/office" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0"> # nolint
# <office:scripts/>
# <office:body><office:presentation>%s</office:presentation></office:body>
# </document-content>
# '



## PARAGRAPH STYLES WORKING!!!!!!

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
  name = "chris", color = "#6502ff", font_weight = "bold", font_name = "FreeSerif", text_align = "end",
)
style_pg_center <- new_paragraph_style_list(
  name = "style_pg_center", text_align = "center"
)

style_gr_pink <- new_graphic_style_list(
  name = "style_gr_pink", fill_type = "solid", fill_color = "#FF00FF", decorative = FALSE
)

style_gr_blue <- new_graphic_style_list(
  name = "style_gr_blue", fill_type = "solid", fill_color = "#3c26ff", stroke_color = "#ff0000", decorative = FALSE
)

# Put all of our styles in a list.
styles <- list(style_pg_chris, style_pg_center, style_gr_pink, style_gr_blue)

# Create our first slide. Give it a catchy title.
slide1 <- slide_list(name = "A Great Slide (with explicit title for accessibility)")

# Creating a text box and apply some styling.
text_box_1 <- text_box_list(
  text = "Hello friends!", width = "10cm",
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

# Now we add all of our items to our current list.
slide1 <- slide1 |>
  add_to_slide(text_box_1) |>
  add_to_slide(text_box_2) |>
  add_to_slide(pink_ellipse) |>
  add_to_slide(blue_rectangle)

# Then we append the current slide to the list of slides.
# Note! For now you need to ensure the slide is in a list itself.
# Once the api stabilizes it would be good to refactor this.
slides <- append(slides, list(slide1))

# Add a joke slide. Here we define the slide and then pipe a text box straight into it.
slide_sin <- slide_list("Sine wave") |>
  add_to_slide(text_box_list(text = "A spoooooky sine wave!", height = "1cm", width = "10cm", x = "1cm", y = "1cm"))

# Let's set up a repeating list of letters.
sin_letters <- paste0(rep(x = "HAPPY HALLOWE'EN ", times = 10), collapse = "") |>
  stringr::str_split("") |>
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
filename <- paste0("test-", Sys.time(), ".odp") |> stringr::str_replace_all(":", "-")

# Take our document, and save the fonts, styles, and slides, then save to our current working folder.
# Up until this point, `fonts`, `styles`, and `slides` have all been simple R lists.
# Here, at the last possible moment, we convert them to XML invisibly.
deck |>
  write_fonts(fonts) |>
  write_styles(styles) |>
  write_slides(slides) |>
  save_pres(filename)

# <loext:graphic-properties draw:fill="none" draw:fill-color="#ffffff"/>
# <style:paragraph-properties fo:text-align="end"/>
# </style:style>
