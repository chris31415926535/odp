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



doc <- new_pres()
pres_node <- xml2::xml_find_first(doc, ".//office:presentation")

# create list
page1 <- page_list()

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

page2 <- page_list(name = "slide the second")
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

  page <- page_list(name = file)

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

doc

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

save_pres(doc, filename)



# getwd()
# setwd("/home/christopher/datascience/R/ewhs2024/analyses/10.demographic_analyses/test/test1")
# system("zip -r ../test6.odp * -x *.odt -x *.odp")



# empty_xml_template <- '<document-content version="1.4" xmlns:anim="urn:oasis:names:tc:opendocument:xmlns:animation:1.0" xmlns:smil="urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0" xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:css3t="http://www.w3.org/TR/css3-text/" xmlns:grddl="http://www.w3.org/2003/g/data-view#" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:drawooo="http://openoffice.org/2010/draw" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" xmlns:tableooo="http://openoffice.org/2009/table" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:rpt="http://openoffice.org/2005/report" xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:officeooo="http://openoffice.org/2009/office" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0"> # nolint
# <office:scripts/>
# <office:body><office:presentation>%s</office:presentation></office:body>
# </document-content>
# '
