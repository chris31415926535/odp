# draw:frame : container for other things
# <draw:frame draw:style-name="gr2" draw:text-style-name="P4"
# draw:layer="layout" svg:width="12cm" svg:height="2cm" svg:x="7cm" svg:y="8cm">

draw_frame_list <- function(
    width, height, x, y, draw_layer = "layout",
    draw_style_name = "gr1", draw_text_style_name = "P1") {
  list(
    type = "draw:frame",
    attributes = c(
      `draw:style-name` = draw_style_name,
      `draw:text-style-name` = draw_text_style_name,
      `draw:layer` = draw_layer,
      `svg:width` = width,
      `svg:height` = height,
      `svg:x` = x,
      `svg:y` = y
    ),
    children = list()
  )
}

# test: create and return a text-box node
# don't think this is used
text_box_xml <- function(text = "") {
  node_text <- sprintf("<draw:text-box><text:p>%s</text:p></draw:text-box>", text)
  xml2::read_xml(node_text)
}


# nolint start

# draw_frame_xml <- function(
#     width, height, x, y,
#     draw_style_name = "gr1", draw_text_style_name = "P1", draw_layer = "layout") {

#   frame_list <- draw_frame_list(draw_style_name, draw_text_style_name, width, height, x, y, draw_layer)


#   node_text <- sprintf("<%s />", frame$type)

#   test <- xml2::read_xml(node_text)
#   test |>
#     xml2::xml_set_attrs(value = frame$attributes)
# }


# node_text <- sprintf("<%s />", frame$type)

# test <- xml2::read_xml(node_text)
# test |>
# xml2::xml_set_attrs(value = frame$attributes)

# text1 <- text_box("Here is some TEXT!")

# test |>
# xml2::xml_add_child(text1)

# width <- "10cm" ; height <- "2cm"; x <- "6.5cm"; y <- "2.22cm"
# nolint end

# create a textbox list including its wrapping draw_frame
text_box_list <- function(
    text, width, height, x, y,
    draw_layer = "layout", draw_style_name = "gr1", draw_text_style_name = "P1") {
  frame <- draw_frame_list(width, height, x, y, draw_layer, draw_style_name, draw_text_style_name)

  textbox <- text_box_list2(text)

  frame$children <- append(frame$children, list(textbox))
  frame
}

# function that just creates the  text box, but the main funciton also wraps it in a draw:frame
text_box_list2 <- function(text) {
  list(
    type = "draw:text-box",
    attributes = c(),
    children = list(
      list(
        type = "text:p",
        attributes = c(),
        children = list(text)
      )
    )
  )
}



page_list <- function(name = "slide") {
  list(
    type = "draw:page",
    attributes = c(`draw:name` = name),
    children = list()
  )
}

# nolint start
# add_text_box <- function(text, width, height, x, y) {
#   frame <- draw_frame(width = "10cm", height = "2cm", x = "1cm", y = "10cm")
# }
# item <- draw_frame_list(width = "10cm", height = "2cm", x = "1cm", y = "5cm")
# nolint end





### ADD A PICTURE
# nolint start
# <draw:frame draw:style-name="gr1" draw:text-style-name="P1" draw:layer="layout" svg:width="11.275cm" svg:height="8.456cm" svg:x="7cm" svg:y="3.044cm">
# <draw:image xlink:href="Pictures/1000261200004F6000003B88DEC6CDB0.svg" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" draw:mime-type="image/svg+xml">
# <text:p/>
# </draw:image>
# <draw:image xlink:href="Pictures/1000000100000300000002409D1A56DD.png" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" draw:mime-type="image/png"/>
# </draw:frame>
# width <- "10cm" ; height <- "2cm"; x <- "6.5cm"; y <- "2.22cm"
# img_filepath <- "~/datascience/R/ewhs2024/analyses/10.demographic_analyses/output/fig/svg/burnout_exhaustion_fct_hilo-demo_ee_black-1-overall-2025-10-30.svg"
# nolint end
image_list <- function(img_filepath, width, height, x, y, alt_text, draw_layer = "layout", draw_style_name = "gr2", draw_text_style_name = "P1") { # nolint

  #  img path local to presentation folder
  img_filename <- stringr::str_split(img_filepath, "/") |>
    unlist() |>
    tail(1) |>
    (\(file) (sprintf(fmt = "Pictures/%s", file)))()

  # destination to copy it to temp folder
  img_filename_destination <- paste0(tempdir(), "/pres/", img_filename)


  if (!dir.exists(paste0(tempdir(), "/pres/Pictures"))) {
    dir.create(
      path = paste0(tempdir(), "/pres/Pictures"),
      recursive = TRUE
    )
  }

  sys_copy_cmd <- sprintf("cp %s %s", img_filepath, img_filename_destination)

  system(sys_copy_cmd)

  frame <- draw_frame_list(width, height, x, y, draw_layer, draw_style_name, draw_text_style_name)

  img <- list(
    type = "draw:image",
    attributes = c(
      `xlink:href` = img_filename,
      `xlink:type` = "simple",
      `xlink:show` = "embed",
      `xlink:actuate` = "onLoad",
      `draw:mime-type` = "image/svg+xml"
    ),
    children = NULL
  )

  alt_text <- list(
    type = "svg:desc",
    attributes = c(),
    children = alt_text
  )

  frame$children <- append(frame$children, list(img))
  frame$children <- append(frame$children, list(alt_text))
  frame
} # end function image_list()



### COVERT LIST TO XML

# recursive funciton to parse list into xml???
list_item_to_xml <- function(item) {
  node <- xml2::read_xml(sprintf("<%s />", item$type)) |>
    suppressWarnings()

  if (!is.null(item$attributes)) {
    xml2::xml_set_attrs(node, value = item$attributes) |>
      suppressWarnings()
  }
  node


  for (child in item$children) {
    if (is.list(child)) {
      child_xml <- list_item_to_xml(child)
      xml2::xml_add_child(node, child_xml)
    } else if (is.character(child)) {
      xml2::xml_set_text(x = node, value = child)
    }
  }

  node
}


# nolint start
empty_xml_text <- '<office:document-content xmlns:anim="urn:oasis:names:tc:opendocument:xmlns:animation:1.0" xmlns:smil="urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0" xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:css3t="http://www.w3.org/TR/css3-text/" xmlns:grddl="http://www.w3.org/2003/g/data-view#" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:drawooo="http://openoffice.org/2010/draw" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" xmlns:tableooo="http://openoffice.org/2009/table" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:rpt="http://openoffice.org/2005/report" xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:officeooo="http://openoffice.org/2009/office" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0" office:version="1.4">
<office:scripts/>

<office:font-face-decls>
<style:font-face style:name="DejaVu Sans" svg:font-family="&apos;DejaVu Sans&apos;" style:font-family-generic="system" style:font-pitch="variable"/>
<style:font-face style:name="FreeSans" svg:font-family="FreeSans" style:font-family-generic="system" style:font-pitch="variable"/>
<style:font-face style:name="Liberation Sans" svg:font-family="&apos;Liberation Sans&apos;" style:font-family-generic="roman" style:font-pitch="variable"/>
<style:font-face style:name="Liberation Serif" svg:font-family="&apos;Liberation Serif&apos;" style:font-family-generic="roman" style:font-pitch="variable"/>
<style:font-face style:name="Noto Sans" svg:font-family="&apos;Noto Sans&apos;" style:font-family-generic="roman" style:font-pitch="variable"/>
<style:font-face style:name="Unifont" svg:font-family="Unifont" style:font-family-generic="system" style:font-pitch="variable"/>
</office:font-face-decls>
<office:automatic-styles>
<style:style style:name="dp1" style:family="drawing-page">
<style:drawing-page-properties presentation:background-visible="true" presentation:background-objects-visible="true" presentation:display-footer="true" presentation:display-page-number="false" presentation:display-date-time="true"/>
</style:style>
<style:style style:name="dp2" style:family="drawing-page">
<style:drawing-page-properties presentation:display-header="true" presentation:display-footer="true" presentation:display-page-number="false" presentation:display-date-time="true"/>
</style:style>
<style:style style:name="gr1" style:family="graphic">
<style:graphic-properties draw:fill="none" draw:stroke="none" style:protect="size" loext:decorative="false"/>
</style:style>
<style:style style:name="pr1" style:family="presentation" style:parent-style-name="Default-notes">
<style:graphic-properties draw:fill-color="#ffffff" draw:auto-grow-height="false" fo:min-height="13.365cm" loext:decorative="false"/>
<style:paragraph-properties style:writing-mode="lr-tb"/>
</style:style>
<style:style style:name="P1" style:family="paragraph">
<loext:graphic-properties draw:fill-color="#ffffff"/>
</style:style>
</office:automatic-styles>

<office:body><office:presentation/></office:body>
</office:document-content>
'
# nolint end

new_pres <- function() {
  Sys.setenv("temp_dir" = sprintf("%s/pres", tempdir()))
  file.remove(path = Sys.getenv("temp_dir"), recursive = TRUE) |> suppressWarnings()
  dir.create(Sys.getenv("temp_dir"), showWarnings = FALSE)
  file.copy(
    from = paste0(system.file("extdata", package = "odp"), "/empty_presentation.odp"),
    to = Sys.getenv("temp_dir"),
    overwrite = TRUE
  )
  old_wd <- getwd()
  setwd(Sys.getenv("temp_dir"))
  system("unzip -o empty_presentation.odp", ignore.stdout = TRUE)
  setwd(old_wd)

  xml2::read_xml(empty_xml_text)
}


# save, copy to working folder
# uses global temp_dir
# should we use getenv / setenv?
save_pres <- function(doc, filename) {
  old_wd <- getwd()
  setwd(Sys.getenv("temp_dir"))

  content_xml_filename <- paste0(Sys.getenv("temp_dir"), "/content.xml")
  xml2::write_xml(x = doc, file = content_xml_filename)
  zip_cmd <- "zip -r output.odp * -x *.odt -x *.odp"
  system(zip_cmd, ignore.stdout = TRUE)
  from_path <- paste0(Sys.getenv("temp_dir"), "/output.odp")
  to_path <- paste0(old_wd, "/", filename)
  file.copy(from = from_path, to = to_path, overwrite = TRUE)
  setwd(old_wd)
}
