# draw:frame : container for other things
# <draw:frame draw:style-name="gr2" draw:text-style-name="P4"
# draw:layer="layout" svg:width="12cm" svg:height="2cm" svg:x="7cm" svg:y="8cm">

#' Create a `draw:frame` list object.
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param  width Character. Width in cm. e.g. "10cm"
#' @param  height Character. Height in cm. e.g. "10cm"
#' @param  x Character. Leftmost position in cm. e.g. "10cm"
#' @param  y Character. Topmost position in cm. e.g. "10cm"
#' @param draw_layer Character. Default "layout".
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param draw_text_style_name Character. The text style to apply. Default "P1".
#' @returns A draw:frame list object.
#' @export
draw_frame_list <- function(
    width, height, x, y,
    draw_layer = "layout",
    draw_style_name = "gr1",
    draw_text_style_name = "P1") {
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


#' Create a textbox list including its wrapping draw_frame
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param text The text to display. Linebreaks will be parsed into separate paragraphs.
#' @param   width Character. Width in cm. e.g. "10cm"
#' @param  height Character. Height in cm. e.g. "10cm"
#' @param  x Character. Leftmost position in cm. e.g. "10cm"
#' @param  y Character. Topmost position in cm. e.g. "10cm"
#' @param draw_layer Character. Default "layout".
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param draw_text_style_name Character. The text style to apply. Default "P1".
#' @returns A textbox list item.
#' @export
text_box_list <- function(
    text, width, height, x, y,
    draw_layer = "layout", draw_style_name = "gr1", draw_text_style_name = "P1") {
  frame <- draw_frame_list(width, height, x, y, draw_layer, draw_style_name, draw_text_style_name)

  textbox <- text_box_list2(text, draw_text_style_name = draw_text_style_name)

  frame$children <- append(frame$children, list(textbox))
  frame
}

#' Internal function that just creates the  text box, but the main funciton also wraps it in a draw:frame
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param text Character. The text to display.
#' @param draw_text_style_name Character. The name of the text style to apply.
#' @returns Description of what the function returns.
text_box_list2 <- function(text, draw_text_style_name) {
  # each line is wrapped in its own p for linebreaks
  list(
    type = "draw:text-box",
    attributes = c(),
    children = text_p_list(text, draw_text_style_name)
  )
}

#' Create a text:p item in list format
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param text Character. The text to display.
#' @param text_style_name Character. The name of the text style to apply.
#' @returns A list of one or more text:p list items.
#' @export
text_p_list <- function(text, text_style_name) {
  text |>
    maybe_split_text_lines() |>
    lapply(\(line) do_text_p_list(text = line, text_style_name = text_style_name))
}

# internal function
do_text_p_list <- function(text, text_style_name) {
  list(
    type = "text:p",
    attributes = c(`text:style-name` = text_style_name),
    children = list(text)
  )
}

# if text is a string, split it at any linebreaks
maybe_split_text_lines <- function(text) {
  if (is.character(text)) {
    strsplit(x = text, split = "\n") |>
      unlist()
  } else {
    text
  }
} # end function maybe_split_text_lines

#' Create a slide item in list format
#'
#' @param name Character. Slide name. Will go in e.g. pdf index. Important for accessibility.
#' @returns Description of what the function returns.
#' @export
slide_list <- function(name = "slide") {
  list(
    type = "draw:page",
    attributes = c(`draw:name` = name),
    children = list()
  )
}

#' Create an image item in list format
#'
#' Only works with png or svg images. This function has side
#' effects and creates a temp copy of the image on disk to it
#' can be embedded in the output odp file.
#'
#' @param img_filepath Character. Path to image to be inserted.
#' @param  width Character. Width in cm. e.g. "10cm"
#' @param  height Character. Height in cm. e.g. "10cm"
#' @param  x Character. Leftmost position in cm. e.g. "10cm"
#' @param  y Character. Topmost position in cm. e.g. "10cm"
#' @param alt_text Character. Alt text for image. Important for accessibility.
#' @param draw_layer Character. Default "layout".
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param draw_text_style_name Character. The text style to apply. Default "P1".
#' @returns An image object that can be added to a slide.
#' @export
image_list <- function(img_filepath, width, height, x, y, alt_text, draw_layer = "layout", draw_style_name = "gr1", draw_text_style_name = "P1") { # nolint

  #  img path local to presentation folder
  img_filename <- strsplit(x = img_filepath, split = "/") |>
    unlist() |>
    utils::tail(1) |>
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

#' Recursive funciton to parse list into xml.
#'
#' @param item A list representing an XML data structure. Each node is
#'             represented as a list with the following named members.
#'             type: Character. The type of node. E.g. "draw:image"
#'             attrbutes: Named character vector, with names as attribute
#'                        names and values as attribute values.
#'             children:  NULL or character or list. If character, raw text.
#'                        If list, list of child nodes of this same type.
#' @returns An XML tree created by the package xml2.
#' @export
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
} # end function list_item_to_xml()


# Plaintext version of empty LibreOffice presentation. Loaded and parsed in new_pres()
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


#' Create a new presentation.
#'
#' This returns an XML object and also has side effects on disk.
#'
#' @returns A new presentation object, plus side effects on disk to set it up.
#' @export
new_pres <- function() {
  Sys.setenv("temp_dir" = sprintf("%s/pres", tempdir()))
  unlink(x = Sys.getenv("temp_dir"), recursive = TRUE, force = TRUE) |> suppressWarnings()
  dir.create(Sys.getenv("temp_dir"), showWarnings = FALSE)
  file.copy(
    from = paste0(system.file("extdata", package = "odp"), "/empty_presentation.odp"),
    to = Sys.getenv("temp_dir"),
    overwrite = TRUE
  )

  unzip_command <- sprintf("cd %s; unzip -o empty_presentation.odp", Sys.getenv("temp_dir"))
  system(unzip_command, ignore.stdout = TRUE)
  file.remove(paste0(Sys.getenv("temp_dir"), "/empty_presentation.odp"))

  xml2::read_xml(empty_xml_text)
}


#' Save presentation as compressed ODP file in the current working directory and return the input document.
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param doc The deck object to save.
#' @param filename The filename in the current working folder to save the output.
#' @returns Description of what the function returns.
#' @export
save_pres <- function(doc, filename) {
  # Save the context.xml file to disk
  content_xml_filename <- paste0(Sys.getenv("temp_dir"), "/content.xml")
  xml2::write_xml(x = doc, file = content_xml_filename)

  # compress it
  zip_cmd <- sprintf("cd %s; zip -r output.odp * -x *.odt -x *.odp", Sys.getenv("temp_dir"))
  system(zip_cmd, ignore.stdout = TRUE)

  # copy to current working folder
  from_path <- paste0(Sys.getenv("temp_dir"), "/output.odp")
  to_path <- paste0(getwd(), "/", filename)
  file.copy(from = from_path, to = to_path, overwrite = TRUE)

  # return input invisibly in case there's more piping to do
  invisible(doc)
} # end funciton save_pres()




#' Write styles in deck object. Styles must be list of list style items
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param doc A deck object.
#' @param styles A list of styles created new_paragraph_style_list() or
#'               odp::new_graphics_style_list().
#' @returns An updated deck object with the styles added to it.
#' @export
write_styles <- function(doc, styles) {
  automatic_styles <- xml2::xml_child(doc, "office:automatic-styles")
  lapply(
    styles,
    \(style) xml2::xml_add_child(automatic_styles, list_item_to_xml(style))
  )
  doc
} # end function write_styles

#' Write slides in deck object. Slides must be list of list style items
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param doc A deck object.
#' @param slides A list of slides.
#' @returns The deck object with the slides added.
#' @export
write_slides <- function(doc, slides) {
  pres_node <- xml2::xml_find_first(doc, ".//office:presentation")

  lapply(
    slides,
    \(slide) xml2::xml_add_child(pres_node, list_item_to_xml(slide))
  )
  doc
} # end function write_slides


#' Write fonts in deck object. Fonts must be list of list font items
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param doc A deck object.
#' @param fonts A list of font declarations from new_font_list().
#' @returns The deck object with the fonts applied.
#' @export
write_fonts <- function(doc, fonts) {
  fonts_node <- xml2::xml_child(doc, "office:font-face-decls")

  lapply(
    fonts,
    \(font) xml2::xml_add_child(fonts_node, list_item_to_xml(font))
  )

  doc
} # end function write_slides



#' Create new paragraph style.
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param  name The style's name. This is used in text_box_list() to apply the style.
#' @param font_weight Character. c("regular", "bold").
#' @param font_style Character. c("regular", "italic").
#' @param font_size Character. Font size in pts. Default "12pt".
#' @param color Character. Text colour in hex format. Default "#000000".
#' @param text_align Character. c("start", "center", "end").
#' @param opacity Character. Opacity in percent. Default "100%".
#' @param font_name character. Default "Liberation Sans".
#' @returns A paragraph style list item.
#' @export
new_paragraph_style_list <- function(
    name,
    font_weight = c("regular", "bold"),
    font_style = c("regular", "italic"),
    font_size = "12pt",
    color = "#000000",
    text_align = c("start", "center", "end"),
    opacity = "100%",
    font_name = "Liberation Sans") {
  font_weight <- match.arg(font_weight, font_weight)
  font_style <- match.arg(font_style, font_style)
  text_align <- match.arg(text_align, text_align)

  style_list <- list(
    `type` = "style:style",
    `attributes` = c(
      `style:name` = name,
      `style:family` = "paragraph"
    ),
    children = list(
      list(
        `type` = "style:text-properties",
        `attributes` = c(
          `fo:font-weight` = font_weight,
          `fo:font-style` = font_style,
          `fo:font-size` = font_size,
          `fo:color` = color,
          `loext:opacity` = opacity,
          `style:font-name` = font_name
        )
      ),
      list(
        `type` = "style:paragraph-properties",
        `attributes` = c(
          `fo:text-align` = text_align
        )
      )
    )
  )

  style_list
} # end function new_paragraph_style_list

# <style:font-face style:name="FreeSans" svg:font-family="FreeSans" style:font-family-generic="system" style:font-pitch="variable"/> # nolint
#' Define new font for use in the deck.
#'
#' Does not install fonts or check that they are available.
#'
#' @param     name Character. Name of the font.
#' @param font_family_generic Character. Default "system".
#' @param    font_pitch  Character. Default "variable".
#' @returns A new font declaration in list format.
#' @export
new_font_list <- function(
    name,
    font_family_generic = "system",
    font_pitch = "variable") {
  list(
    `type` = "style:font-face",
    attributes = c(
      `style:name` = name,
      `svg:font-family` = name,
      `style:font-family-generic` = font_family_generic,
      `style:font-pitch` = font_pitch
    )
  )
}



#
#' Add item to slide.  slide and item must both be list items
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param slide A slide to add the item to.
#' @param item An item to add to the slide. E.g. output of text_box_list(), new_custom_shape_list().
#' @returns The input slide with the new item added to it.
#' @export
add_to_slide <- function(slide, item) {
  slide$children <- append(slide$children, list(item))

  slide
}


#' Create new custom shape.
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param type  Character. c("rectangle", "ellipse")
#' @param width Character. Width in cm. e.g. "10cm"
#' @param height Character. Height in cm. e.g. "10cm"
#' @param x Character. Leftmost position in cm. e.g. "10cm"
#' @param y Character. Topmost position in cm. e.g. "10cm"
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param text_style_name Character. The text style to apply. Default "P1".
#' @param text Character. Text to include in shape. Default "".
#' @returns Description of what the function returns.
#' @export
new_custom_shape_list <- function(
    type = c("rectangle", "ellipse"),
    width,
    height,
    x,
    y,
    draw_style_name = "gr1",
    text_style_name = "P1",
    text = "") {
  type <- match.arg(type, type)

  list(
    `type` = "draw:custom-shape",
    attributes = c(
      `draw:style-name` = draw_style_name,
      # `draw:text-style-name` = text_style_name,
      `draw:layer` = "layout",
      `svg:width` = width,
      `svg:height` = height,
      `svg:x` = x,
      `svg:y` = y
    ),
    children = append(
      text_p_list(text, text_style_name),
      list(draw_enhanced_geometry_list(type))
    )
  )
}

#  Internal function. Create enhanced geometry item. Not used by user
# @param type Character. c("ellipse", "rectangle").
# @returns A list defining a basic geometry shape.
draw_enhanced_geometry_list <- function(type = c("ellipse", "rectangle")) {
  # <draw:enhanced-geometry svg:viewBox="0 0 21600 21600"
  # draw:glue-points="10800 0 3163 3163 0 10800 3163 18437 10800 21600 18437 18437 21600 10800 18437 3163" #nolint
  # draw:text-areas="3163 3163 18437 18437" draw:type="ellipse" draw:enhanced-path="U 10800 10800 10800 10800 0 360 Z N"/> #nolint
  if (type == "ellipse") {
    list(
      `type` = "draw:enhanced-geometry",
      attributes = c(
        `svg:viewBox` = "0 0 21600 21600",
        `draw:glue-points` = "10800 0 3163 3163 0 10800 3163 18437 10800 21600 18437 18437 21600 10800 18437 3163",
        `draw:text-areas` = "3163 3163 18437 18437",
        `draw:type` = "ellipse",
        `draw:enhanced-path` = "U 10800 10800 10800 10800 0 360 Z N"
      ),
      children = list()
    )
  } else if (type == "rectangle") {{
    # <draw:enhanced-geometry svg:viewBox="0 0 21600 21600" draw:type="rectangle" draw:enhanced-path="M 0 0 L 21600 0 21600 21600 0 21600 0 0 Z N"/> # nolint
    list(
      `type` = "draw:enhanced-geometry",
      attributes = c(
        `svg:viewBox` = "0 0 21600 21600",
        `draw:type` = "rectangle",
        `draw:enhanced-path` = "M 0 0 L 21600 0 21600 21600 0 21600 0 0 Z N"
      ),
      children = list()
    )
  }}
} # end function draw_enhanced_geometry_list()



# <style:style style:name="gr1" style:family="graphic" style:parent-style-name="standard">
# <style:graphic-properties svg:stroke-color="#ff0000" svg:stroke-opacity="100%"
# draw:fill="solid" draw:fill-color="#2a6099" draw:opacity="100%"
#  draw:textarea-horizontal-align="justify" draw:textarea-vertical-align="middle" draw:auto-grow-height="false"
#  fo:min-height="4.25cm" fo:min-width="4cm" loext:decorative="false"/>
# <style:paragraph-properties style:writing-mode="lr-tb"/>
# </style:style>

#' Define a new graphics style.
#'
#' Description of what the function does.
#' how duplication is measured.
#'
#' @param name Character. Name of the style, used later to apply it.
#' @param stroke_color Character. Stroke colour in hex format. Default "#000000".
#' @param stroke_opacity Character. Default "100%".
#' @param fill_type Character. c("none", "solid").
#' @param fill_color Character. Default "#FFFFFF".
#' @param fill_opacity Character. Default "100%".
#' @param decorative Boolean. Default TRUE.
#' @returns A new graphic style item.
#' @export
new_graphic_style_list <- function(
    name,
    stroke_color = "#000000",
    stroke_opacity = "100%",
    fill_type = c("none", "solid"),
    fill_color = "#FFFFFF",
    fill_opacity = "100%",
    decorative = TRUE) {
  fill_type <- match.arg(fill_type)

  style_list <- list(
    `type` = "style:style",
    `attributes` = c(
      `style:name` = name,
      `style:family` = "graphic",
      `style:parent-style-name` = "standard"
    ),
    children = list(
      list(
        `type` = "style:graphic-properties",
        attributes = c(
          `svg:stroke-color` = stroke_color,
          `svg:stroke-opacity` = stroke_opacity,
          `draw:fill` = fill_type,
          ` draw:fill-color` = fill_color,
          `draw:opacity` = fill_opacity,
          `draw:auto-grow-height` = "false",
          `loext:decorative` = tolower(decorative)
        )
      )
    )
  )

  style_list
} # end function new_graphic_style_list()


#' A field for the current page/slide number.
#'
#' This field item should be used inside a text box.
#'
#' @returns A field showing the current page number.
#' @export
field_page_num_list <- function() {
  list(
    list(
      `type` = "text:page-number",
      children = c("&lt;number&gt;")
    )
  )
} # end function field_page_num_list()




#' Write the manifest.xml for any images added to the presentation
#'
#' Returns the deck object unmodified, but has side-effects on disk.
#'
#' @param deck A deck object.
#' @returns The deck unmodified, but has side-effects on disk.
#' @export
write_manifest <- function(deck) {
  temp_dir <- Sys.getenv("temp_dir")

  # skip if no pictures, no need to update manifest
  if (!dir.exists(paste0(temp_dir, "/Pictures"))) {
    return(deck)
  }

  image_files <- paste0("Pictures/", list.files(paste0(temp_dir, "/Pictures")))

  manifest_xml <- xml2::read_xml(paste0(temp_dir, "/META-INF/manifest.xml"))

  lapply(image_files, \(filename) xml2::xml_add_child(manifest_xml, create_manifest_img_xml(filename)))
  xml2::write_xml(x = manifest_xml, file = paste0(temp_dir, "/META-INF/manifest.xml"))

  deck
} # end function write_manifest()

# for an image added to the deck, create a new child node for the manifest.xml
# called from write_manifest()
create_manifest_img_xml <- function(filename) {
  media_type <- if (grepl(x = tolower(filename), pattern = ".png", fixed = TRUE)) {
    "image/png"
  } else if (
    grepl(x = tolower(filename), pattern = ".svg", fixed = TRUE)
  ) {
    "image/svg+xml"
  } else {
    stop(sprintf("Problem with image file %s: Images should be PNG or SVG.", filename))
  }

  xml_txt <- sprintf('<manifest version="1.4" xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" >
   <manifest:file-entry manifest:full-path="%s" manifest:media-type="%s" /> </manifest>', filename, media_type)

  xml2::read_xml(xml_txt) |>
    xml2::xml_child()
} # end function create_manifest_img_xml()
