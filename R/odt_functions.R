# draw:frame : container for other things
# <draw:frame draw:style-name="gr2" draw:text-style-name="P4"
# draw:layer="layout" svg:width="12cm" svg:height="2cm" svg:x="7cm" svg:y="8cm">

#' Create a `draw:frame` list object.
#'
#' This is used  internally by other functions.
#'
#' This is the main function for adding text to your slides. Text boxes are
#' the highest-level text objects, and they set the geometry for the text:
#' where it will go, how large the shape will be, and so on.
#'
#' Text inside the boxes is provided as a list of text_p() (paragraph) or
#' new_list() (list) objects.
#' @param width Character. Width in cm. e.g. "10cm"
#' @param height Character. Height in cm. e.g. "10cm"
#' @param x Character. Leftmost position in cm. e.g. "10cm"
#' @param y Character. Topmost position in cm. e.g. "10cm"
#' @param draw_layer Character. Default "layout".
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param draw_text_style_name Character. The text style to apply. Default "P1".
#' @returns A draw:frame list object.
#' @export
draw_frame <- function(
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


#' Create a text box list including its wrapping draw_frame
#'
#' This is the main function for adding text to your slides. Text boxes are
#' the highest-level text objects, and they set the geometry for the text:
#' where it will go, how large the shape will be, and so on.
#'
#' Text inside the boxes is provided as a list of text_p() (paragraph) or
#' new_list() (list) objects.
#'
#' @param text List. The text to display as a series of text_p() or new_list() objects.
#' @param width Character. Width in cm. e.g. "10cm"
#' @param height Character. Height in cm. e.g. "10cm"
#' @param x Character. Leftmost position in cm. e.g. "10cm"
#' @param y Character. Topmost position in cm. e.g. "10cm"
#' @param draw_layer Character. Default "layout".
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param draw_text_style_name Character. The text style to apply. Default "P1".
#' @returns A textbox list item.
#' @export
text_box <- function(
    text, width, height, x, y,
    draw_layer = "layout", draw_style_name = "gr1", draw_text_style_name = "P1") {
  frame <- draw_frame(width, height, x, y, draw_layer, draw_style_name, draw_text_style_name)
  #
  # FOR NOW! textbox3 requires inputs to be list of text:p lists! FIXME
  textbox <- text_box3(text, draw_text_style_name = draw_text_style_name)

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
text_box3 <- function(text, draw_text_style_name) {
  # each line is wrapped in its own p for linebreaks
  # FOR NOW text must be list of text:p lists! FIXME
  stopifnot(is.list(text))
  stopifnot(text[[1]]$type %in% c("text:p", "text:list"))
  list(
    type = "draw:text-box",
    attributes = c(),
    children = text
  )
} # end function text_box3


#' Create a text:p item in list format
#'
#' Contents must be provided in ONE of two ways. First, they can be provided
#' as comma-separated arguments which will be processed by R's "..." argument.
#' Arguments provided this way should be results of calls to text_span().
#'
#' OR, arguments can be provided in a list of text_span() objects provided to
#' the function parameter `contents_list`. This is useful if e.g. you want to
#' prepare the contents procedurally and then add then to a paragraph later.
#'
#' If `contents_list` is provided, it overrides anything provided to `...`.
#'
#' @param ... Comma-separated text_span() items. The text to display.
#' @param contents_list List. A list of text_span() items prepared previously.
#'        Overrides any values provided to `...`.
#' @param text_style_name Character. Name of new_paragraph_style() output style.
#' @returns A list of one or more text:p list items.
#' @export
text_p <- function(..., contents_list = NA, text_style_name = NA) {
  # FOR NOW, say text must be a list. FIXME!
  # FOR NOW, say text must be a list OF SPANS. FIXME!
  # text is either a list of objects in contents_list, or else objects provided unlisted in ...
  text <- if (!all(is.na(contents_list))) {
    if (!is.list(contents_list)) {
      stop("contents_list argument must contain a list() of contents. (Most like text_span() objects.)")
    }
    contents_list
  } else {
    list(...)
  }

  if (!is.list(text)) {
    stop("FOR NOW! text_p() input must be a list.")
  }

  if (length(text) == 0) {
    stop("Must provide at least one child. Either a list of text_span() objects to contents_list, or one or more text_span() objects passed as regualr arguments.") # nolint
  }

  if (!text[[1]]$type %in% c("text:span")) {
    stop("FOR NOW! text_p() input must be list of text:span objects.")
  }

  the_p <- list(
    type = "text:p",
    children = text
  )

  if (!is.na(text_style_name)) {
    the_p$attributes <- c(`text:style-name` = text_style_name)
  }
  the_p
}

# internal function
do_text_p <- function(text, text_style_name) {
  the_p <- list(
    type = "text:p",
    children = list(text)
  )
  if (!is.na(text_style_name)) {
    the_p$attributes <- c(`text:style-name` = text_style_name)
  }

  the_p
} # end function do_text_p()



#' Create a text span in list format
#'
#' Spans are the smallest unit of text. They can be individually styled with a new_text_style()
#' or new_text_style_minimal() style object. They must live inside of text_p() objects. They do
#' not create linebreaks or whitespace if they are next to each other.
#'
#' Spans can contain either raw character text or field objects, e.g. from field_page_num().
#'
#' @param text Character or field list object.
#' @param style_name Character. Optional. Name of new_text_style() or new_text_style_minimal() object.
#' @returns A text_span list object.
#' @export
text_span <- function(text, style_name = NA) {
  # need it to handle plain text, but also maybe object children like fields
  the_children <- if (is.character(text)) {
    text
  } else {
    list(text)
  }

  the_span <- list(
    type = "text:span",
    children = the_children
  )

  if (!is.na(style_name)) {
    the_span$attributes <- c(`text:style-name` = style_name)
  }

  the_span
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
#' Each slide is a new page in your presentation. Slides are defined as lists. Objects are added
#' to slides with the add_to_slide() function.
#'
#' @param name Character. Slide name. Will go in e.g. pdf index. Important for accessibility.
#' @returns A slide object in list format.
#' @export
new_slide <- function(name = "slide") {
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
new_image <- function(img_filepath, width, height, x, y, alt_text, draw_layer = "layout", draw_style_name = "gr1", draw_text_style_name = "P1") { # nolint

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

  frame <- draw_frame(width, height, x, y, draw_layer, draw_style_name, draw_text_style_name)

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
} # end function new_image()



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
  if (is.null(item$type)) {
    stop("Error: property `type` does not exist on object: ", as.character(item))
  }
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
  from <- paste0(system.file("extdata", package = "odp"), "/empty_presentation.odp")
  to <- Sys.getenv("temp_dir")
  utils::unzip(zipfile = from, exdir = to)
  xml2::read_xml(empty_xml_text)
}


#' Save presentation as compressed ODP file in the current working directory and return the input document.
#'
#' This function receives a final deck object, performs disk operations as a side effect to create
#' an output file, and then returns the original deck object invisibly.
#'
#' This is the *final step* and should be done after calling write_fonts(), write_styles(), write_manifest(),
#' and write_slides(). They can be conveniently piped together, with `save_pres()` as the final step.
#'
#' @param doc The deck object to save.
#' @param filename The filename in the current working folder to save the output.
#' @returns The input deck object, invisibly.
#' @export
save_pres <- function(doc, filename) {
  # Save the context.xml file to disk
  content_xml_filename <- paste0(Sys.getenv("temp_dir"), "/content.xml")
  xml2::write_xml(x = doc, file = content_xml_filename, options = "")

  # compress it. zip seems to need you to be in the same directory!
  # spent too long trying to do it wihtout changing wd...
  to_path <- paste0(getwd(), "/", filename)
  old_working_dir <- getwd()
  setwd(Sys.getenv("temp_dir"))
  utils::zip(zipfile = to_path, files = list.files(), flags = "-r9XFS")
  setwd(old_working_dir)

  # return input invisibly in case there's more piping to do
  invisible(doc)
} # end funciton save_pres()




#' Write styles in deck object. Styles must be list of list style items
#'
#' Update the deck object with user-defined styles.
#'
#' @param doc A deck object.
#' @param styles A list of styles created by a style function, e.g. new_paragraph_style() or
#'               odp::new_graphics_style().
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
#' Update the deck object to write user-defined slides. This should only be
#' run once in a final pipeline that ends with `save_pres()`.
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
#' Update deck object to write user-defined fonts. Does not include fonts in
#' the file; users must have fonts installed for this to work properly.
#'
#' Should be run once in a final pipeline terminating in `save_pres()`.
#'
#' @param doc A deck object.
#' @param fonts A list of font declarations from new_font().
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
#' Create a new paragraph style with sensible defaults.
#'
#' To use this style you need to do two things. First, make sure that all
#' styles are put into a list and saved as part of your final project pipeline
#' with the function `write_styles()`.
#'
#' Second, ensure that you use each style's name as defined in its `name` attribute
#' when you created it. This is similar to CSS, where a style is defined and named
#' and then referred back to at the point of use.
#'
#' Paragraph styles can be used in text_p() function calls.
#'
#' @param  name The style's name. This is used in text_box() to apply the style.
#' @param font_weight Character. c("regular", "bold").
#' @param font_style Character. c("regular", "italic").
#' @param font_size Character. Font size in pts. Default "12pt".
#' @param color Character. Text colour in hex format. Default "#000000".
#' @param text_align Character. c("start", "center", "end").
#' @param opacity Character. Opacity in percent. Default "100%".
#' @param font_name character. Default "Liberation Sans".
#' @param text_underline_style Character. c("none", "solid")).
#' @returns A paragraph style list item.
#' @export
new_paragraph_style <- function(
    name,
    font_weight = c("regular", "bold"),
    font_style = c("regular", "italic"),
    font_size = "12pt",
    color = "#000000",
    text_align = c("start", "center", "end"),
    opacity = "100%",
    font_name = "Liberation Sans",
    text_underline_style = c("none", "solid")) {
  font_weight <- match.arg(font_weight, font_weight)
  font_style <- match.arg(font_style, font_style)
  text_align <- match.arg(text_align, text_align)
  text_underline_style <- match.arg(text_underline_style, text_underline_style)

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
          `style:font-name` = font_name,
          `style:text-underline-style` = text_underline_style,
          `style:text-underline-width` = "auto",
          `style:text-underline-color` = "font-color"
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
} # end function new_paragraph_style

new_text_style <- function(
    name,
    font_weight = c("regular", "bold"),
    font_style = c("regular", "italic"),
    font_size = "12pt",
    color = "#000000",
    opacity = "100%",
    font_name = "Liberation Sans",
    text_underline_style = c("none", "solid")) {
  font_weight <- match.arg(font_weight, font_weight)
  font_style <- match.arg(font_style, font_style)
  text_underline_style <- match.arg(text_underline_style, text_underline_style)

  style_list <- list(
    `type` = "style:style",
    `attributes` = c(
      `style:name` = name,
      `style:family` = "text"
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
          `style:font-name` = font_name,
          `style:text-underline-style` = text_underline_style,
          `style:text-underline-width` = "auto",
          `style:text-underline-color` = "font-color"
        )
      )
    )
  )

  style_list
} # end function new_text_style

maybe_add_to_vector <- function(vector, name, value_or_na) {
  if (!is.na(value_or_na)) {
    vector[name] <- value_or_na
  }
  vector
}


new_text_style_minimal <- function(
    name,
    font_weight = NA,
    font_style = NA,
    font_size = NA,
    color = NA,
    opacity = NA,
    font_name = NA,
    text_underline_style = NA,
    text_underline_width  = NA,
    text_underline_color = NA) {
  attributes <- c() |>
    maybe_add_to_vector("fo:font-weight", font_weight) |>
    maybe_add_to_vector("fo:font-style", font_style) |>
    maybe_add_to_vector("fo:font-size", font_size) |>
    maybe_add_to_vector("fo:color", color) |>
    maybe_add_to_vector("loext:opacity", opacity) |>
    maybe_add_to_vector("style:font-name", font_name) |>
    maybe_add_to_vector("style:text-underline-style", text_underline_style) |>
    maybe_add_to_vector("style:text-underline-width", text_underline_width) |>
    maybe_add_to_vector("style:text-underline-color", text_underline_color)
    

  style_list <- list(
    `type` = "style:style",
    `attributes` = c(
      `style:name` = name,
      `style:family` = "text"
    ),
    children = list(
      list(
        `type` = "style:text-properties",
        `attributes` = attributes
      )
    )
  )

  style_list
} # end function new_text_style




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
new_font <- function(
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
#' Update a slide list item to add a new item to it. In practice this will
#' likely be text_box() or new_custom_shape() objects.
#'
#' @param slide A slide to add the item to.
#' @param item An item to add to the slide. E.g. output of text_box(), new_custom_shape().
#' @returns The input slide with the new item added to it.
#' @export
add_to_slide <- function(slide, item) {
  slide$children <- append(slide$children, list(item))

  slide
}


#' Create new custom shape.
#'
#' Create a new shape object in list item format. Styles must be defined with
#' new_graphic_style().
#'
#' @param type  Character. c("rectangle", "ellipse")
#' @param width Character. Width in cm. e.g. "10cm"
#' @param height Character. Height in cm. e.g. "10cm"
#' @param x Character. Leftmost position in cm. e.g. "10cm"
#' @param y Character. Topmost position in cm. e.g. "10cm"
#' @param draw_style_name Character. The draw style to apply. Default "gr1".
#' @param text_style_name Character. The text style to apply. Default "P1".
#' @param text Character. Text to include in shape. Default "".
#' @param alt_text Character. Alt text for image. Important for accessibility.
#' @param rect_radius Numeric. Value between 0 and 10800, higher numbers give rounder rounded rectangles.
#' @returns A custom shape list object.
#' @export
new_custom_shape <- function(
    type = c("rectangle", "ellipse", "round-rectangle"),
    width,
    height,
    x,
    y,
    draw_style_name = "gr1",
    text_style_name = "P1",
    text = "",
    alt_text = "",
    rect_radius = 0) {
  type <- match.arg(type, type)

  alt_text <- list(
    type = "svg:desc",
    attributes = c(),
    children = alt_text
  )

  # if text is character, make a text_p(text_span()) for it. otherwise pass through
  text_child <- if (all(is.character(text))) {
    list(text_p(text_span(text, style_name = text_style_name), text_style_name = text_style_name))
  } else if (is.list(text)) {
    # received a list: if it's one item with proprty `type` then wrap it in a list, otherwise pass the list unchanged
    if (!is.null(text$type)) {
      list(text)
    } else {
      text
    }
  }

  list(
    `type` = "draw:custom-shape",
    attributes = c(
      `draw:style-name` = draw_style_name,
      `draw:layer` = "layout",
      `svg:width` = width,
      `svg:height` = height,
      `svg:x` = x,
      `svg:y` = y
    ),
    children = append(
      text_child,
      list(draw_enhanced_geometry(type, rect_radius), alt_text)
    )
  )
}

#  Internal function. Create enhanced geometry item. Not used by user
# @param type Character. c("ellipse", "rectangle", "rect_radius").
# @param rect_radius Numeric. Value between 0 and 10800, higher numbers give rounder rounded rectangles.
# @returns A list defining a basic geometry shape.
draw_enhanced_geometry <- function(type = c("ellipse", "rectangle", "round-rectangle"), rect_radius) {
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
  }} else if (type == "round-rectangle") {
    list(
      `type` = "draw:enhanced-geometry",
      attributes = c(
        `svg:viewBox` = "0 0 21600 21600",
        `draw:path-stretchpoint-x` = "10800",
        `draw:path-stretchpoint-y` = "10800",
        `draw:text-areas` = "?f3 ?f4 ?f5 ?f6",
        `draw:type` = "round-rectangle",
        `draw:modifiers` = rect_radius,
        `draw:enhanced-path` = "M ?f7 0 X 0 ?f8 L 0 ?f9 Y ?f7 21600 L ?f10 21600 X 21600 ?f9 L 21600 ?f8 Y ?f10 0 Z N"
      ),
      children = list(
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f0",
            `draw:formula` = "85" # rect_radius
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f1",
            `draw:formula` = "$0 *sin(?f0 *(pi/180))"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f2",
            `draw:formula` = "?f1 *3163/7636"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f3",
            `draw:formula` = "left+?f2"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f4",
            `draw:formula` = "top+?f2"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f5",
            `draw:formula` = "right-?f2"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f6",
            `draw:formula` = "bottom-?f2"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f7",
            `draw:formula` = "left+$0"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f8",
            `draw:formula` = "top+$0"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f9",
            `draw:formula` = "bottom-$0"
          )
        ),
        list(
          `type` = "draw:equation",
          attributes = c(
            `draw:name` = "f10",
            `draw:formula` = "right-$0"
          )
        )
      )
    )
  }
} # end function draw_enhanced_geometry()



# <style:style style:name="gr1" style:family="graphic" style:parent-style-name="standard">
# <style:graphic-properties svg:stroke-color="#ff0000" svg:stroke-opacity="100%"
# draw:fill="solid" draw:fill-color="#2a6099" draw:opacity="100%"
#  draw:textarea-horizontal-align="justify" draw:textarea-vertical-align="middle" draw:auto-grow-height="false"
#  fo:min-height="4.25cm" fo:min-width="4cm" loext:decorative="false"/>
# <style:paragraph-properties style:writing-mode="lr-tb"/>
# </style:style>

#' Define a new graphics style.
#'
#' Create a new graphics style with sensible defaults.
#'
#' To use this style you need to do two things. First, make sure that all
#' styles are put into a list and saved as part of your final project pipeline
#' with the function `write_styles()`.
#'
#' Second, ensure that you use each style's name as defined in its `name` attribute
#' when you created it. This is similar to CSS, where a style is defined and named
#' and then referred back to at the point of use.
#'
#' Graphics styles can be used in new_custom_shape() function calls.
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
new_graphic_style <- function(
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
} # end function new_graphic_style()


#' A field for the current page/slide number.
#'
#' This field item should be used inside a text box.
#'
#' @returns A field showing the current page number.
#' @export
field_page_num <- function() {
  # list(
  list(
    `type` = "text:page-number",
    children = "&lt;number&gt;"
  )
  # )
} # end function field_page_num()




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




#' Create a list
#'
#' Must be embedded in a text_box().
#'
#' Contents must be provided in ONE of two ways. First, they can be provided
#' as comma-separated arguments which will be processed by R's "..." argument.
#' Arguments provided this way should be results of calls to text_p().
#'
#' OR, arguments can be provided in a list of text_p() objects provided to
#' the function parameter `contents_list`. This is useful if e.g. you want to
#' prepare the contents procedurally and then add then to a paragraph later.
#'
#' If `contents_list` is provided, it overrides anything provided to `...`.
#'
#' @param ... Comma-separated text_p()  or new_list() items. The text to display.
#' @param contents_list List. A list of text_p() or new_list() items prepared previously.
#'        Overrides any values provided to `...`.
#' @param list_style_name Character. Name of list style.
#' @param text_style_name Character. Name of new_paragraph_style() output style.
#' @export
new_list <- function(..., contents_list = NA, list_style_name = "L1", text_style_name = NA) {
  contents_list_supplied <- !all(is.na(contents_list))
  contents_list_is_list <- is.list(contents_list)
  contents_list_contains_lists <- lapply(X = contents_list, FUN = is.list) |>
    unlist() |>
    all()

  contents <- if (contents_list_supplied) {
    # contents_list must be a list
    if (!contents_list_is_list || !contents_list_contains_lists) {
      stop("contents_list argument must contain a list() of text_p() objects.)")
    }

    # list of list objects supplied, pass it on
    contents_list
  } else {
    # otherwise wrap supplied objects in a list
    # TODO would be good to validate this as well
    list(...)
  }

  if (length(contents) == 0) {
    stop("Must provide at least one child. Either a list of text_p() objects to contents_list, or one or more text_p() objects passed as regualr arguments.") # nolint
  }

  if (!contents[[1]]$type %in% c("text:p")) {
    stop("input must be list of text:p objects.")
  }


  thelist <- list(
    `type` = "text:list",
    attributes = c(
      `text:style-name` = list_style_name
    ),
    `children` = lapply(X = contents, FUN = \(x) handle_list_contents(x, text_style_name))
  )

  thelist
} # end funciton new_list()

handle_list_contents <- function(list_contents, text_style_name) {
  child <- list(
    `type` = "text:list-item",
    children = list(list_contents)
  )
  if (!is.na(text_style_name)) {
    child$attributes <- c(`text:style-name` = text_style_name)
  }

  child
} # end function handle_list_contents()
