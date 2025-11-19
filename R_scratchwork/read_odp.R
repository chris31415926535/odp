# scratchwork testing loading xml file and converting it to odp package function calls (templating?)
#
xml <- xml2::read_xml("temp/content.xml")
xml2::xml_child(xml, "office:body") |>
  xml2::xml_child("office:presentation") |>
  xml2::xml_children() |>
  xml2::xml_find_all("page")


pages <- xml2::xml_find_all(xml, "//draw:page")
page_num <- 1
page <- pages[[page_num]]
page

child_num <- 1

child <- xml2::xml_child(page, child_num)

listify_child(child)

listify_child <- function(child) {
  type <- xml2::xml_name(child)
  attrs <- xml2::xml_attrs(child)

  message("type: ", type)
  message("attrs: ", attrs)

  text <- xml2::xml_text(child)
  draw_style_name <- attrs["style-name"]
  text_style_name <- attrs["text-style-name"]
  layer <- attrs["layer"]
  width <- attrs["width"]
  height <- attrs["heigh"]
  x <- attrs["x"]
  y <- attrs["y"]

  if (type == "custom-shape") {
    shape_xml <- xml2::xml_child(child, "draw:enhanced-geometry")
    shape_attrs <- xml2::xml_attrs(shape_xml)
    shape_type <- shape_attrs["type"]
    rect_radius <- shape_attrs["modifiers"] |> as.numeric()

    odp::new_custom_shape(
      type = shape_type,
      width = width,
      height = height,
      layer = layer,
      x = x,
      y = y,
      draw_style_name = draw_style_name,
      text_style_name = text_style_name,
      text = text,
      rect_radius = rect_radius
    )
  } else if (type == "frame") {
    textbox_xml <- xml2::xml_child(child, "draw:text-box")
    textbox_attrs <- xml2::xml_attrs(textbox_xml)
    textbox_type <- textbox_attrs["type"]
    odp::text_box(
      text = text,
      width = width,
      height = height,
      x = x,
      y = y,
      draw_layer = layer,
      draw_style_name = draw_style_name,
      draw_text_style_name = text_style_name
    )
  } else {
    message("unknown type: ", type)
  }
} # end function listify_child()
