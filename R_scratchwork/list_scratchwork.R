# scratchwork for unordered (maybe ordered?) lists
devtools::load_all()
text_style_name <- "L1"

new_list("butts")
new_list(list("sbig","ugly","butts",
list("that","suck!")))

text_box("sadf", width="1cm",height="1cm",x="1cm",y="1cm")


test_text_list <- text_box(text=list("tem 1", "items 2!!!"), width="10cm",height="10cm",x="1cm",y="1cm")

test_text_normal <- text_box(text="normal text", width="10cm",height="10cm",x="1cm",y="1cm")
deck <- odp::new_pres()

slide1 <- new_slide() |>
add_to_slide(test_text_normal)

slides <- list(slide1)

slides |>
write_slides() |>
save_pres(filename = "testlist.odp")
