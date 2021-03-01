.onLoad <- function(...) {
    systemfonts::register_variant(
        name = "Fira Sans Condensed Semibold",
        family = "Fira Sans Condensed",
        weight = c("nomarl", "semibold")
    )
    systemfonts::register_variant(
        name = "Fira Sans Condensed Medium",
        family = "Fira Sans Condensed",
        weight = c("nomarl", "medium")
    )
}
