.onLoad <- function(...) {
    if (systemfonts::match_fonts("Fira Sans Condensed") |> nrow()) {
        systemfonts::register_variant(
            name = "Fira Sans Condensed Semibold",
            family = "Fira Sans Condensed",
            weight = c("normal", "semibold")
        )
        systemfonts::register_variant(
            name = "Fira Sans Condensed Medium",
            family = "Fira Sans Condensed",
            weight = c("normal", "medium")
        )
    }
}
