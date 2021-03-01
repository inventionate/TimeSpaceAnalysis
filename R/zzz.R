.onLoad <- function(...) {
    fs_condensed <-
        systemfonts::system_fonts() %>%
        filter(str_detect(name, "FiraSansCondensed"))

    fs_condensed_plain <-
        fs_condensed %>%
        filter(str_detect(style, "Regular"))

    fs_condensed_italic <-
        fs_condensed %>%
        filter(str_detect(style, "^Italic*"))

    fs_condensed_bold <-
        fs_condensed %>%
        filter(str_detect(style, "SemiBold"))

    fs_condensed_bolditalic <-
        fs_condensed %>%
        filter(str_detect(style, "Semibold Italic"))

    systemfonts::register_font(
        'Fira Sans Condensed Semibold',
        plain = list(fs_condensed_plain$path, fs_condensed_plain$index),
        bold = list(fs_condensed_bold$path, fs_condensed_bold$index),
        italic = list(fs_condensed_italic$path, fs_condensed_italic$index),
        bolditalic = list(fs_condensed_bolditalic$path, fs_condensed_bolditalic$index)
    )
}
