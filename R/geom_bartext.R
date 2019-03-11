#' @format NULL
#' @usage NULL
#' @importFrom rlang %||%
#' @import ggplot2
#' @export
GeomBarText <- ggproto("GeomBarText", Geom,
                    required_aes = c("x", "label"),

                    non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

                    default_aes = aes(
                        lwd = 0.5, fill = "grey35", colour = NA, angle = 0,
                        fontsize = 3, fontface = 1, alpha = 1, textcolour = "white"
                    ),

                    setup_data = function(data, params) {
                        data$width <- data$width %||%
                            params$width %||% (resolution(data$x, FALSE) * 0.9)

                        data <- data %>%
                            mutate(y = 1)

                        transform(data,
                                  ymin = pmin(y, 0), ymax = pmax(y, 0),
                                  xmin = x - width / 2, xmax = x + width / 2, width = NULL
                        )
                    },

                    draw_panel = function(self, data, panel_params, coord, width = NULL,
                                          parse = FALSE, na.rm = FALSE) {

                        # Group the data
                        grouped_data <- data %>%
                            group_by(x) %>%
                            summarise(xmax = max(xmax), xmin = min(xmin),
                                      ymin = min(ymin), ymax = max(ymax)) %>%
                            left_join(select(data, -xmin, -xmax, -ymin, -ymax))

                        # Hack to ensure that width is detected as a parameter
                        rectgrb <- ggproto_parent(GeomRect, self)$draw_panel(grouped_data, panel_params, coord)

                        lab <- data$label
                        if (parse) {
                            lab <- parse_safe(as.character(lab))
                        }

                        data <- coord$transform(data, panel_params)

                        ## Produce the data containing text positions
                        label_text <- data %>%
                            arrange_at(vars(label)) %>%
                            mutate(x = factor(x)) %>%
                            mutate(ypos = (ymax + ymin) / 2,
                                   xpos = (xmax + xmin) / 2,
                                   size = data$fontsize)

                        txtgrb <- textGrob(
                            label_text$label,
                            label_text$xpos, label_text$ypos, default.units = "native",
                            hjust = data$hjust, vjust = data$vjust,
                            rot = data$angle,
                            gp = gpar(
                                col = alpha(data$textcolour, data$alpha),
                                fontsize = label_text$size * .pt,
                                fontfamily = data$family,
                                fontface = data$fontface,
                                lineheight = data$lineheight
                            )
                        )

                        grobTree(rectgrb, txtgrb)
                    },

                    draw_key = draw_key_text
)

#' Labeled Bar Charts
#'
#' This function adds a geom_bartext geom which essentially composes geom_bar() and geom_text()
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @import grid
#'
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' mpg_manu <- mpg %>%
#'     distinct(manufacturer, model)
#'
#' ggplot(mpg_manu, aes(x = manufacturer)) +
#'     geom_bartext(aes(label = model))
#'
#' ggplot(mpg_manu, aes(x = manufacturer)) +
#'     geom_bartext(aes(label = model), colour = "red", textcolour = "black", fill = "white")
geom_bartext <- function(mapping = NULL, data = NULL,
                     position = "stack",
                     ...,
                     width = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE,
                     parse = FALSE) {

    layer(
        data = data,
        mapping = mapping,
        stat = "identity",
        geom = GeomBarText,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            width = width,
            parse = parse,
            na.rm = na.rm,
            ...
        )
    )
}
