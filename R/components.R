#' Color palettes from components.ai, ramped to a specified length
#'
#' @inheritParams components_pal
#' @param n Numeric. Number of colors to be displayed.
#'
#' @export
components <- function(palette = "lab",
                       level = 8,
                       n,
                       alpha = 1,
                       reverse = FALSE) {

  stopifnot(is.numeric(level))

  pal <- components_palettes[[palette]]

  if (is.null(pal)) stop("Palette not found.")

  if (level > nrow(pal)) stop("This palette only has ", nrow(pal), " levels.")

  pal <- pal[level, ]

  if (missing(n)) n <- length(pal)

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, alpha)(n)

}

#' Color palettes from components.ai
#'
#' @param palette Character. A palette to display; one of "bootstrap", "lab"
#' (the default), "material", "open_color", "palx", or "tachyons".
#' @param level Numeric. The "level" of the palette to be displayed.
#' @param alpha Numeric. Transparency.
#' @param reverse Logical. Should the order of colors be reversed?
#'
#' @export
components_pal <- function(palette = "lab",
                           level = 8,
                           alpha = 1,
                           reverse = FALSE) {

  function(n) {
    components(palette, level, n, alpha, reverse)
  }

}

#' components.ai color scales for ggplot2
#'
#' @inheritParams components
#' @param ... Arguments passed to either [ggplot2::discrete_scale] or
#' [ggplot2::scale_color_gradientn], as appropriate.
#'
#'
#' @rdname scale_color_components
#' @export
scale_color_components <- function(palette = "lab",
                                   level = 8,
                                   discrete = TRUE,
                                   alpha = 1,
                                   reverse = FALSE,
                                   ...) {

  if (discrete) {
    ggplot2::discrete_scale(
      "colour",
      "components",
      components_pal(palette,
                     level = level,
                     alpha = alpha,
                     reverse = reverse),
      ...)
  }
  else {
    ggplot2::scale_color_gradientn(
      colours = components(palette,
                           level = level,
                           256,
                           alpha = alpha,
                           reverse = reverse),
      ...)
  }
}

#' @rdname scale_color_components
#' @export
scale_colour_components <- scale_color_components

#' components.ai fill scales for ggplot2
#'
#' @inheritParams components
#' @param ... Arguments passed to either [ggplot2::discrete_scale] or
#' [ggplot2::scale_fill_gradientn], as appropriate.
#'
#' @export
scale_fill_components <- function(palette = "lab",
                                  level = 8,
                                  discrete = TRUE,
                                  alpha = 1,
                                  reverse = FALSE,
                                  ...) {

  if (discrete) {
    ggplot2::discrete_scale(
      "fill",
      "components",
      components_pal(palette,
                     level = level,
                     alpha = alpha,
                     reverse = reverse),
      ...)
  }
  else {
    ggplot2::scale_fill_gradientn(
      colours = components(palette,
                           level = 5,
                           256,
                           alpha = alpha,
                           reverse = reverse),
      ...)
  }
}


#' Display a color palette
#'
#' Given a character vector (hex RGB values), display palette in graphics window.
#'
#' @param palette vector of character hex RGB values
#'
#' @export
components_show_palette <- function(palette, level) {

  name <- paste0(palette, ": Level ", level)

  palette <- components(palette, level)

  n <- length(palette)

  if (length(palette > 0)) {

    graphics::image(1:n, 1, as.matrix(1:n), col = palette,
                    xlab = "", ylab = "", xaxt = "n", yaxt = "n",
                    bty = "n")
    graphics::title(main = name)

  }
}
