#' List of primary and secondary WCM palettes
#'
#' Use \code{\link{wcm_palette}} to construct palettes of desired length.
#'
#' @export
wcm_palettes <- list(
  primary = c("#B31B1B", "#CF4520", "#E87722", "#FFC72C"),
  secondary = c("#000000", "#555555", "#777777", "#dddddd", "#f7f7f7", "#FFFFFF")
)

#' WCM palette generator
#'
#' official primary and secondary color palettes of WCM.
#'
#' @param name Name of palette. Choices are:
#'   \code{primary} (default), \code{secondary}
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param type Either "continuous" or "discrete". Continuous automatically interpolates between colors.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colors.
#' @export
#' @keywords colors
#' @examples
#' wcm_palette("primary")
#' wcm_palette("primary", n = 10, type = "continuous")
wcm_palette <- function(palette_name, n, type = c("discrete", "continuous")) {
  if (missing(palette_name)) palette_name <- "primary"

  type <- match.arg(type)
  pal <- wcm_palettes[[palette_name]]
  len <- length(pal)

  if (is.null(pal)) stop("Palette not found.")

  if (missing(n)) len -> n

  if (type == "discrete" && n > len) stop("Number of requested colors greater than what palette can offer")

  to_return <- switch(type,
                      continuous = grDevices::colorRampPalette(pal)(n),
                      discrete = pal[1:n]
  )

  wesanderson:::print.palette(structure(to_return, class = "palette", name = palette_name))
}


