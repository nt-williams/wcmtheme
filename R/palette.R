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
#' Primary and secondary color palettes of Weill Cornell Medicine.
#'
#' @param palette_name Name of palette. Derived from \href{https://brand.weill.cornell.edu/brand-guidelines/color-palette}{WCM branding}.
#'   Choices are: \code{primary} (default), \code{secondary}
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param type Either "continuous" or "discrete". Continuous automatically interpolates between colors.
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
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

  structure(to_return, class = "palette", name = palette_name)
}

#' @export
#' @importFrom graphics rect par image text
#' @inheritParams wesanderson::print.palette
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = grDevices::rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
