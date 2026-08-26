#' Utilities for typesetting dimensions

# The {{< var dim.power >}} style doesn't work in PDF. Here's an alternate

#' @name dimensions

mydimensions <- c(
  "dim" = "*dimension*",
  "length" = "L",
  "area" = "L^2^",
  "volume" = "L^3^",
  "time" = "T",
  "mass" = "M",
  "money" = "W",
  "temperature" = "$\\Theta$",
  "capita" = "C",
  "worth" = "W",
  "number" = "[1]",
  "lumen" = "Q",
  "velocity" = "L T^-1^",
  "acceleration" = "L T^-2^",
  "force" = "M L T^-2^",
  "energy" = "M L^2^ T^-2^",
  "power" = "M L^2^ T^-3^",
  "current" = "I",
  "mole" = "N",
  "voltage" = "M L^2^ T^-3^ I^-1^"
)

#' @param what Typesets a dimension using exponents on the base dimensions.
#' @rdname dimensions
#' @export
mydim <- function(what) {
  what <- deparse(substitute(what))
  if (what %in% names(mydimensions)) return(as.character(mydimensions[what]))
  else stop("Invalid name for dimension shortcode.")
}

#' @rdname dimensions
#' @export
available_dimensions <- function() {
  paste(names(mydimensions), " = ", mydimensions, collapse = "\n") |> cat()
}
