#' definitions of core modeling functions

#' @param x Generic argument to function

#' @rdname core_functions
#' @export
osc <- function(x) sin(2*pi*x)
#' @rdname core_functions
#' @export
hillside <- function(t) pnorm(t)
#' @rdname core_functions
#' @export
hill <- function(x) dnorm(x)
#' @rdname core_functions
#' @export
flat <- unity <- function(x) 1.0
#' @rdname core_functions
#' @export
steady <- id <- function(x) x
#' @rdname core_functions
#' @export
same<- id <- function(x) x
#' @rdname core_functions
#' @export
exp2 <- double <- function(x) 2^x
#' @rdname core_functions
#' @export
exp10 <- tenfolds <- function(x) 10^x
#' @rdname core_functions
#' @export
recip <- function(x) 1/x
#' @rdname core_functions
#' @export
magnitude <- tennings <- function(x) log10(x)
