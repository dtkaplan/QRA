#' Calculate the heat index based on temperature and humidity
#'
#' Implements a polynomial approximation to the Steadman table, per Rothfusz.
#' Out of bounds inputs still generate a result, although it is not meaningful
#' owing to the nature of polynomials explosive growth
#'
#' @param temerature in deg C
#' @param humidity relative humidity in percent, but as a decimal, e.g. 0.50
#'
#' @returns Heat index in degrees C
#'
#' @export
heat_index <- function(temperature, humidity) {
  # Convert Celsius to Fahrenheit
  t_f <- (temperature * 9/5) + 32
  rh <- 100 * humidity

  # Simple average formula used for lower temperatures
  hi_simple <- 0.5 * (t_f + 61.0 + ((t_f - 68.0) * 1.2) + (rh * 0.094))

  # Check if simple formula applies (average of T and HI is < 80°F)
  if ((hi_simple + t_f) / 2 < 80) {
    hi_f <- hi_simple
  } else {
    # Full Rothfusz multi-regression polynomial
    hi_f <- -42.379 +
      2.04901523 * t_f +
      10.14333127 * rh -
      0.22475541 * t_f * rh -
      0.00683783 * (t_f^2) -
      0.05481717 * (rh^2) +
      0.00122874 * (t_f^2) * rh +
      0.00085282 * t_f * (rh^2) -
      0.00000199 * (t_f^2) * (rh^2)

    # Adjustment for low humidity and high temperature
    if (rh < 13 && t_f >= 80 && t_f <= 112) {
      adj <- ((13 - rh) / 4) * sqrt((17 - abs(t_f - 95)) / 17)
      hi_f <- hi_f - adj
    }
    # Adjustment for high humidity and high temperature
    else if (rh > 85 && t_f >= 80 && t_f <= 87) {
      adj <- ((rh - 85) / 10) * ((87 - t_f) / 5)
      hi_f <- hi_f + adj
    }
  }

  # Convert result back to Celsius
  hi_c <- (hi_f - 32) * 5/9
  return(hi_c)
}

# Inputs: T = Temperature in Fahrenheit, RH = Relative Humidity (0-100)
calculate_rothfusz_hi <- function(T, RH) {
  # Step 1: Check simple formula first for mild conditions
  HI_simple <- 0.5 * (T + 61.0 + ((T - 68.0) * 1.2) + (RH * 0.094))

  # If the simple average is below 80 deg F, use it directly
  if (((HI_simple + T) / 2) < 80) {
    return(HI_simple)
  }

  # Step 2: Apply the full 9-term Rothfusz Regression
  c1 <- -42.379
  c2 <- 2.04901523
  c3 <- 10.14333127
  c4 <- -0.22475541
  c5 <- -0.00683783
  c6 <- -0.05481717
  c7 <- 0.00122874
  c8 <- 0.00085282
  c9 <- -0.00000199

  HI <- c1 + (c2 * T) + (c3 * RH) + (c4 * T * RH) + (c5 * T^2) +
    (c6 * RH^2) + (c7 * T^2 * RH) + (c8 * T * RH^2) + (c9 * T^2 * RH^2)

  # Step 3: Conditional Adjustments
  # Adjustment for dry, hot conditions
  if (RH < 13 && T >= 80 && T <= 112) {
    adj <- ((13 - RH) / 4) * sqrt((17 - abs(T - 95)) / 17)
    HI <- HI - adj
  }
  # Adjustment for high humidity, hot conditions
  else if (RH > 85 && T >= 80 && T <= 87) {
    adj <- ((RH - 85) / 10) * ((87 - T) / 5)
    HI <- HI + adj
  }

  return(HI)
}

# Example usage (Fahrenheit):
# calculate_rothfusz_hi(T = 95, RH = 50)

