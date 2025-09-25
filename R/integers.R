# Helpers / Aux -----------------------------------------------------------

#' Get number of digits of a number
#' log10 of a number gives us the approximate number of digits - 1. By using the floor operation we cancel the decimal noise
#'
#' @param x A positive integer.
#' @param base The base to use for the logarithm.
#'
#' @return The number of digits in x.
#' @export
#'
#' @examples
#' get_digits(123)
get_digits <- function(x, base = 10) {
  assertthat::assert_that(min(x) >= 0, msg = "x should be positive!")
  ndigits <- floor(log(x, base = base) + 1)
  return(ndigits)
}

# By taking 10 to the power of the number of digits of a number and dividing
# by 10 we get the geometric sequence 1, 10 , 100 , 1000 etc


#' Get powers of ten
#'
#' @param ndigits The number of digits.
#' @param include_one A logical indicating whether to include 1 in the sequence.
#' @param full_seq A logical indicating whether to return the full sequence or just the upper bound.
#'
#' @return A vector of powers of ten.
#' @export
#'
#' @examples
#' get_powers_of_ten(3)
get_powers_of_ten <- function(ndigits,
                              include_one = FALSE,
                              full_seq = TRUE) {
  assertthat::assert_that(min(ndigits) > 0, msg = "ndigits should be positive!")
  if (full_seq) {
    # Count digits down to 0 to get the geometric sequence 1,10,100 etc
    ndigits <- rev(seq(from = ndigits, to = if_else(include_one, 0, 1)))
    pwr_ten <- 10**ndigits
  } else {
    # Only take upper bound
    pwr_ten <- 10**max(ndigits)
  }
  return(pwr_ten)
}

# Reverses the deconstruction of integers in digits.https://mathworld.wolfram.com/Concatenation.html

#' Concatenate numbers
#' Reverses the deconstruction of integers in digits.https://mathworld.wolfram.com/Concatenation.html
#' @param x The first number.
#' @param y The second number.
#' @param base The base to use for the concatenation.
#'
#' @return The concatenated number.
#' @export
#'
#' @examples
#' concatenate_math(12, 34)
concatenate_math <- function(x, y, base = 10) {
  assertthat::assert_that(y != 0,
    msg = "Use of y = 0 is not recommended, because the number of digits of 0 is -Inf, which forces x*base**(get_digits(y)) to zero, thus setting x to zero!!!"
  )
  concat <- x * base**(get_digits(y, base = base)) + y
  return(concat)
}

#' Transform to base ten
#'
#' @param x The number to transform.
#' @param base The base of the number.
#'
#' @return The number in base ten.
#' @export
#'
#' @examples
#' transform_to_base_ten(1000, 2)
transform_to_base_ten <- function(x, base) {
  digits_seq <- seq(1:get_digits(x, base = 10)) - 1
  transformed <- sum(extract_digits(x) * base**digits_seq)
  return(transformed)
}

# Main functions ------------------------------------------------------

# Combination of the function above plus one more step. We first get the number
# of digits the integer x followed by its exponent. Then we reapply the previous
# function but with another base, which is the integer itself

# https://stackoverflow.com/questions/19764244/how-can-we-split-an-integer-number-into-a-vector-of-its-constituent-digits-in-r


#' Extract digits from a vector of numbers
#'
#' @param x_vec A vector of numbers.
#'
#' @return A vector of digits.
#' @export
#'
#' @examples
#' extract_digits(c(123, 456))
extract_digits <- function(x_vec) {
  # Container
  digit_vec_out <- c()
  for (x in x_vec) {
    # Get power of 10 series
    pwr_ten <- get_powers_of_ten(ndigits = get_digits(x))

    # Modulo of input number for power of ten series (without 1 as mod 1 is always 0)
    modulo_vec <- x %% pwr_ten

    # Divide Modulo by Geometric sequence (now including 1, to get down to single digit)
    digit_vec <- modulo_vec %/% (pwr_ten %/% 10)

    digit_vec_out <- c(digit_vec_out, digit_vec)
  }
  return(digit_vec_out)
}

#' Extract digits from a vector of numbers and return a matrix
#'
#' @param x A vector of numbers.
#'
#' @return A matrix of digits.
#' @export
#'
#' @examples
#' extract_digits_matrix(c(123, 456))
extract_digits_matrix <- function(x) {
  pwr_ten <- get_powers_of_ten(ndigits = get_digits(max(x)))
  # Create matrix of repeated power of ten series with outer-product
  M_upper <- pwr_ten %o% rep(1, length(x)) # upper bound to 10
  M_lower <- pwr_ten %/% 10 %o% rep(1, length(x)) # upper bound - 1 to 1
  A <- t(x %o% rep(1, length(pwr_ten))) # Matrix of input repeated (same dim as above)

  # Create bool mask to rm superflous zeros introduced by redundancy
  # Everything that is TRUE is redundant, e.g. for an entry 10, we need to calculate
  # 10mod10 = 1 >> 1\1 = 0 ~ digit 2
  # 10mod100 = 10 >> 10\10 = 1 ~ digit 1
  # 10mod1000 = 10 >> 10\100 = 0 redundant by matrix design
  # The redundancy arises when multiple entries with differing digits are passed
  # to this function, since the matrices have to be padded with the power ten series
  mask_false_zeros <- A < M_lower

  # Basically these are Hadamard operations (element-wise)
  res <- A %% M_upper %/% M_lower

  # Rm superfluous zeros by NAS
  res[mask_false_zeros] <- NA

  # Clear redundant 0
  out <- res[!is.na(res)]
  return(out)
}

#' Extract the last digit of each number in a vector
#'
#' @param x A vector of numbers.
#'
#' @return A vector of the last digits.
#' @export
#'
#' @examples
#' extract_last_digits(c(123, 456))
extract_last_digits <- function(x) {
  last_digit <- x %% 10
  return(last_digit)
}

#' Check if a number is divisible by its digits
#' https://www.youtube.com/watch?v=UDQjn_-pDSs
#' @param x A number.
#'
#' @return A logical indicating whether the number is divisible by its digits.
#' @export
#'
#' @examples
#' is_divisible_by_digit(12)
is_divisible_by_digit <- function(x) {
  # TODO: Not yet implemented
  return(NULL)
}

#' Title
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
plot_all_digits <- function(vec) {
  count_digits <- extract_digits_matrix(vec) %>%
    table() %>%
    as_tibble()

  g_count_digits <- count_digits %>%
    ggplot() +
    geom_bar(aes(x = ., y = n, fill = .),
      stat = "identity"
    ) +
    labs(
      title = "Distribution of the digits",
      x = "digits"
    ) +
    theme_minimal()
  print(g_count_digits)
}


#' Title
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
plot_last_digits <- function(vec) {
  count_digits <- extract_last_digits(vec) %>%
    table() %>%
    as_tibble()

  g_count_digits <- count_digits %>%
    ggplot() +
    geom_bar(aes(x = ., y = n, fill = .),
      stat = "identity"
    ) +
    labs(
      title = "Distribution of the last digits",
      x = "digits"
    ) +
    theme_minimal()
  print(g_count_digits)
}
