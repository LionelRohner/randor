
# Construct derivative matrix ---------------------------------------------

#' Construct a derivative matrix
#'
#' @param order The order of the polynomial.
#'
#' @returns A derivative matrix.
#' @export
#'
#' @examples
#' construct_derivate_matrix(4)
construct_derivate_matrix <- function(order) {
  # construct entries of A
  x <- rep(1:order, order)

  # construct A
  A <- matrix(x, ncol = order)

  # B = A with diag = {0}
  B <- A
  diag(B) <- 0

  # add zero vector
  C <- A - B
  C <- cbind(rep(0, order), C)

  # return diagonal matrix with increasing
  return(C)
}

# Process polynomial ------------------------------------------------------

#' Prepare a polynomial for matrix operations
#'
#' @param polynomial A character string representing the polynomial.
#' @param order The order of the polynomial.
#'
#' @returns A numeric vector representing the polynomial.
#' @export
#'
#' @examples
#' prep_polynomial("1+x+x^2+x^3+x^4", 4)
prep_polynomial <- function(polynomial, order) {
  # Initialize coefficient vector of size order + 1
  polyVec <- numeric(order + 1)
  names(polyVec) <- 0:order

  # Pre-process string:
  # 1. remove all whitespace
  polynomial <- gsub("\\s+", "", polynomial)
  # 2. Replace '-' with '+-' for easier splitting
  polynomial <- gsub("-", "+-", polynomial)
  # 3. If it starts with "+-", the first term was negative. Remove leading "+".
  if (startsWith(polynomial, "+-")) {
    polynomial <- substring(polynomial, 2)
  }
  # 4. Add implicit 1 coefficients to 'x' terms, e.g. x -> 1x, +x -> +1x, -x -> -1x
  polynomial <- gsub("(?<=[+-]|^)x", "1x", polynomial, perl = TRUE)

  # Split polynomial into terms
  terms <- unlist(strsplit(polynomial, "\\+"))
  terms <- terms[terms != ""] # Remove empty strings from splitting

  for (term in terms) {
    # Case 1: Constant term (no 'x')
    if (!grepl("x", term)) {
      polyVec["0"] <- polyVec["0"] + as.numeric(term)
      next
    }

    # Case 2: Term with 'x'
    # Default exponent is 1 if not specified
    if (!grepl("\\^", term)) {
      term <- paste0(term, "^1")
    }

    parts <- strsplit(term, "x\\^")[[1]]
    coef <- as.numeric(parts[1])
    exp <- as.numeric(parts[2])

    if (exp > order) {
      warning(paste("Term", term, "has exponent greater than order", order, "and will be ignored."))
    } else {
      polyVec[as.character(exp)] <- polyVec[as.character(exp)] + coef
    }
  }

  return(as.numeric(polyVec))
}

# Differentiate the polynomial using linear algebra -----------------------

#' Differentiate a polynomial using a derivative matrix
#'
#' @param polyVec A numeric vector representing the polynomial.
#' @param d_dx The derivative matrix.
#' @param order The order of the polynomial.
#'
#' @returns A character string representing the derivative of the polynomial.
#' @export
#'
#' @examples
#' d_dx <- construct_derivate_matrix(4)
#' polyVec <- prep_polynomial("1+x+x^2+x^3+x^4", 4)
#' matrix_derivative(polyVec, d_dx, 4)
matrix_derivative <- function(polyVec, d_dx, order) {
  # calculate derivative using d_dx matrix
  derivative <- d_dx %*% polyVec


  # generate a human readable output
  result <- c()

  for (i in 1:order) {
    newCoef <- derivative[i]
    if (newCoef == 0) {
      next
    }

    # first term is just the constant part
    if (i == 1) {
      newTerm <- "x^0"
    } else {
      # terms with degree higher than 0 are constructed using paste
      newTerm <- paste("x", "^", i - 1, sep = "")
    }
    result <- append(result, paste(newCoef, "*", newTerm, sep = ""))
  }

  if (length(result) == 0) {
    return("0")
  }

  out <- paste(result, collapse = "+")
  out <- gsub("\\+-", "-", out)
  return(out)
}


#' Differentiate a polynomial
#'
#' @param polynomial A character string representing the polynomial.
#' @param order The order of the polynomial.
#'
#' @returns A character string representing the derivative of the polynomial.
#' @export
#'
#' @examples
#' differentiate_polynomial("1+x+x^2+x^3+x^4", order = 4)
differentiate_polynomial <- function(polynomial, order) {
  # Construct derivative matrix
  d_dx <- construct_derivate_matrix(order = order)

  # Process polynomial
  polyVec <- prep_polynomial(polynomial = polynomial, order = order)

  # Differentiate and return result
  out <- matrix_derivative(polyVec = polyVec, d_dx = d_dx, order = order)
  return(out)
}

# # Examples ----------------------------------------------------------------
#
# # these dont work
#
# # if constant not specified, it doesnot work
# differentiate_polynomial("x^2", order = 2)
#
# # if degree > 9 it doesnt work, as the exponent is the last char, 2 digit exponents are not handeled
# differentiate_polynomial("x+x^10", order = 10)
#
# # only works if terms are added, subtraction are not specified
# differentiate_polynomial("1-x-x^2", order = 2)
