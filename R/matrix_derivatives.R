
# Construct derivative matrix ---------------------------------------------


#' Title
#'
#' @param order
#'
#' @returns
#' @export
#'
#' @examples
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

#' Title
#'
#' @param polynomial
#' @param order
#'
#' @returns
#' @export
#'
#' @examples
prep_polynomial <- function(polynomial, order) {
  # string processing:
  # replace + by comma
  a <- gsub("\\+", ",", polynomial)

  # add coefficients of 1 if missing
  b <- gsub(",x", ",1x", a)

  # add exponents of 1 if missing (used for indexing)
  c <- gsub("x,", "x^1,", b)

  # make it iterable
  polynomialSplit <- unlist(strsplit(c, ","))

  # loop through polynomial and generate the polynomial vector
  polyVec <- vector(mode = "numeric", length = order)

  for (term in polynomialSplit) {
    # if term has length 1 it is expected to be the constant part of the polynomial
    if (nchar(term) == 1) {
      polyVec[1] <- term
    } else {
      # split terms into index (exponent of the term, i.e. last char) and coef (first char)
      index <- as.numeric(substring(term, nchar(term), nchar(term))) + 1
      coef <- substring(term, 1, 1)
      polyVec[index] <- coef
    }
  }
  # transform to numeric for math operations
  out <- as.numeric(polyVec)
  return(out)
}

# Differentiate the polynomial using linear algebra -----------------------

#' Title
#'
#' @param polyVec
#' @param d_dx
#' @param order
#'
#' @returns
#' @export
#'
#' @examples
#' differentiate_polynomial("1+x+x^2+x^3+x^4", order = 4)
#' differentiate_polynomial("1+x^9+3x^3+5x^6", order = 9)
#' differentiate_polynomial("0+x+x^2", order = 2)
matrix_derivative <- function(polyVec, d_dx, order) {
  # calculate derivative using d_dx matrix
  derivative <- d_dx %*% polyVec


  # generate a human readable output
  result <- c()

  for (i in 1:order) {
    # first term is just the constant part
    if (i == 1) {
      newCoef <- derivative[i]
      newTerm <- paste(i)
    } else {
      # terms with degree higher than 0 are constructed using paste
      newCoef <- derivative[i]
      newTerm <- paste("x", "^", i - 1, collapse = "", sep = "")
    }
    result <- append(result, paste(newCoef, "*", newTerm, collapse = "", sep = ""))

    # inner grepl function (remove zero terms) followed by concatenation
    paste(result[!grepl("^0", result)], sep = "", collapse = "+")
  }

  # remove 0 terms
  # result[!grepl("^0",result)]

  # make a string
  out <- paste(result[!grepl("^0", result)], sep = "", collapse = "+")
  return(out)
}


#' Title
#'
#' @param polynomial
#' @param order
#'
#' @returns
#' @export
#'
#' @examples
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
