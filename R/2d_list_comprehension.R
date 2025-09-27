#' 2D List Comprehension
#' Emulates 2D list comprehension from Python.
#' @param row Number of rows
#' @param col Number of columns
#' @param x Expression to evaluate for each element
#' @param cond Condition to filter elements
#'
#' @return A matrix
#' @export
#'
#' @examples
#' # Default value is from 1 to row*col
#' list_comp_2d(row = 3, col = 3)
#' # zero matrix
#' list_comp_2d(row = 3, col = 3, x = 0)
#' # tetration
#' list_comp_2d(row = 3, col = 3, x = "i^i")
#' # ln(x)
#' list_comp_2d(row = 3, col = 3, x = "log(i)")
#' # cond test 1
#' list_comp_2d(row = 3, col = 3, cond = "i %% 2 == 0")
#' # cond test 2
#' list_comp_2d(row = 3, col = 3, cond = "i != 3 & i != 5")
list_comp_2d <- function(row, col, x = NULL, cond = NULL) {
  if (is.null(x) && is.null(cond)) {
    out <- t(matrix(comprehenr::to_vec(for (i in 1:(row * col)) i), nrow = col))
    return(out)
  } else if (!is.null(x) && is.null(cond)) {
    out <- t(matrix(comprehenr::to_vec(for (i in 1:(row * col)) eval(parse(text = x))), nrow = col))
    return(out)
  } else if (!is.null(cond) && is.null(x)) {
    # counting elements that are required of matrix of dim(row,col)
    elements <- row * col

    # count how many iterations are required given the condition

    cnt <- 0
    for (i in 1:10^6) {
      if (eval(parse(text = cond))) {
        cnt <- cnt + 1
      }
      if (cnt == elements) {
        maxElements <- i
        break
      }
    }

    # use upper limit (maxElements) for iteration
    out <- t(matrix(comprehenr::to_vec(for (i in 1:maxElements) if (eval(parse(text = cond))) i), nrow = row))
    return(out)
  }
}
