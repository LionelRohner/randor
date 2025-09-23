
#' Title
#'
#' @param vec 
#'
#' @return
#' @export
#'
#' @examples
plot_all_digits <- function(vec){
  count_digits <- extract_digits_matrix(vec) %>%
    table() %>%
    as_tibble() 
  
  g_count_digits <- count_digits %>% 
    ggplot() +
    geom_bar(aes(x = ., y = n, fill = .),
             stat = "identity") +
    labs(title = "Distribution of the digits",
         x = "digits") +
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
plot_last_digits <- function(vec){
  count_digits <- extract_last_digits(vec) %>%
    table() %>%
    as_tibble() 
  
  g_count_digits <- count_digits %>% 
    ggplot() +
    geom_bar(aes(x = ., y = n, fill = .),
             stat = "identity") +
    labs(title = "Distribution of the last digits",
         x = "digits") +
    theme_minimal()
  print(g_count_digits)
}
