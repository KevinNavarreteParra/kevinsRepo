#' Make a bingo card
#'
#' @param str A character vector of at least length 25 denoting the contents of the bingo card
#' @param wrap A numeric value indicating the number of characters to wrap the text at
#' @param n_cards A numeric value indicating the number of bingo cards to create
#' @param free_space A character string to customize what the free space says
#' @param title A character string to customize the title of the bingo card
#' @param file_type A character string indicating the file type to save the bingo card as. Default is '.pdf', but can also be '.png', '.jpeg', or '.svg', among others supported by the `ggplot2::ggsave` function.
#' @param save A logical value indicating whether or not to save the bingo cards. Default is FALSE.
#'
#' @details
#' This function repurposes code found in Quang Nguyen's blog post on creating bingo cards using the ggplot2 package. The original blog post can be found [on his blog](https://qntkhvn.netlify.app/projects/bingo). I've updated the original code to include additional parameters as well as the additional string wrapping functionality to ensure that the text fits within the bingo card cells.
#'
#' @return A ggplot object of the bingo card(s) with the specified contents
#' @export
#'
#' @examples
#' # Create a bingo card with the default parameters
#' make_bingo(letters)
make_bingo <- function(str, wrap = 20, n_cards = 5, free_space = "Free", title = "Bingo!", file_type = ".pdf", save = FALSE) {

  if(!is.character(str)) {
    stop("str must be a character string")
  }

  if(!is.numeric(wrap)) {
    stop("wrap must be a numeric value")
  }

  if(length(str) < 25) {
    stop("str must be a vector of at least length 25")
  }

  if(!is.numeric(n_cards)) {
    stop("n_cards must be a numeric value")
  }

  str <- str_wrap(str, width = wrap)

  plots <- list()

  for(i in 1:n_cards){
      tibble::tibble(what = sample(str, 25),
                     row = rep(1:5, 5),
                     col = rep(1:5, each = 5)) %>%
      dplyr::mutate(what = ifelse(row == 3 & col == 3,
                           free_space, what)) %>%
      ggplot2::ggplot(ggplot2::aes(row, col)) +
      ggplot2::geom_tile(color = "black", fill = "white") +
      ggplot2::geom_text(ggplot2::aes(label = what), size = 2) +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5)) -> plot

    plots[[i]] <- plot

    if(save == TRUE) {
    ggplot2::ggsave(paste0("bingo_", i, file_type), plot)
    }
  }

  return(plots)

}
