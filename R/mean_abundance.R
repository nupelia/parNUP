#' Calculate mean abundance of species
#'
#' This function calculates the mean abundance of species across hosts.
#'
#' @param data A data frame with columns for hosts (column 1) and species (columns 3 onwards).
#'
#' @return A data frame with columns for host, species, mean, sd, and se.
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(
#'   host = c("A", "A", "A", "B", "B", "B"),
#'   species1 = c(1, 5, 3, 2, 4, 0),
#'   species2 = c(3, 2, 0, 1, 0, 0)
#' )
#'
#' mean_abundance(data)
#'
mean_abundance <- function(data) {

  result <- data %>%
    dplyr::rename(host = 1) %>%
    dplyr::group_by(host) %>%
    dplyr::add_count(name = "n_host") %>%
    dplyr::relocate(n_host, .after = host) %>%
    tidyr::pivot_longer(cols = 3:tidyr::last_col()) %>%
    dplyr::group_by(host, name) %>%
    dplyr::summarise(
      mean = mean(value),
      sd = sd(value),
      se = sd(value)/sqrt(length(host))
    ) %>%
    ungroup()

  return(result)

}
