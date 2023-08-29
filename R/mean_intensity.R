#' Calculate the mean intensity for each host
#'
#' @param data A data frame with host in the first column and intensity values in the subsequent columns
#'
#' @return A data frame with the mean intensity for each host
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(host = c(1, 1, 2, 2, 3, 3),
#'                    A = c(2, 0, 0, 0, 1, 0),
#'                    B = c(0, 0, 0, 0, 0, 0))
#' mean_intensity(data)
#'
#' @export
#'
mean_intensity <- function(data) {

  result <- data %>%
    dplyr::rename(host = 1) %>%
    dplyr::filter(!dplyr::if_all(2:tidyr::last_col(),
                                 ~.x == 0)) %>%
    dplyr::group_by(host) %>%
    dplyr::add_count(name = "n_host") %>%
    dplyr::relocate(n_host, .after = host) %>%
    tidyr::pivot_longer(cols = 3:tidyr::last_col()) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(host, name) %>%
    dplyr::summarise(
      mean = mean(value),
      sd = sd(value),
      se = sd(value)/sqrt(length(host))
    ) %>%
    ungroup()

  return(result)

}
