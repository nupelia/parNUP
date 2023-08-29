
#' Calculate the prevalence of parasitism
#'
#' @param data A data frame with individual hosts as the first column and parasite counts as the remaining columns
#'
#' @return A data frame with the prevalence of each species
#'
#' @export
#'
#' @examples
#' prevalence(dataset)
prevalence <- function(data) {

   result <- data %>%
    dplyr::rename(host = 1) %>%
    dplyr::group_by(host) %>%
    dplyr::add_count(name = "n_host") %>%
    dplyr::relocate(n_host, .after = host) %>%
    tidyr::pivot_longer(cols = 3:tidyr::last_col()) %>%
    dplyr::mutate(value = ifelse(value > 0, 1, 0)) %>%
    dplyr::group_by(host, name) %>%
    dplyr::add_count(wt = value) %>%
    dplyr::select(-value) %>%
    dplyr::distinct() %>%
    dplyr::summarise(
      prevalence = (n/n_host)*100
    ) %>%
    ungroup()

  return(result)

}
