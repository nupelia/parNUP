#' Calculate the prevalence of parasitism
#'
#' @param data A data frame with individual hosts as the first column and
#'   parasite counts as the remaining columns
#'
#' @return A data frame with the host as the first column and prevalence as
#'   the second
#'
#' @export
#'
#' @examples
#' data <- data.frame(host = c("A", "A", "B", "B"),
#'                    parasite1 = c(1, 0, 0, 1),
#'                    parasite2 = c(0, 1, 0, 0))
#' prevalence(data)
#' #>   host n_parasited n_host prevalence
#' #> 1    A            2      2        1.0
#' #> 2    B            1      2        0.5
#'
#' @importFrom dplyr rename mutate group_by add_count select distinct
#'   summarise
#' @importFrom tidyr last_col
#' @importFrom dplyr if_any
prevalence <- function(data) {
  data %>%
    dplyr::rename(host = 1) %>%
    dplyr::mutate(parasited = ifelse(dplyr::if_any(2:tidyr::last_col(),
                                                   ~.x != 0),
                                     1,
                                     0)) %>%
    dplyr::group_by(host) %>%
    dplyr::add_count(name = "n_parasited", wt = parasited) %>%
    dplyr::add_count(name = "n_host") %>%
    dplyr::select(host, n_parasited, n_host) %>%
    dplyr::distinct() %>%
    dplyr::group_by(host) %>%
    dplyr::summarise(prevalence = n_parasited/n_host)
}

