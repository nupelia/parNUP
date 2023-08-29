teste <- dplyr::tibble(
  host = c("a", "a", "b", "c"),
  par1 = c(1,1,0,2),
  par2 = c(0,0,0,1)
)

library(dplyr)
teste %>%
  group_by(host) %>%
  add_count(name = "n_host") %>%
  dplyr::relocate(n_host, .after = 1) %>%
  tidyr::pivot_longer(
    cols = 3:tidyr::last_col()
  ) %>%
  add_count(wt = value, name = "infected_hosts") %>%
  dplyr::filter(infected_hosts != 0) %>%
  dplyr::select(-c(name, value)) %>%
  dplyr::distinct() %>%
  dplyr::summarise(
    prevalence = (infected_hosts/n_host)*100,

  )
