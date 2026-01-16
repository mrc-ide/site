net_efficacy <- read.csv("data-raw/net_efficacy_adjusted.csv") |>
  dplyr::mutate(gamman = gamman * 365)

usethis::use_data(net_efficacy, overwrite = TRUE)
