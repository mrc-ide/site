example_site <- readRDS("c:/users/pwinskil/desktop/site.rds")
example_site$eir$eir <- 5

example_site$shape <- lapply(example_site$shape, function(x){
  data.frame()
})
example_site$population$population_by_age <- data.frame()

format(object.size(example_site), "Mb")

usethis::use_data(example_site, overwrite = TRUE)
