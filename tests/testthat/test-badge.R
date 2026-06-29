test_that("badge returns expected HTML with rgba conversion", {
  out <- badge(label = "Test", colour = "#112233", link = "https://example.com")
  html <- as.character(out)
  expect_true(length(html) >= 1)
  expect_match(html, "class=\"data-badge\"")
  expect_match(html, "rgba\\(17, 34, 51, 0.75\\)")
  expect_match(html, "https://example.com")
  expect_match(html, ">Test</a>")
})

test_that("badge helpers include expected label and link", {
  out <- un_badge()
  html <- as.character(out)
  expect_match(html, "UN")
  expect_match(html, "data-sources.html#UN")
})

test_that("all data-source badge helpers render valid badge HTML", {
  # Each helper maps to the anchor it should link to on the data-sources page.
  helpers <- c(
    un_badge = "#UN",
    worldpop_badge = "#WorldPop",
    map_badge = "#MAP",
    chirps_badge = "#CHIRPS",
    who_badge = "#WHO",
    vectors_badge = "#vectors",
    gadm_badge = "#GADM",
    dhs_badge = "#DHS",
    smc_badge = "#SMC",
    unicef_badge = "#UNICEF"
  )

  for (fn in names(helpers)) {
    html <- as.character(get(fn)())
    expect_match(html, "class=\"data-badge\"", info = fn)
    expect_match(html, "rgba\\([0-9]+, [0-9]+, [0-9]+, 0.75\\)", info = fn)
    expect_match(
      html,
      paste0("data-sources\\.html", helpers[[fn]]),
      info = fn
    )
  }
})
