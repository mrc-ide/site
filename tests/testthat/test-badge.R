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
