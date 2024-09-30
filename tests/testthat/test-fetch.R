test_that("has a default location", {
  cfg <- location_configuration()
  expect_equal(cfg$type, "packit")
  expect_match(cfg$args$url, "^https?://")
})

test_that("can override github token of default location", {
  withr::local_envvar(GITHUB_TOKEN="mytoken")
  cfg <- location_configuration()

  expect_equal(cfg$type, "packit")
  expect_match(cfg$args$url, "^https?://")
  expect_equal(cfg$args$token, "mytoken")
})

test_that("can override default location", {
  cfg <- list(type = "path", args = list(path = "path/to/location"))
  withr::local_options("site.orderly_location" = cfg)
  expect_equal(location_configuration(), cfg)
})

test_that("can configure orderly repository", {
  local_test_setup()

  root <- suppressMessages(configure_orderly())
  expect_true(fs::dir_exists(root))
  expect_contains(orderly2::orderly_location_list(root = root), LOCATION_NAME)
})

test_that("can change orderly location", {
  local_test_setup()
  upstream1 <- local_orderly_root()
  upstream2 <- local_orderly_root()

  withr::local_options("site.orderly_location" = list(
    type = "path",
    args = list(path = upstream1)))

  root <- suppressMessages(configure_orderly())
  res <- orderly2::orderly_location_list(root = root, verbose = TRUE)
  expect_equal(res[[which(res$name == LOCATION_NAME), "args"]]$path, upstream1)

  withr::local_options("site.orderly_location" = list(
    type = "path",
    args = list(path = upstream2)))

  root <- configure_orderly()
  res <- orderly2::orderly_location_list(root = root, verbose = TRUE)

  expect_equal(res[[which(res$name == LOCATION_NAME), "args"]]$path, upstream2)
})


test_that("can fetch files from upstream", {
  upstream <- local_test_setup()

  id <- create_orderly_packet("data", list("data.txt" = "Contents"),
                              root = upstream)

  dest <- withr::local_tempdir()
  res <- suppressMessages(fetch_files("data", list(), dest, "data.txt"))

  expect_equal(readLines(fs::path(dest, "data.txt")), "Contents")
  expect_equal(id, res)
})


test_that("files are cached and only downloaded once", {
  upstream <- local_test_setup()

  id <- create_orderly_packet("data", list("data.txt" = "Contents"),
                              root = upstream)

  dest <- withr::local_tempdir()
  suppressMessages({
    expect_message(fetch_files("data", list(), dest,
                               c("first.txt" = "data.txt")),
                   "Need to fetch 1 file")

    expect_no_message(fetch_files("data", list(), dest,
                                  c("second.txt" = "data.txt")),
                      message = "Need to fetch 1 file")
  })

  expect_equal(readLines(fs::path(dest, "first.txt")), "Contents")
  expect_equal(readLines(fs::path(dest, "second.txt")), "Contents")
})


test_that("can filter based on parameters", {
  upstream <- local_test_setup()

  create_orderly_packet("data", list("data.txt" = "Hello"),
                        parameters = list(code = "foo"),
                        root = upstream)

  create_orderly_packet("data", list("data.txt" = "World"),
                        parameters = list(code = "bar"),
                        root = upstream)

  dest <- withr::local_tempdir()

  suppressMessages(fetch_files("data", list(code = "foo"),
                               dest, c("hello.txt" = "data.txt")))

  suppressMessages(fetch_files("data", list(code = "bar"),
                               dest, c("world.txt" = "data.txt")))

  suppressMessages(fetch_files("data", list(), dest,
                               c("latest.txt" = "data.txt")))

  expect_equal(readLines(fs::path(dest, "hello.txt")), "Hello")
  expect_equal(readLines(fs::path(dest, "world.txt")), "World")
  expect_equal(readLines(fs::path(dest, "latest.txt")), "World")
})


test_that("can filter based on packet ID", {
  upstream <- local_test_setup()

  id1 <- create_orderly_packet("data", list("data.txt" = "Hello"),
                               parameters = list(code = "foo"),
                               root = upstream)

  id2 <- create_orderly_packet("data", list("data.txt" = "World"),
                               parameters = list(code = "bar"),
                               root = upstream)

  dest <- withr::local_tempdir()

  suppressMessages(fetch_files("data", list(), expr = id1,
                               dest, c("hello.txt" = "data.txt")))

  suppressMessages(fetch_files("data", list(), expr = id2,
                               dest, c("world.txt" = "data.txt")))

  expect_equal(readLines(fs::path(dest, "hello.txt")), "Hello")
  expect_equal(readLines(fs::path(dest, "world.txt")), "World")
})


test_that("can filter based on an arbitrary query", {
  upstream <- local_test_setup()

  id1 <- create_orderly_packet("data", list("data.txt" = "Hello"),
                               parameters = list(foo = 1, bar = 1),
                               root = upstream)

  id2 <- create_orderly_packet("data", list("data.txt" = "World"),
                               parameters = list(foo = 1, bar = 2),
                               root = upstream)

  dest <- withr::local_tempdir()

  suppressMessages(fetch_files(
    expr = "single(parameter:foo == parameter:bar)",
    "data", list(), dest, c("hello.txt" = "data.txt")))

  suppressMessages(fetch_files(
    expr = "single(parameter:foo != parameter:bar)",
    "data", list(), dest, c("world.txt" = "data.txt")))

  expect_equal(readLines(fs::path(dest, "hello.txt")), "Hello")
  expect_equal(readLines(fs::path(dest, "world.txt")), "World")
})


test_that("can fetch site", {
  upstream <- local_test_setup()

  make <- function(iso3c, admin_level, version, value) {
    create_orderly_packet(
      "calibration_diagnostics",
      list("calibrated_scaled_site.rds" = list(value = value)),
      parameters = list(iso3c = iso3c, version = version,
                        admin_level = admin_level),
      root = upstream)
  }
  fetch <- function(...) suppressMessages(fetch_site(...))

  id1 <- make("TGO", 1, "v1", value = 1)
  id2 <- make("TGO", 2, "v1", value = 2)
  id3 <- make("NGA", 1, "v1", value = 3)
  id4 <- make("NGA", 1, "v2", value = 4)

  expect_equal(fetch("TGO"), list(value = 2))
  expect_equal(fetch("NGA"), list(value = 4))

  expect_equal(fetch("TGO", admin_level = 1), list(value = 1))
  expect_equal(fetch("TGO", admin_level = 2), list(value = 2))

  expect_equal(fetch("NGA", version = "v1"), list(value = 3))
  expect_equal(fetch("NGA", version = "v2"), list(value = 4))

  expect_equal(fetch(id = id1), list(value = 1))
  expect_equal(fetch(id = id2), list(value = 2))
  expect_equal(fetch(id = id3), list(value = 3))
  expect_equal(fetch(id = id4), list(value = 4))

  expect_error(fetch("TGO", id = id1),
               "Exactly one of `iso3c` and `id` must be supplied")
})
