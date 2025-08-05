local_orderly_root <- function(..., .local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  suppressMessages(orderly2::orderly_init(root, ...))
  root
}

#' Configure the site package to use a temporary cache directory and upstream.
#'
#' This setup is suitable for unit testing the package and is automatically torn
#' down when the caller's frame exits.
#'
#' @return the path to the orderly root used by the package as the upstream.
#' @noRd
local_test_setup <- function(.local_envir = parent.frame()) {
  cache <- withr::local_tempdir(.local_envir = .local_envir)
  upstream <- local_orderly_root(.local_envir = .local_envir)

  withr::local_envvar(R_USER_CACHE_DIR = cache, .local_envir = .local_envir)
  withr::local_options(
    .local_envir = .local_envir,
    "site.orderly_location" = list(type = "path",
                                   args = list(path = upstream)))

  upstream
}

#' Create an orderly packet with the given parameters and contents.
#'
#' Orderly doesn't have an easy way to craft packets from scratch, so we have
#' to resort to creating a report directory, writing the files into it,
#' generating a <name>.R file with a call to `orderly_parameters` in it and
#' running the report.
#'
#' @param name the name used for the packet
#' @param files a named list of files and their contents to include in the
#'   report. If the filename ends in `.rds`, the value is written with
#'   [saveRDS], otherwise [writeLines] is used.
#' @param parameters a named list of parameters to attach to the packet.
#' @param root the orderly root in which the packet will be created.
#' @return the packet id.
#' @noRd
create_orderly_packet <- function(name, files, parameters = list(), root) {
  src <- fs::dir_create(root, "src", name)
  withr::defer(fs::dir_delete(src))

  args <- paste0(sprintf("%s = NULL", names(parameters)), collapse=",")
  code <- sprintf("p <- orderly2::orderly_parameters(%s)", args)
  writeLines(code, fs::path(src, sprintf("%s.R", name)))

  for (i in seq_along(files)) {
    f <- fs::path(src, names(files)[[i]])
    if (fs::path_ext(f) == "rds") {
      saveRDS(files[[i]], f)
    } else {
      writeLines(files[[i]], f)
    }
  }
  suppressMessages(orderly2::orderly_run(name, parameters, echo = FALSE,
                                         root = root))
}
