#' @export
set_cmake_config <- function(...) {
    .nm_env$cmake_config <- c(...)
}

#' @export
set_cmake_build <- function(...) {
    .nm_env$cmake_build <- c(...)
}
