#' set cmake config options
#' @description all arguments become cmake config options
#' @export
set_cmake_config <- function(...) {
    .nm_env$cmake_config <- c(...)
}

#' set cmake build options
#' @description all arguments become cmake build options
#' @export
set_cmake_build <- function(...) {
    .nm_env$cmake_build <- c(...)
}
