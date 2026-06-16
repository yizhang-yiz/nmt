#' Set NONMEM path
#'
#' @param p NONMEM directory path
#' @return Normalized path to NONMEM directory
#' @export
set_nm_path <- function(p) {
    .nm_env$nm_path <- normalizePath(p, mustWork = TRUE)
}

#' Set NONMEM license path
#'
#' @param p NONMEM license path
#' @return Normalized path to NONMEM license
#' @export
set_nm_lic <- function(p) {
    .nm_env$nm_lic <- normalizePath(p, mustWork = TRUE)
}

#' Set NONMEM & lic path
#'
#' @param p1 NONMEM directory path
#' @param p2 NONMEM license path
#'
#' @return Normalized path to NONMEM license directory
#' @export
set_nm <- function(p1, p2) {
    set_nm_path(p1)
    set_nm_lic(p2)
}
