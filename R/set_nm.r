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
#' @param nm_path NONMEM directory path
#' @param nm_lic NONMEM license path
#'
#' @return Normalized path to NONMEM license directory
#' @export
set_nm <- function(nm_path, nm_lic) {
    set_nm_path(nm_path)
    set_nm_lic(nm_lic)
}
