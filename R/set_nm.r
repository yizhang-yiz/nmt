set_nm_path <- function(p) {
    .nm_env$nm_path <- normalizePath(p, mustWork = TRUE)
}

set_nm_lic <- function(p) {
    .nm_env$nm_lic <- normalizePath(p, mustWork = TRUE)
}
