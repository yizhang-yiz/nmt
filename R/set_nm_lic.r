set_nm_lic <- function(p) {
    .nm_env$nm_lic <- normalizePath(p, mustWork = FALSE)
}
