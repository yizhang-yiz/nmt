set_nm_path <- function(p) {
    .nm_env$nm_path <- normalizePath(p, mustWork = FALSE)
}
