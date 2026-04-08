nm <- function(build_path, file_path, ...) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    res <- sys::exec_wait("cmake",
                          args = c(.nm_env$nm_path,
                                   paste0("-B", build_path), paste0("-Dmodel=", file_path), "-GNinja", ...))
    res <- sys::exec_wait("cmake",
                          args = c("--build", "."))
    res <- sys::exec_wait("cmake",
                          args = c("--install", "."))

    f <- basename(file_path)
    setwd(dirname(file_path))
    if (.Platform$OS.type == "windows") {
        mod <- paste0(tools::file_path_sans_ext(f), ".exe")
    } else {
        mod <- file.path(".", tools::file_path_sans_ext(f))
    }
    res <- sys::exec_wait(cmd=mod, args = c(f, "output", paste0("-licfile=", .nm_env$nm_lic)))
}
