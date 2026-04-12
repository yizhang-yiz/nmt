nm <- function(model) {
    if (is.character(model)) {
        nm_file(model)
    } else if (inherits(model, "nmodel")) {
    }
}

nm_file <- function(raw_model_path) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    f <- basename(raw_model_path)
    f0 <- tools::file_path_sans_ext(f)
    build_dir <- tempfile(pattern = paste0("cmake_", f0, "_"), tmpdir = tempdir())
    dir.create(build_dir, showWarnings = FALSE, recursive = TRUE)
    model_dir <- normalizePath(dirname(raw_model_path))
    files <- c(file.path(model_dir, f),
               file.path(model_dir, datafile(raw_model_path)),
               file.path(model_dir, inclfile(raw_model_path)))
    file.copy(from = files, , to = build_dir, overwrite = TRUE)

    if (is.null(.nm_env$nm_path)) {
        stop("Use 'set_nm_path()' to set NONMEM path.")
    }
    if (is.null(.nm_env$nm_lic)) {
        stop("Use 'set_nm_lic()' to set NONMEM license path.")
    }
    setwd(build_dir)
    res <- sys::exec_wait("cmake",
                          args = c(.nm_env$nm_path, "-B", ".",
                                   paste0("-Dmodel=", file.path(f)),
                                   "-GNinja", .nm_env$cmake_config))
    res <- sys::exec_wait("cmake",
                          args = c("--build", ".", .nm_env$cmake_build))

    if (.Platform$OS.type == "windows") {
        mod <- paste0(f0, ".exe")
    } else {
        mod <- file.path(".", f0)
    }

    res <- sys::exec_wait(cmd=mod, args = c(file.path(f), "output", paste0("-licfile=", .nm_env$nm_lic)))

    structure(list(nm=.nm_env$nm_path,
                   files=files,
                   model=paste(readLines(file.path(f), warn = FALSE), collapse="\n"),
                   result_path=build_dir,
                   cpu=as.numeric(readLines(file.path(paste0(f0, ".cpu")), warn = FALSE))),
                   class = "nmrun")
}

nm_save <- function(fit, to) {
    if(inherits(fit, "nmrun")) {
        f0 <- tools::file_path_sans_ext(basename(fit$files[1]))
        if (!dir.exists(to)) dir.create(to, recursive = TRUE)
        f <- list.files(fit$result_path, full.names=TRUE)
        f <- f[!grepl("\\.cmake$|\\.bak$|\\.a$|build\\.ninja|modules$|fort\\.[0-9]+$|CMake", f)]
        f <- f[!grepl(paste0(f0,"$"), f)]
        f <- f[!grepl(paste0(f0,".exe$"), f)]
        print("x")
        file.copy(f, to, overwrite=TRUE, copy.date=TRUE)
    }
}
