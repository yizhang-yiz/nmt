#' Run the model either from file or "nmodel" object
#'
#' @param model model control stream file or "nmodel" object
#' @param new_builddir Whether to use new tempdir as build dir
#' @param quiet Whether to output cmake build info to screen
#' @return nmrun object
#' @export
nm <- function(model, new_builddir=FALSE, quiet=TRUE) {
    if (is.character(model)) {
        nm_file(model, new_builddir, quiet)
    } else if (inherits(model, "nmodel")) {
        write_nm_csv(model$data, "data.csv")
        write(model$control, file="model.ctl")
        nm_file("model.ctl", new_builddir, quiet)
    }
}

#' Run NONMEM model specified in a file
#'
#' The function runs a NONMEM model given in a control stream file. It requires that
#' the data file is located in the same path as the control stream.
#'
#' @param raw_model_path model control stream file path
#' @param new_builddir Whether to build in a new temp dir managed by R
#' @param quiet Whether to output cmake build info to screen
#' @return nmrun object
#' @export
nm_file <- function(raw_model_path, new_builddir, quiet) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    f <- basename(raw_model_path)
    f0 <- tools::file_path_sans_ext(f)
    build_dir <- ifelse(new_builddir, tempfile(pattern = paste0("cmake_", f0, "_")), paste0("cmake_", f0))
    dir.create(build_dir, showWarnings = FALSE, recursive = TRUE)
    model_file_path <- normalizePath(raw_model_path)
    model_dir <- normalizePath(dirname(raw_model_path))
    tables <- tablefile(raw_model_path)
    files <- c(file.path(model_dir, f),
               file.path(model_dir, datafile(raw_model_path)),
               file.path(model_dir, incfile(raw_model_path)))

    if (is.null(.nm_env$nm_path)) {
        stop("Use 'set_nm_path()' to set NONMEM path.")
    }
    if (is.null(.nm_env$nm_lic)) {
        stop("Use 'set_nm_lic()' to set NONMEM license path.")
    }

    print(paste0("-- building dir: ", build_dir))

    res <- sys::exec_wait("cmake",
                          args = c(.nm_env$nm_path, "-B", build_dir,
                                   paste0("-Dmodel=", model_file_path),
                                   "-GNinja", .nm_env$cmake_config))
    if (res != 0) stop(sprintf("cmake config failed with exit code %d", res))

    if (!quiet) {
        res <- sys::exec_wait("cmake",
                              args = c("--build", build_dir, .nm_env$cmake_build))
    } else {
        res <- sys::exec_wait("cmake",
                              args = c("--build", build_dir, .nm_env$cmake_build), std_out=NULL)
    }

    writeLines(readLines(file.path(model_dir, "FMSG")))
    if (res != 0) stop(sprintf("cmake build failed with exit code %d", res))

    res <- sys::exec_wait("cmake", args=c("--install", build_dir))
    if (res != 0) stop(sprintf("cmake install failed with exit code %d", res))

    if (.Platform$OS.type == "windows") {
        mod <- paste0(f0, ".exe")
    } else {
        mod <- file.path(".", f0)
    }

    setwd(model_dir)
    res <- sys::exec_wait(cmd=mod, args = c(file.path(f), paste0("-licfile=", .nm_env$nm_lic)))

    file.cov <- file.path(model_dir, paste0(f0, ".cov"))
    file.diagnostics <- file.path(model_dir, "diagnostics.tab")
    structure(list(nm=.nm_env$nm_path,
                   file_stem=f0,
                   files=files,
                   model=structure(list(data=NULL,
                                        control=paste(readLines(file.path(f), warn = FALSE),
                                                      collapse="\n")), class="nmodel"),
                   tables=tables,
                   result_path=model_dir,
                   diagnostics=if (file.exists(file.diagnostics)) read.table(file.diagnostics, header=TRUE, skip=1) else NULL,
                   cov=read_nm_table(file.cov) ),
                   class = "nmrun")
}
