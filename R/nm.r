#' Run the model either from file or "nmodel" object
#'
#' @param model model control stream file or "nmodel" object
#' @param cmake_stdout Whether to output cmake build info to screen
#' @return nmrun object
#' @export
nm <- function(model, cmake_stdout=FALSE) {
    if (is.character(model)) {
        nm_file(model, cmake_stdout)
    } else if (inherits(model, "nmodel")) {
        write_nm_csv(model$data, "data.csv")
        write(model$control, "./model.ctl")
        nm_file("./model.ctl", cmake_stdout)
    }
}

#' Run NONMEM model specified in a file
#'
#' The function runs a NONMEM model given in a control stream file. It requires that
#' the data file is located in the same path as the control stream.
#'
#' @param raw_model_path model control stream file path
#' @param cmake_stdout Whether to output cmake build info to screen
#' @return nmrun object
#' @export
nm_file <- function(raw_model_path, cmake_stdout) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    f <- basename(raw_model_path)
    f0 <- tools::file_path_sans_ext(f)
    build_dir <- tempfile(pattern = paste0("cmake_", f0, "_"), tmpdir = tempdir())
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
    setwd(build_dir)
    res <- sys::exec_wait("cmake",
                          args = c(.nm_env$nm_path, "-B", ".",
                                   paste0("-Dmodel=", model_file_path),
                                   "-GNinja", .nm_env$cmake_config))
    if (res != 0) stop(sprintf("cmake config failed with exit code %d", res))

    if (cmake_stdout) {
        res <- sys::exec_wait("cmake",
                              args = c("--build", ".", .nm_env$cmake_build))
    } else {
        res <- sys::exec_wait("cmake",
                              args = c("--build", ".", .nm_env$cmake_build), std_out=NULL)
    }

    if (res != 0) stop(sprintf("cmake build failed with exit code %d", res))

    res <- sys::exec_wait("cmake", args=c("--install", "."))
    if (res != 0) stop(sprintf("cmake install failed with exit code %d", res))

    if (.Platform$OS.type == "windows") {
        mod <- paste0(f0, ".exe")
    } else {
        mod <- file.path(".", f0)
    }

    setwd(model_dir)
    res <- sys::exec_wait(cmd=mod, args = c(file.path(f), paste0("-licfile=", .nm_env$nm_lic)))

    file.cov <- file.path(model_dir, paste0(f0, ".cov"))
    file.coi <- file.path(model_dir, paste0(f0, ".coi"))
    file.cor <- file.path(model_dir, paste0(f0, ".cor"))
    structure(list(nm=.nm_env$nm_path,
                   file_stem=f0,
                   files=files,
                   model=paste(readLines(file.path(f), warn = FALSE), collapse="\n"),
                   tables=tables,
                   result_path=model_dir,
                   par=raw_summary(file.path(model_dir, paste0(f0, ".ext"))),
                   diagnostics=read.table(file.path(model_dir, "diagnostics.tab"), header=TRUE, skip=1),
                   cov=if (file.exists(file.cov)) res_read_table(file.cov) else NULL,
                   coi=if (file.exists(file.coi)) res_read_table(file.coi) else NULL,
                   cor=if (file.exists(file.cor)) res_read_table(file.cor) else NULL),
                   class = "nmrun")
}

res_read_table <- function(file) {
    df <- read.table(file, header=TRUE, skip=1)
    df <- df[,2:ncol(df)]
    rownames(df) <- colnames(df)
    return(df)
}

#' read summary in .ext file from nmrun object.
#' @description Raw summary from the ".ext" file
#'
#' @export
raw_summary <- function(file) {
    if (file.size(file) == 0L) {
        return(NULL)
    } else {
        df <- read.table(file, header=TRUE, skip=1)
        df <- df[df$ITERATION<0, ]
        return(df)
    }
}

#' read .phi file from nmrun object.
#' @description Raw individual estimate from the ".phi" file
#'
#' @export
phi <- function(fit, summary=FALSE) {
    file <- paste0(tools::file_path_sans_ext(fit$files[1]), ".phi")
    if (file.size(file) == 0L) {
        return(NULL)
    } else {
        return(read.table(file, header=TRUE, skip=1))
    }
}

nm_save <- function(fit, to) {
    if(inherits(fit, "nmrun")) {
        f0 <- tools::file_path_sans_ext(basename(fit$files[1]))
        if (!dir.exists(to)) dir.create(to, recursive = TRUE)
        f <- list.files(fit$result_path, full.names=TRUE)
        f <- f[!grepl("\\.cmake$|\\.tmp$|\\.trash$|\\.bak$|\\.a$|build\\.ninja|modules$|fort\\.[0-9]+$|CMake", f)]
        f <- f[!grepl(paste0(f0,"$"), f)]
        f <- f[!grepl(paste0(f0,".exe$"), f)]
        print("x")
        file.copy(f, to, overwrite=TRUE, copy.date=TRUE)
    }
}
