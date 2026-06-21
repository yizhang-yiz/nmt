#' Write a data.frame to .csv compatible with NONMEM format
#' One can use R's read.csv to read a NONMEM .csv file:
#'
#' read.csv(file, comment.char = ignore, na.strings = ".")
#'
#' @param data data object to be saved
#' @param file .csv file path
#' @export
write_nm_csv <- function(data, file) {
    x <- data
    colnames(x)[1] <- paste0("# ", colnames(x)[1])
    write.csv(x, file, na=".", quote=FALSE, row.names=FALSE)
}

#' Create an "nmodel" object based on given data and code.
#'
#' @param data data object for the model. It must contain necessary
#' columns for NONMEM models.
#' @param code NONMEM code, excluding $PROB, $DATA, and $INPUT.
#' @param tables list of variable arrays for table output. Each array
#' will be for one table. The table file's name is "tablex.tab", with
#' "x" enumerates from 1 to the number of arrays.
#' @return An "nmodel" object that contains the data and a vector of
#' characters for control stream.
#' @export
nm_model <- function(data, code, tables=NULL) {
    ## build_dir <- tempfile(pattern = paste0("nmt_cmake_", "_"), tmpdir = tempdir())
    ## dir.create(build_dir, showWarnings = FALSE, recursive = TRUE)
    ## write_nm_csv(data, file.path(build_dir, "data.csv"))

    nm_control <- paste0("$PROB model by nmt - ", date(), "\n",
                         "$INPUT ", paste(colnames(data), collapse=" "), "\n",
                         "$DATA data.csv", "\n")

    nm_control <- paste0(nm_control, code, "\n")

    nm_control <- paste0(nm_control,
                         paste("$TABLE", paste(colnames(data), collapse=" "),
                               "CPRED,CRES,CWRES,CIPRED,CIRES,CIWRES,CIPREDI,CIRESI,CIWRESI,EPRED,ERES,EWRES,NPDE,NOPRINT ONEHEADER,file=diagnostics.tab"), "\n")
    table_output <- paste(
        mapply(function(pars, id) paste0("$TABLE ",
                                         paste(pars, collapse=" "),
                                         " ONEHEADER", " file=table", id, ".tab"),
               tables, seq_along(tables)),
        collapse="\n")

    nm_control <- paste0(nm_control, table_output, "\n")
    structure(list(data=data, control=nm_control), class="nmodel")
}

#' Print the generate control stream from "nmodel" object
#'
#' @export
print.nmodel <- function(model) {
    cat(model$control)
}
