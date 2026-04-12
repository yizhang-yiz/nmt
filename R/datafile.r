datafile <- function(file) {
    x <- readLines(file, warn = FALSE)

    line <- x[grepl("^\\s*\\$DATA\\b", x)][1]

    sub("^\\s*\\$DATA\\b\\s*(\\S+).*", "\\1", line, perl = TRUE)
}
