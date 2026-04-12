inclfile <- function(file) {
    x <- readLines(file, warn = FALSE)

    include_lines <- grep("^\\s*\\$?INCLUDE\\s+\\S+", x, value = TRUE)
    sub("^\\s*\\$?INCLUDE\\s+(\\S+).*$", "\\1", include_lines, perl = TRUE)
}
