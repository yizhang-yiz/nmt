datafile <- function(file) {
    x <- readLines(file, warn = FALSE)

    line <- x[grepl("^\\s*\\$DATA\\b", x)][1]

    sub("^\\s*\\$DATA\\b\\s*(\\S+).*", "\\1", line, perl = TRUE)
}

incfile <- function(file) {
    x <- readLines(file, warn = FALSE)

    include_lines <- grep("^\\s*\\$?INCLUDE\\s+\\S+", x, value = TRUE)
    sub("^\\s*\\$?INCLUDE\\s+(\\S+).*$", "\\1", include_lines, perl = TRUE)
}

tablefile <- function(file) {
  lines <- readLines(file, warn = FALSE)

  # Ignore anything after ';' on each line
  lines <- sub(";.*$", "", lines, perl = TRUE)

  out <- character()
  n <- length(lines)
  i <- 1

  while (i <= n) {
    if (grepl("^\\s*\\$TABLE\\b", lines[i], perl = TRUE)) {
      block <- lines[i]
      i <- i + 1

      while (i <= n && !grepl("^\\s*\\$", lines[i], perl = TRUE)) {
        block <- paste(block, lines[i])
        i <- i + 1
      }

      m <- regexec("\\bFILE\\s*=\\s*(['\"]?)([^\\s'\"]+)\\1",
                   block, ignore.case = TRUE, perl = TRUE)
      hit <- regmatches(block, m)[[1]]

      if (length(hit) >= 3) {
        out <- c(out, hit[3])
      }
    } else {
      i <- i + 1
    }
  }

  unique(out)
}
