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

#' read nm output file, assume all tables have TITLE and LABEL. The
#' "TABLE_NO." is read as a new column "table", and the analysis
#' information as "analysis".
#' @description Read raw output files, such as ".ext", ".phi", ".shk", etc.
#'
#' @param infile path to a nm output file
#' @return data.frame of the read file, with "TABLE_NO." as "table" column.
#' @export
read_nm_table <- function(infile) {
if (!file.exists(infile)) return(NULL)

lines <- readLines(infile, warn = FALSE)
if (length(lines)==0) return(NULL)

# TABLE text is captured between the colon after the number and the colon that
# introduces the metadata, such as "Problem=1".
table_pattern <- paste0(
  "^\\s*TABLE\\s+NO\\.\\s*([0-9]+)\\s*:",
  "\\s*(.*?)\\s*:",
  "\\s*(?:Problem|Subproblem|Superproblem|Iteration)[0-9]*="
)

is_table_line <- function(x) {
  grepl("^\\s*TABLE\\s+NO\\.", x, perl = TRUE)
}

split_fields <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) character(0L) else strsplit(x, "\\s+", perl = TRUE)[[1L]]
}

parse_table_line <- function(x) {
  parts <- regmatches(x, regexec(table_pattern, x, perl = TRUE))[[1L]]
  if (length(parts) == 0L) {
    stop("Cannot parse TABLE NO. line: ", x)
  }
  list(
    table = as.integer(parts[2L]),
    analysis = trimws(parts[3L])
  )
}

header <- NULL
records <- list()
i <- 1L
n <- length(lines)

while (i <= n) {
  # Only a TABLE NO. line begins a chunk. Ignore blank/non-chunk text outside
  # chunks rather than attempting to guess whether it is a header or data row.
  if (!is_table_line(lines[i])) {
    i <- i + 1L
    next
  }

  table_info <- parse_table_line(lines[i])

  # The following physical line is required to be this chunk's label row.
  if (i + 1L > n) {
    stop("TABLE NO. line has no following label line: ", lines[i])
  }
  label_fields <- split_fields(lines[i + 1L])
  if (length(label_fields) == 0L) {
    stop("Empty label line after: ", lines[i])
  }

  # Use only the first label line as the output schema. Later label lines are
  # consumed here and never evaluated as possible data rows.
  if (is.null(header)) {
    header <- make.names(label_fields, unique = TRUE)
  } else if (length(label_fields) != length(header)) {
    warning(sprintf(
      paste0(
        "Ignoring labels for TABLE NO. %d: %d columns found; ",
        "the first label row has %d columns."
      ),
      table_info$table, length(label_fields), length(header)
    ))
  }

  i <- i + 2L

  # Read only lines between this chunk's label row and the next TABLE NO. line.
  # A nonblank line is a data row precisely when it has the first header's
  # field count. Crucially, no line is reclassified as a header based on value
  # type or contents, so a data value "FALSE" cannot become a spurious row.
  while (i <= n && !is_table_line(lines[i])) {
    fields <- split_fields(lines[i])

    if (length(fields) == 0L) {
      i <- i + 1L
      next
    }

    if (length(fields) != length(header)) {
      warning(sprintf(
        paste0(
          "Skipping line in TABLE NO. %d: expected %d fields, found %d. ",
          "Line: %s"
        ),
        table_info$table, length(header), length(fields), lines[i]
      ))
      i <- i + 1L
      next
    }

    records[[length(records) + 1L]] <- c(
      list(
        table = table_info$table,
        analysis = table_info$analysis
      ),
      stats::setNames(as.list(fields), header)
    )

    i <- i + 1L
  }
}

if (is.null(header)) {
  stop("No TABLE NO. block and label row were found in: ", infile)
}
if (length(records) == 0L) {
  stop("No data rows were found in: ", infile)
}

out <- do.call(rbind.data.frame, c(
  records,
  stringsAsFactors = FALSE,
  check.names = FALSE
))
out$table <- as.integer(out$table)

head(out, -1)
}

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
