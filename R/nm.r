nm <- function(build_path, file_path, ...) {
  res <- sys::exec_internal("cmake", args = c("../../nonmem",
  paste0("-B", build_path), paste0("-Dmodel=", file_path), "-GNinja"),
  ...)
  list(
    status = res$status,
    stdout = sys::as_text(res$stdout),
    stderr = sys::as_text(res$stderr)
  )
}
