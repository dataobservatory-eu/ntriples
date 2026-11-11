#' Escape special characters in RDF literals
#'
#' Escapes backslashes, quotation marks, and control characters
#' for valid N-Triples literal serialization.
#'
#' @param x A character string to escape.
#' @return The escaped character string.
#' @keywords internal
#' @noRd
escape_literal <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  x
}
