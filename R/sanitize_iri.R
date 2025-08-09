#' Sanitize a Text String for IRIs
#'
#' @param x A string to convert into an IRI
#'
#' @returns A string with removed illegal characters, expressions.
#' @keywords internal
sanitize_iri <- function(x) {
  if (!is.character(x)) stop("sanitize_iri() expects a character vector.")
  x <- trimws(x)
  # Percent-encode spaces
  x <- gsub(" ", "%20", x, fixed = TRUE)
  # Remove illegal angle brackets
  x <- gsub("<|>", "", x)
  # Lowercase scheme
  x <- sub("^([A-Z]+):", function(m) tolower(m), x, perl = TRUE)
  x
}

