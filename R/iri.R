#' Create IRIs
#'
#' @param A string to convert into an IRI
#'
#' @returns An IRI
#' @export
#'
#' @examples
iri <- function(x) {
  if (!is.character(x)) stop("iri() expects a character vector.")
  x <- trimws(x)
  x <- sub("^<", "", x)   # remove starting <
  x <- sub(">$", "", x)   # remove ending >
  if (!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:", x[1])) {
    stop("Not a valid IRI: missing scheme")
  }
  structure(x, class = c("iri", "character"))
}
