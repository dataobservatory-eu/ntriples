
#' Create IRIs
#'
#' Validate absolute IRIs and assign the `iri` class.
#' Identifiers are preserved exactly as supplied.
#'
#' @param x A character vector of absolute IRIs.
#'
#' @return A character vector with class `iri`.
#' @examples
#' # IRI from the W3C N-Triples specification
#' subject <- iri("http://one.example/subject1")
#' subject
#'
#' # Use the IRI in an N-Triples statement
#' n_triple(
#'   subject,
#'   "http://one.example/predicate1",
#'   "http://one.example/object1"
#' )
#' @references
#' W3C. [RDF 1.2 N-Triples](https://www.w3.org/TR/rdf12-n-triples/).
#' @export
iri <- function(x) {
  if (!is.character(x) || anyNA(x)) {
    stop("iri(): x must be a character vector without missing values.")
  }

  # Require an absolute IRI scheme
  scheme <- "^[A-Za-z][A-Za-z0-9+.-]*:"

  # Characters forbidden in N-Triples IRI references
  forbidden <- '[<>"{}|^`\\\\[:space:][:cntrl:]]'

  if (any(!grepl(scheme, x))) {
    stop("iri(): invalid IRI: missing or invalid scheme.")
  }

  if (any(grepl(forbidden, x, perl = TRUE))) {
    stop("iri(): invalid IRI: contains forbidden characters.")
  }

  structure(x, class = c("iri", "character"))
}

