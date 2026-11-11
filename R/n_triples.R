#' Create N-Triples
#'
#' Create RDF triple statements to annotate your dataset with standard,
#' interoperable metadata.
#'
#' N-Triples is a line-based serialization format for RDF. It is easy to parse
#' and widely supported. For details, see the
#' [W3C RDF 1.2 N-Triples specification](https://www.w3.org/TR/rdf12-n-triples/).
#'
#' @param triples A character vector of concatenated N-Triples, created with
#'   [`n_triple()`].
#'
#' @return A character vector of unique N-Triple strings.
#'
#' @examples
#' triple_1 <- n_triple(
#'   "http://example.org/show/218",
#'   "http://www.w3.org/2000/01/rdf-schema#label",
#'   "That Seventies Show"
#' )
#'
#' triple_2 <- n_triple(
#'   "http://example.org/show/218",
#'   "http://example.org/show/localName",
#'   '"Cette Série des Années Septante"@fr-be'
#' )
#'
#' n_triples(c(triple_1, triple_2, triple_1))
#'
#' @export


n_triples <- function(triples) {
  unique(triples)
}


#' @param p Predicate IRI, or `"a"` for `rdf:type`.
#' @param o Object IRI, blank node, literal, or supported R scalar.
#'
#' @return A single character string containing an N-Triple.
#' @export
n_triple <- function(s, p, o) {
  s <- rdf_term(s)
  p <- rdf_term(p)
  o <- rdf_term(o)

  # Validate RDF term positions
  is_iri <- function(x) {
    grepl("^<[^<>[:space:]]+>$", x)
  }

  is_blank <- function(x) {
    grepl("^_:[A-Za-z][A-Za-z0-9._-]*$", x)
  }

  if (!is_iri(s) && !is_blank(s)) {
    stop("n_triple(): subject must be an IRI or blank node.")
  }

  if (!is_iri(p)) {
    stop("n_triple(): predicate must be an IRI.")
  }

  sprintf("%s %s %s .", s, p, o)
}
