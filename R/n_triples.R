#' Create N-Triples
#'
#' Create RDF triple statements to annotate your dataset with standard,
#' interoperable metadata.
#'
#' N-Triples is a line-based serialization format for RDF. It is easy to parse
#' and widely supported. For details, see the
#' [W3C RDF 1.2 N-Triples specification](https://www.w3.org/TR/rdf12-n-triples/).
#'
#' Use [`rdf_term()`] to serialize individual R values as RDF terms,
#' [`n_triple()`] to assemble a subject, predicate, and object into a
#' statement, and `n_triples()` to collect statements and remove duplicates.
#'
#' @param triples A character vector of N-Triples statements, created with
#'   [`n_triple()`].
#'
#' @return A character vector of unique N-Triple strings.
#'
#' @seealso [`rdf_term()`], [`n_triple()`], [`iri()`],
#'   [`person_triples()`]
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


#' Create a single N-Triple statement
#'
#' Assemble a subject, predicate, and object into a single RDF statement
#' in N-Triples syntax.
#'
#' This function converts individual values using [`rdf_term()`] and
#' validates their positions in the statement. Subjects must be IRIs or
#' blank nodes, predicates must be IRIs, and objects may also be literals.
#'
#' Use [`n_triples()`] as the main entry point for collecting statements
#' into a unique set of N-Triples.
#'
#' @param s Subject IRI or blank node.
#' @param p Predicate IRI, or `"a"` for `rdf:type`.
#' @param o Object IRI, blank node, literal, or supported R scalar.
#'
#' @return A single character string containing an N-Triple statement.
#'
#' @seealso [`n_triples()`], [`rdf_term()`], [`iri()`]
#'
#' @examples
#' n_triple(
#'   "https://example.org/dataset/1",
#'   "http://purl.org/dc/terms/title",
#'   "Growth of Orange Trees"
#' )
#'
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
