#' Convert an R scalar to an RDF term
#'
#' Serialize a scalar R value as an RDF term in N-Triples syntax.
#' Character values are serialized as XSD string literals, numeric and
#' logical values as appropriately typed literals, and IRIs as RDF
#' resources. Already serialized RDF terms are preserved.
#'
#' Factors must be explicitly converted to character labels or integer
#' category codes before serialization. Person objects and other
#' structured entities are not supported.
#'
#' @param x A single, non-missing R value or RDF term.
#'
#' @return A character scalar containing an RDF term in N-Triples syntax.
#'
#' @examples
#' rdf_term("Growth of Orange Trees")
#' rdf_term(42L)
#' rdf_term(iri("urn:example:dataset"))
#'
#' @export
rdf_term <- function(x) {
  if (length(x) != 1 ||
    is.list(x) ||
    (is.atomic(x) && anyNA(x))) {
    stop("rdf_term(): input must be a non-missing scalar value.")
  }

  # Factors require an explicit representation choice
  if (is.factor(x)) {
    stop(
      "rdf_term(): factors cannot be serialized unambiguously. ",
      "Use as.character() for category labels or ",
      "as.integer() for category codes."
    )
  }

  xsd <- "http://www.w3.org/2001/XMLSchema#"

  # Explicit IRIs
  if (inherits(x, "iri")) {
    return(sprintf("<%s>", as.character(x)))
  }

  # Typed R values
  if (inherits(x, "POSIXct")) {
    value <- format_xsd_datetime(x)
    return(sprintf('"%s"^^<%sdateTime>', value, xsd))
  }

  if (inherits(x, "Date")) {
    return(sprintf('"%s"^^<%sdate>', as.character(x), xsd))
  }

  if (is.integer(x)) {
    return(sprintf('"%s"^^<%sinteger>', x, xsd))
  }

  if (is.numeric(x)) {
    return(sprintf('"%s"^^<%sdouble>', as.character(x), xsd))
  }

  if (is.logical(x)) {
    return(sprintf(
      '"%s"^^<%sboolean>',
      tolower(as.character(x)), xsd
    ))
  }

  if (!is.character(x)) {
    stop("rdf_term(): unsupported input type.")
  }

  # RDF type shorthand
  if (identical(x, "a")) {
    return("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")
  }

  # Already serialized RDF terms
  if (grepl("^<[^<>[:space:]]+>$", x)) {
    return(x)
  }

  if (grepl(
    '^"(?:[^"\\\\]|\\\\.)*"(?:@[A-Za-z]+(?:-[A-Za-z0-9]+)*|\\^\\^<[^<>[:space:]]+>)?$',
    x,
    perl = TRUE
  )) {
    return(x)
  }

  # Blank nodes
  if (grepl("^_:[A-Za-z][A-Za-z0-9._-]*$", x)) {
    return(x)
  }

  # HTTP(S) IRIs
  if (grepl(
    '^https?://[^[:space:]<>"{}|^`\\\\]+$',
    x,
    perl = TRUE
  )) {
    return(sprintf("<%s>", x))
  }

  # Ordinary character literals
  sprintf('"%s"^^<%sstring>', escape_literal(x), xsd)
}
