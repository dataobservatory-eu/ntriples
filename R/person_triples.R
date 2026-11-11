#' Serialize a person as RDF provenance statements
#'
#' Creates RDF statements describing a `utils::person` as an agent,
#' using PROV-O by default or FOAF as an alternative.
#'
#' Uses an ORCID, ISNI, VIAF, or Wikidata identifier when available.
#' Otherwise, a blank-node identifier must be supplied through `subject`.
#' Person roles are deliberately ignored; role serialization belongs
#' to a separate function.
#'
#' @param x A single `utils::person` object.
#' @param ontology Ontology to use: `"prov"` (default) or `"foaf"`.
#' @param subject Optional IRI or blank-node identifier for the person.
#'   Overrides any authority identifier found in `x`.
#' @return A character vector containing N-Triples statements.
#' @examples
#' author <- utils::person(
#'   given = "Jane", family = "Doe",
#'   comment = c(ORCID = "0000-0002-1825-0097")
#' )
#' person_triples(author)
#'
#' # Use a blank node when no authority identifier is available
#' contributor <- utils::person(given = "John", family = "Smith")
#' person_triples(contributor, subject = "_:john")
#' @export
person_triples <- function(
  x,
  ontology = c("prov", "foaf"),
  subject = NULL
) {
  ontology <- match.arg(ontology)

  if (!inherits(x, "person") || length(x) != 1) {
    stop("person_triples(): x must be a single utils::person object.")
  }

  # Resolve the person's identifier
  if (is.null(subject)) {
    subject <- get_person_iri(x)
  }

  if (is.null(subject)) {
    stop("person_triples(): subject is required without an authority IRI.")
  }

  # Subjects must be absolute IRIs or valid blank-node identifiers
  if (!is.character(subject) || length(subject) != 1 || is.na(subject)) {
    stop("person_triples(): subject must be an IRI or blank node.")
  }

  if (!grepl("^_:[A-Za-z][A-Za-z0-9._-]*$", subject)) {
    subject <- iri(as.character(subject))
  }

  # Ontology-specific terms
  vocabularies <- list(
    prov = list(
      type = "http://www.w3.org/ns/prov#Agent",
      name = "http://www.w3.org/2000/01/rdf-schema#label"
    ),
    foaf = list(
      type = "http://xmlns.com/foaf/0.1/Person",
      name = "http://xmlns.com/foaf/0.1/name"
    )
  )

  vocab <- vocabularies[[ontology]]

  # Agent type is always present
  triples <- n_triple(subject, "a", vocab$type)

  # Add a name only when available
  name_parts <- c(x$given, x$family)
  name_parts <- name_parts[!is.na(name_parts) & nzchar(trimws(name_parts))]
  name <- trimws(paste(name_parts, collapse = " "))

  if (nzchar(name)) {
    triples <- c(triples, n_triple(subject, vocab$name, name))
  }

  n_triples(triples)
}
