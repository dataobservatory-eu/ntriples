#' Get a person's authority IRI
#'
#' Resolve an authority identifier from a utils::person object.
#' Preference order: ORCID, ISNI, VIAF, Wikidata.
#'
#' @keywords internal
#' @noRd
get_person_iri <- function(p) {
  if (!inherits(p, "person") || length(p) != 1) {
    stop("get_person_iri(): p must be a single utils::person object.")
  }

  comments <- p$comment
  if (is.null(comments)) {
    return(NULL)
  }

  identifiers <- c(
    orcid = "https://orcid.org/",
    isni = "https://isni.org/isni/",
    viaf = "https://viaf.org/viaf/",
    wikidata = "https://www.wikidata.org/wiki/"
  )

  keys <- tolower(names(comments))

  for (key in names(identifiers)) {
    idx <- which(keys == key)
    if (length(idx) == 0) next

    value <- as.character(comments[[idx[1]]])
    if (is.na(value) || !nzchar(trimws(value))) next

    prefix <- identifiers[[key]]

    # Accept either an identifier or its full authority IRI
    if (startsWith(value, prefix)) {
      return(value)
    }

    return(paste0(prefix, value))
  }

  NULL
}
