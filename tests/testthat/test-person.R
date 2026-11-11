# Authority identifier resolution ----------------------------------------

test_that("get_person_iri() resolves authority identifiers", {
  # ORCID
  p <- utils::person("Jane", "Doe", comment = c(ORCID = "0000-0002-1825-0097"))
  expect_identical(get_person_iri(p), "https://orcid.org/0000-0002-1825-0097")

  # ISNI
  p <- utils::person("Jane", "Doe", comment = c(ISNI = "000000012146438X"))
  expect_identical(get_person_iri(p), "https://isni.org/isni/000000012146438X")

  # VIAF
  p <- utils::person("Jane", "Doe", comment = c(VIAF = "123456"))
  expect_identical(get_person_iri(p), "https://viaf.org/viaf/123456")

  # Wikidata
  p <- utils::person("Jane", "Doe", comment = c(wikidata = "Q42"))
  expect_identical(get_person_iri(p), "https://www.wikidata.org/wiki/Q42")
})


# Identifier normalization -----------------------------------------------

test_that("get_person_iri() preserves complete authority IRIs", {
  # ORCID
  p <- utils::person("Jane", "Doe",
    comment = c(ORCID = "https://orcid.org/0000-0002-1825-0097")
  )
  expect_identical(get_person_iri(p), "https://orcid.org/0000-0002-1825-0097")

  # ISNI
  p <- utils::person("Jane", "Doe",
    comment = c(ISNI = "https://isni.org/isni/000000012146438X")
  )
  expect_identical(get_person_iri(p), "https://isni.org/isni/000000012146438X")

  # VIAF
  p <- utils::person("Jane", "Doe",
    comment = c(VIAF = "https://viaf.org/viaf/123456")
  )
  expect_identical(get_person_iri(p), "https://viaf.org/viaf/123456")

  # Wikidata
  p <- utils::person("Jane", "Doe",
    comment = c(wikidata = "https://www.wikidata.org/wiki/Q42")
  )
  expect_identical(get_person_iri(p), "https://www.wikidata.org/wiki/Q42")
})


# Authority preference ---------------------------------------------------

test_that("get_person_iri() applies authority preference order", {
  # ORCID takes precedence over Wikidata
  p <- utils::person("Jane", "Doe", comment = c(
    wikidata = "Q42",
    ORCID = "0000-0002-1825-0097"
  ))
  expect_identical(get_person_iri(p), "https://orcid.org/0000-0002-1825-0097")

  # ISNI takes precedence over VIAF
  p <- utils::person("Jane", "Doe", comment = c(
    VIAF = "123456",
    ISNI = "000000012146438X"
  ))
  expect_identical(get_person_iri(p), "https://isni.org/isni/000000012146438X")
})


# Missing identifiers and validation ------------------------------------

test_that("get_person_iri() handles missing and invalid identifiers", {
  # No authority identifiers
  expect_null(get_person_iri(utils::person("Jane", "Doe")))

  # Unrecognized metadata
  p <- utils::person("Jane", "Doe", comment = c(note = "Archivist"))
  expect_null(get_person_iri(p))

  # Invalid input
  expect_error(get_person_iri("Jane Doe"), "utils::person")
  expect_error(get_person_iri(42), "utils::person")
})


# PROV-O agent construction ----------------------------------------------

test_that("prov_author() creates PROV-O agent statements", {
  rdf_type <- "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
  prov_agent <- "<http://www.w3.org/ns/prov#Agent>"

  # Identified author
  p <- utils::person("Jane", "Doe", comment = c(ORCID = "0000-0002-1825-0097"))
  expect_identical(
    prov_author(p),
    paste(
      "<https://orcid.org/0000-0002-1825-0097>",
      rdf_type, prov_agent, "."
    )
  )

  # Unidentified author
  p <- utils::person("Jane", "Doe")
  expect_identical(
    prov_author(p),
    paste("_:janedoe", rdf_type, prov_agent, ".")
  )
})


# Multiple authors -------------------------------------------------------

test_that("prov_author() handles multiple authors", {
  authors <- c(
    utils::person("Jane", "Doe", comment = c(ORCID = "0000-0002-1825-0097")),
    utils::person("John", "Smith")
  )

  triples <- prov_author(authors)

  expect_type(triples, "character")
  expect_length(triples, 2)
  expect_true(any(grepl("orcid.org", triples, fixed = TRUE)))
  expect_true(any(grepl("_:johnsmith", triples, fixed = TRUE)))
  expect_identical(triples, unique(triples))
})


# Input validation -------------------------------------------------------

test_that("prov_author() rejects unsupported inputs", {
  expect_error(prov_author("Jane Doe"), "utils::person")
  expect_error(prov_author(42), "utils::person")
  expect_error(prov_author(list("Jane Doe")), "utils::person")
})
