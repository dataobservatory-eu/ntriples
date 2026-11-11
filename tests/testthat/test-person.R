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
  expect_identical(get_person_iri(p), "http://viaf.org/viaf/123456")

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
    comment = c(VIAF = "123456")
  )
  expect_identical(get_person_iri(p), "http://viaf.org/viaf/123456")

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

