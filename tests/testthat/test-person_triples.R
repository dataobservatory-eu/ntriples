# PROV-O serialization ---------------------------------------------------

test_that("person_triples() defaults to PROV-O", {
  p <- utils::person(
    given = "Jane", family = "Smith",
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(p)
  subject <- "<https://orcid.org/0000-0002-1825-0097>"
  rdf_type <- "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
  prov_agent <- "<http://www.w3.org/ns/prov#Agent>"
  label <- "<http://www.w3.org/2000/01/rdf-schema#label>"
  name <- '"Jane Smith"^^<http://www.w3.org/2001/XMLSchema#string>'

  expect_length(triples, 2)
  expect_true(paste(subject, rdf_type, prov_agent, ".") %in% triples)
  expect_true(paste(subject, label, name, ".") %in% triples)
})


# FOAF serialization -----------------------------------------------------

test_that("person_triples() supports FOAF", {
  p <- utils::person(
    given = "Jane", family = "Smith",
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(p, ontology = "foaf")

  expect_length(triples, 2)
  expect_true(any(grepl(
    "<http://xmlns.com/foaf/0.1/Person>", triples,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "<http://xmlns.com/foaf/0.1/name>", triples,
    fixed = TRUE
  )))
  expect_false(any(grepl("prov#Agent", triples, fixed = TRUE)))
})


# Authority identifiers --------------------------------------------------

test_that("person_triples() resolves authority identifiers", {
  identifiers <- list(
    ORCID = c(
      "0000-0002-1825-0097",
      "https://orcid.org/0000-0002-1825-0097"
    ),
    ISNI = c(
      "0000000121032683",
      "https://isni.org/isni/0000000121032683"
    ),
    VIAF = c(
      "123456",
      "http://viaf.org/viaf/123456"
    ),
    Wikidata = c(
      "Q12345",
      "https://www.wikidata.org/wiki/Q12345"
    )
  )

  for (key in names(identifiers)) {
    p <- utils::person(
      given = "Jane", family = "Smith",
      comment = setNames(identifiers[[key]][1], key)
    )

    triples <- person_triples(p)
    expected <- paste0("<", identifiers[[key]][2], ">")

    expect_length(triples, 2)
    expect_true(all(startsWith(triples, paste0(expected, " "))))
  }
})


# Explicit subjects ------------------------------------------------------

test_that("person_triples() accepts and prioritizes explicit subjects", {
  p <- utils::person(given = "Jane", family = "Smith")

  triples <- person_triples(p, subject = "_:person1")
  expect_length(triples, 2)
  expect_true(all(startsWith(triples, "_:person1 ")))

  p_orcid <- utils::person(
    given = "Jane", family = "Smith",
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(
    p_orcid,
    subject = iri("https://example.org/person/jane")
  )

  expect_true(all(startsWith(
    triples, "<https://example.org/person/jane> "
  )))
  expect_false(any(grepl("orcid.org", triples, fixed = TRUE)))
})


# Name escaping ----------------------------------------------------------

test_that("person_triples() escapes names correctly", {
  p <- utils::person(
    given = 'Jane "J."',
    family = "Smith",
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(p)
  expect_true(any(grepl(
    'Jane \\"J.\\" Smith', triples,
    fixed = TRUE
  )))
})


# Role independence ------------------------------------------------------

test_that("person_triples() does not serialize contributor roles", {
  p <- utils::person(
    given = "Jane", family = "Smith",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(p)

  expect_length(triples, 2)
  expect_false(any(grepl("creator", triples, fixed = TRUE)))
  expect_false(any(grepl("contributor", triples, fixed = TRUE)))
})


# Input validation -------------------------------------------------------

test_that("person_triples() validates its arguments", {
  p <- utils::person(given = "Jane", family = "Smith")

  expect_error(person_triples("Jane Smith"), "utils::person")
  expect_error(person_triples(p), "subject is required")
  expect_error(person_triples(p, ontology = "invalid"), "arg")

  # Invalid RDF subjects
  expect_error(person_triples(p, subject = "Jane Smith"))
  expect_error(person_triples(p, subject = 42L))
  expect_error(person_triples(p, subject = character(0)))
  expect_error(person_triples(p, subject = c("_:p1", "_:p2")))
})




# Missing names ----------------------------------------------------------

test_that("person_triples() omits unavailable names", {
  # A person with an identifier but no given or family name
  p <- utils::person(
    email = "anonymous@example.org",
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(p)
  expect_length(triples, 1)
  expect_true(any(grepl("prov#Agent", triples, fixed = TRUE)))

  # Family name only
  p <- utils::person(family = "Smith")
  triples <- person_triples(p, subject = "_:person2")

  expect_length(triples, 2)
  expect_true(any(grepl('"Smith"', triples, fixed = TRUE)))

  # Given name only
  p <- utils::person(given = "Jane")
  triples <- person_triples(p, subject = "_:person3")

  expect_length(triples, 2)
  expect_true(any(grepl('"Jane"', triples, fixed = TRUE)))
})


# Role and resource separation -------------------------------------------

test_that("person_triples() does not serialize resource relationships", {
  p <- utils::person(
    given = "Jane", family = "Smith", role = c("aut", "cre"),
    comment = c(ORCID = "0000-0002-1825-0097")
  )

  triples <- person_triples(p)

  expect_length(triples, 2)
  expect_false(any(grepl("wasAttributedTo", triples, fixed = TRUE)))
  expect_false(any(grepl("creator", triples, fixed = TRUE)))
  expect_false("resource" %in% names(formals(person_triples)))
})
