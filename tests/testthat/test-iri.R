# IRI construction -------------------------------------------------------

test_that("iri() constructs absolute IRIs", {
  # HTTP and HTTPS
  expect_identical(
    unclass(iri("http://one.example/subject1")),
    "http://one.example/subject1"
  )
  expect_identical(
    unclass(iri("https://example.org/id")),
    "https://example.org/id"
  )

  # Other absolute IRI schemes
  expect_identical(
    unclass(iri("urn:isbn:9780123456789")),
    "urn:isbn:9780123456789"
  )
  expect_identical(
    unclass(iri("mailto:author@example.org")),
    "mailto:author@example.org"
  )

  # Class and underlying type
  expect_s3_class(iri("https://example.org/id"), "iri")
  expect_true(is.character(iri("https://example.org/id")))
})


# Vectorized construction ------------------------------------------------

test_that("iri() handles character vectors", {
  x <- c("https://example.org/1", "https://example.org/2")

  # Preserve values and order
  expect_identical(unclass(iri(x)), x)
  expect_length(iri(x), 2)

  # Empty vector
  expect_length(iri(character(0)), 0)

  # Validate every element, not just the first
  expect_error(iri(c(x[1], "not-an-iri")), "scheme")
  expect_error(iri(c(x[1], "https://example.org/my resource")), "forbidden")
})


# IRI syntax validation --------------------------------------------------

test_that("iri() rejects invalid IRI syntax", {
  # Missing or invalid scheme
  expect_error(iri("example.org/id"), "scheme")
  expect_error(iri("/relative/path"), "scheme")
  expect_error(iri(""), "scheme")
  expect_error(iri("1http://example.org"), "scheme")

  # Forbidden characters
  expect_error(iri("https://example.org/my resource"), "forbidden")
  expect_error(iri("https://example.org/<id>"), "forbidden")
  expect_error(iri('https://example.org/"id"'), "forbidden")
  expect_error(iri("https://example.org/a\nb"), "forbidden")
  expect_error(iri("https://example.org/a\\b"), "forbidden")
})


# Identifier preservation ------------------------------------------------

test_that("iri() preserves identifiers without sanitization", {
  # Percent-encoded characters
  x <- "https://example.org/my%20resource"
  expect_identical(unclass(iri(x)), x)

  # Unicode IRIs
  x <- "https://example.org/café"
  expect_identical(unclass(iri(x)), x)

  # No silent trimming
  expect_error(iri(" https://example.org/id"), "scheme|forbidden")
  expect_error(iri(" https://example.org/id"), "scheme|forbidden")

  # No silent removal of angle brackets
  expect_error(iri("<https://example.org/id>"), "scheme|forbidden")
})


# Input validation -------------------------------------------------------

test_that("iri() rejects unsupported inputs", {
  # Missing values
  expect_error(iri(NA_character_), "character")
  expect_error(iri(c("https://example.org/id", NA_character_)), "character")

  # Incorrect types
  expect_error(iri(42), "character")
  expect_error(iri(TRUE), "character")
  expect_error(iri(list("https://example.org/id")), "character")
})


# Integration with RDF serialization ------------------------------------

test_that("iri() integrates with rdf_term() and n_triple()", {
  # Explicit IRI object
  subject <- iri("http://one.example/subject1")
  expect_identical(rdf_term(subject), "<http://one.example/subject1>")

  # N-Triples statement
  triple <- n_triple(
    subject,
    iri("http://one.example/predicate1"),
    iri("http://one.example/object1")
  )

  expect_identical(
    triple,
    paste(
      "<http://one.example/subject1>",
      "<http://one.example/predicate1>",
      "<http://one.example/object1> ."
    )
  )
})
