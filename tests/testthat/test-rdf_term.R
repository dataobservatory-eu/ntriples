# IRI and RDF term recognition -------------------------------------------

test_that("rdf_term() recognizes IRIs and RDF syntax", {
  # Absolute HTTP(S) IRIs
  expect_identical(rdf_term("https://example.org/id"), "<https://example.org/id>")
  expect_identical(rdf_term("http://example.org/id"), "<http://example.org/id>")
  expect_identical(rdf_term("<https://example.org/id>"), "<https://example.org/id>")

  # Explicit IRI objects, including non-HTTP schemes
  expect_identical(rdf_term(iri("https://example.org/id")), "<https://example.org/id>")
  expect_identical(rdf_term(iri("urn:uuid:12345678")), "<urn:uuid:12345678>")
  expect_identical(rdf_term(iri("doi:10.1234/example")), "<doi:10.1234/example>")

  # Blank nodes
  expect_identical(rdf_term("_:agent1"), "_:agent1")

  # RDF type shorthand
  rdf_type <- "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
  expect_identical(rdf_term("a"), rdf_type)
})


# Character literals -----------------------------------------------------

test_that("rdf_term() serializes character literals", {
  xsd <- "http://www.w3.org/2001/XMLSchema#"

  # Ordinary text
  expect_identical(rdf_term("Example"), paste0('"Example"^^<', xsd, "string>"))
  expect_identical(
    rdf_term("hello@example.org"),
    paste0('"hello@example.org"^^<', xsd, "string>")
  )

  # Non-HTTP identifier strings remain literals without iri()
  expect_identical(
    rdf_term("urn:uuid:12345678"),
    paste0('"urn:uuid:12345678"^^<', xsd, "string>")
  )

  # URL-like prose must remain a literal
  expect_identical(
    rdf_term("https://example.org is our website"),
    paste0('"https://example.org is our website"^^<', xsd, "string>")
  )

  # Language-tagged literals
  expect_identical(rdf_term('"Bonjour"@fr'), '"Bonjour"@fr')
  expect_identical(rdf_term('"Hallo"@nl-BE'), '"Hallo"@nl-BE')

  # Preformatted typed literal
  literal <- paste0('"42"^^<', xsd, "integer>")
  expect_identical(rdf_term(literal), literal)
})


# Typed scalar values ----------------------------------------------------

test_that("rdf_term() serializes R scalar types", {
  xsd <- "http://www.w3.org/2001/XMLSchema#"

  # Numeric
  expect_identical(rdf_term(42L), paste0('"42"^^<', xsd, "integer>"))
  expect_identical(rdf_term(3.5), paste0('"3.5"^^<', xsd, "double>"))

  # Logical
  expect_identical(rdf_term(TRUE), paste0('"true"^^<', xsd, "boolean>"))
  expect_identical(rdf_term(FALSE), paste0('"false"^^<', xsd, "boolean>"))

  # Date
  expect_identical(
    rdf_term(as.Date("2026-11-11")),
    paste0('"2026-11-11"^^<', xsd, "date>")
  )
})


# Date-time serialization ------------------------------------------------

test_that("rdf_term() delegates POSIXct formatting", {
  xsd <- "^^<http://www.w3.org/2001/XMLSchema#dateTime>"

  # UTC with explicit offset
  utc <- as.POSIXct("2026-11-11 12:30:00", tz = "UTC")
  expect_identical(rdf_term(utc), paste0('"2026-11-11T12:30:00+00:00"', xsd))

  # Local time with daylight saving
  local <- as.POSIXct("2026-07-01 12:30:00", tz = "Europe/Budapest")
  expect_identical(rdf_term(local), paste0('"2026-07-01T12:30:00+02:00"', xsd))

  # Fractional seconds
  fractional <- utc + 0.125
  expect_identical(
    rdf_term(fractional),
    paste0('"2026-11-11T12:30:00.125+00:00"', xsd)
  )
})


# Literal escaping -------------------------------------------------------

test_that("rdf_term() escapes special characters", {
  xsd <- "^^<http://www.w3.org/2001/XMLSchema#string>"

  # Quotation marks
  expect_identical(
    rdf_term('A "quoted" title'),
    paste0('"A \\"quoted\\" title"', xsd)
  )

  # Newlines and tabs
  expect_identical(
    rdf_term("First\nSecond"),
    paste0('"First\\nSecond"', xsd)
  )
  expect_identical(
    rdf_term("A\tB"),
    paste0('"A\\tB"', xsd)
  )

  # Windows paths
  expect_identical(
    rdf_term("C:\\Users\\Documents"),
    paste0('"C:\\\\Users\\\\Documents"', xsd)
  )
})


# Input validation -------------------------------------------------------

test_that("rdf_term() rejects unsupported inputs", {
  # Missing and empty values
  expect_error(rdf_term(NULL), "scalar")
  expect_error(rdf_term(character(0)), "scalar")
  expect_error(rdf_term(NA_character_), "scalar")
  expect_error(rdf_term(NA_integer_), "scalar")

  # Non-scalar values
  expect_error(rdf_term(c("one", "two")), "scalar")
  expect_error(rdf_term(1:3), "scalar")

  # Structured values
  expect_error(rdf_term(list("one")), "scalar")
  expect_error(rdf_term(data.frame(x = 1)), "scalar")

  # Person objects belong to person_triples()
  p <- utils::person(given = "Jane", family = "Smith")
  expect_error(rdf_term(p), "scalar")

  # Unsupported scalar types
  expect_error(
    rdf_term(factor("example")),
    "cannot be serialized unambiguously"
  )

  # Factors require explicit conversion
  x <- factor("medium", levels = c("low", "medium", "high"))

  expect_error(rdf_term(x), "cannot be serialized unambiguously")
  expect_identical(
    rdf_term(as.character(x)),
    '"medium"^^<http://www.w3.org/2001/XMLSchema#string>'
  )
  expect_identical(
    rdf_term(as.integer(x)),
    '"2"^^<http://www.w3.org/2001/XMLSchema#integer>'
  )
})
