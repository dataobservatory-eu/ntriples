# Basic N-Triple construction --------------------------------------------

test_that("n_triple() serializes IRIs and ordinary literals", {
  # Common RDF terms
  s <- "https://example.org/resource/1"
  p <- "http://www.w3.org/2000/01/rdf-schema#label"

  expect_identical(
    n_triple(s, p, "Example"),
    paste0(
      "<", s, "> <", p, '> "Example"^^',
      "<http://www.w3.org/2001/XMLSchema#string> ."
    )
  )

  # IRI objects
  expect_identical(
    n_triple(s, p, "https://example.org/resource/2"),
    paste0("<", s, "> <", p, "> <https://example.org/resource/2> .")
  )

  # RDF type shorthand
  expect_identical(
    n_triple(s, "a", "http://www.w3.org/ns/prov#Entity"),
    paste0(
      "<", s, "> ",
      "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ",
      "<http://www.w3.org/ns/prov#Entity> ."
    )
  )
})


# Typed values -----------------------------------------------------------

test_that("n_triple() serializes R scalar values", {
  s <- "https://example.org/resource/1"
  p <- "https://example.org/value"
  prefix <- paste0("<", s, "> <", p, "> ")

  # Integer and double
  expect_identical(
    n_triple(s, p, 42L),
    paste0(prefix, '"42"^^<http://www.w3.org/2001/XMLSchema#integer> .')
  )
  expect_identical(
    n_triple(s, p, 3.5),
    paste0(prefix, '"3.5"^^<http://www.w3.org/2001/XMLSchema#double> .')
  )

  # Date
  expect_identical(
    n_triple(s, p, as.Date("2026-11-11")),
    paste0(
      prefix, '"2026-11-11"^^',
      "<http://www.w3.org/2001/XMLSchema#date> ."
    )
  )
})


# Preformatted RDF terms -------------------------------------------------

test_that("n_triple() preserves preformatted RDF terms", {
  s <- "https://example.org/resource/1"
  p <- "https://example.org/label"
  prefix <- paste0("<", s, "> <", p, "> ")

  # Typed literal
  literal <- '"42"^^<http://www.w3.org/2001/XMLSchema#integer>'
  expect_identical(n_triple(s, p, literal), paste0(prefix, literal, " ."))

  # Explicit IRI
  object <- "<https://example.org/resource/2>"
  expect_identical(n_triple(s, p, object), paste0(prefix, object, " ."))
})


# Input validation -------------------------------------------------------

test_that("n_triple() rejects unsupported input shapes", {
  s <- "https://example.org/resource/1"
  p <- "https://example.org/value"

  # Multiple objects
  expect_error(n_triple(s, p, c("one", "two")), "scalar")

  # Multiple subjects and predicates
  expect_error(n_triple(c(s, s), p, "value"), "scalar")
  expect_error(n_triple(s, c(p, p), "value"), "scalar")

  # Structured objects
  expect_error(n_triple(s, p, data.frame(x = 1)), "scalar")
})


# RDF compliance: intended behaviour ------------------------------------
# These tests document requirements not yet met by the implementation.

test_that("n_triple() escapes special characters in literals", {
  s <- "https://example.org/resource/1"
  p <- "https://example.org/label"
  prefix <- paste0("<", s, "> <", p, "> ")

  # Quotation marks
  expect_identical(
    n_triple(s, p, 'A "quoted" title'),
    paste0(
      prefix, '"A \\"quoted\\" title"^^',
      "<http://www.w3.org/2001/XMLSchema#string> ."
    )
  )

  # Newlines
  expect_identical(
    n_triple(s, p, "First\nSecond"),
    paste0(
      prefix, '"First\\nSecond"^^',
      "<http://www.w3.org/2001/XMLSchema#string> ."
    )
  )
})

test_that("n_triple() distinguishes literal strings from IRIs", {
  s <- "https://example.org/resource/1"
  p <- "https://example.org/description"

  # A URL appearing in prose is still a literal
  expect_identical(
    n_triple(s, p, "https://example.org is our website"),
    paste0(
      "<", s, "> <", p, "> ",
      '"https://example.org is our website"^^',
      "<http://www.w3.org/2001/XMLSchema#string> ."
    )
  )
})

test_that("n_triple() preserves language-tagged literals", {
  s <- "https://example.org/resource/1"
  p <- "http://www.w3.org/2000/01/rdf-schema#label"

  expect_identical(
    n_triple(s, p, '"Bonjour"@fr'),
    paste0("<", s, "> <", p, '> "Bonjour"@fr .')
  )
})

# Windows path escaping --------------------------------------------------

test_that("n_triple() handles Windows backslashes", {
  expect_identical(
    escape_literal("C:\\Users\\Documents"),
    "C:\\\\Users\\\\Documents"
  )
})


# RDF position validation ------------------------------------------------

test_that("n_triple() accepts valid RDF term positions", {
  # IRI subject, predicate and literal object
  triple <- n_triple("https://example.org/s", "https://example.org/p", "Value")
  expect_true(startsWith(triple, "<https://example.org/s> "))
  expect_true(grepl('"Value"', triple, fixed = TRUE))

  # Blank-node subject and RDF type shorthand
  triple <- n_triple("_:agent1", "a", "http://www.w3.org/ns/prov#Agent")
  expect_true(startsWith(triple, "_:agent1 "))
  expect_true(grepl("rdf-syntax-ns#type", triple, fixed = TRUE))

  # Explicit non-HTTP IRIs
  triple <- n_triple(iri("urn:uuid:123"), iri("urn:example:predicate"), 42L)
  expect_true(startsWith(triple, "<urn:uuid:123> "))
  expect_true(grepl("<urn:example:predicate>", triple, fixed = TRUE))
})


# Invalid RDF subjects ---------------------------------------------------

test_that("n_triple() rejects invalid subjects", {
  p <- "https://example.org/p"

  # Character and numeric literals
  expect_error(n_triple("Plain text", p, "Value"), "subject")
  expect_error(n_triple(42L, p, "Value"), "subject")
  expect_error(n_triple(TRUE, p, "Value"), "subject")

  # Invalid or missing identifiers
  expect_error(n_triple("relative/path", p, "Value"), "subject")
  expect_error(n_triple(NA_character_, p, "Value"), "scalar")
})


# Invalid RDF predicates -------------------------------------------------

test_that("n_triple() rejects invalid predicates", {
  s <- "https://example.org/s"

  # Character and numeric literals
  expect_error(n_triple(s, "Plain text", "Value"), "predicate")
  expect_error(n_triple(s, 42L, "Value"), "predicate")

  # Blank nodes cannot be predicates
  expect_error(n_triple(s, "_:predicate1", "Value"), "predicate")

  # Relative identifiers are not predicates
  expect_error(n_triple(s, "relative/path", "Value"), "predicate")
})


# RDF object flexibility -------------------------------------------------

test_that("n_triple() accepts valid RDF objects", {
  s <- "https://example.org/s"
  p <- "https://example.org/p"

  # IRI and blank-node objects
  expect_true(grepl("<https://example.org/o>", n_triple(s, p, iri("https://example.org/o")), fixed = TRUE))
  expect_true(grepl("_:object1", n_triple(s, p, "_:object1"), fixed = TRUE))

  # Typed literals
  expect_true(grepl('"42"^^', n_triple(s, p, 42L), fixed = TRUE))
  expect_true(grepl('"true"^^', n_triple(s, p, TRUE), fixed = TRUE))

  # Language-tagged literals
  expect_true(grepl('"Bonjour"@fr', n_triple(s, p, '"Bonjour"@fr'), fixed = TRUE))
})

