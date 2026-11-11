# Local timezone formatting ----------------------------------------------

test_that("format_xsd_datetime() preserves local timezone offsets", {
  # Winter: Central European Time
  winter <- as.POSIXct("2026-11-11 12:30:00",
    tz = "Europe/Budapest"
  )
  expect_identical(
    format_xsd_datetime(winter),
    "2026-11-11T12:30:00+01:00"
  )

  # Summer: Central European Summer Time
  summer <- as.POSIXct("2026-07-11 12:30:00",
    tz = "Europe/Budapest"
  )
  expect_identical(
    format_xsd_datetime(summer),
    "2026-07-11T12:30:00+02:00"
  )

  # UTC
  utc_time <- as.POSIXct("2026-11-11 12:30:00", tz = "UTC")
  expect_identical(
    format_xsd_datetime(utc_time),
    "2026-11-11T12:30:00+00:00"
  )
})


# UTC normalization ------------------------------------------------------

test_that("format_xsd_datetime() optionally normalizes to UTC", {
  # Winter and summer offsets
  winter <- as.POSIXct("2026-11-11 12:30:00",
    tz = "Europe/Budapest"
  )
  summer <- as.POSIXct("2026-07-11 12:30:00",
    tz = "Europe/Budapest"
  )

  expect_identical(
    format_xsd_datetime(winter, utc = TRUE),
    "2026-11-11T11:30:00Z"
  )
  expect_identical(
    format_xsd_datetime(summer, utc = TRUE),
    "2026-07-11T10:30:00Z"
  )

  # UTC normalization must not modify the original object
  original <- winter
  format_xsd_datetime(winter, utc = TRUE)
  expect_identical(winter, original)
})


# Daylight-saving transitions --------------------------------------------

test_that("format_xsd_datetime() handles daylight-saving transitions", {
  # Spring transition: 02:00 local time does not occur in Budapest
  before <- as.POSIXct("2026-03-29 00:30:00", tz = "UTC")
  after <- as.POSIXct("2026-03-29 01:30:00", tz = "UTC")
  attr(before, "tzone") <- "Europe/Budapest"
  attr(after, "tzone") <- "Europe/Budapest"

  expect_identical(
    format_xsd_datetime(before),
    "2026-03-29T01:30:00+01:00"
  )
  expect_identical(
    format_xsd_datetime(after),
    "2026-03-29T03:30:00+02:00"
  )

  # Both instants retain their UTC representations
  expect_identical(
    format_xsd_datetime(before, TRUE),
    "2026-03-29T00:30:00Z"
  )
  expect_identical(
    format_xsd_datetime(after, TRUE),
    "2026-03-29T01:30:00Z"
  )
})


# Input validation -------------------------------------------------------

test_that("format_xsd_datetime() validates its arguments", {
  x <- as.POSIXct("2026-11-11 12:30:00", tz = "UTC")

  # Unsupported types and missing values
  expect_error(format_xsd_datetime("2026-11-11"), "POSIXct")
  expect_error(format_xsd_datetime(as.Date("2026-11-11")), "POSIXct")
  expect_error(format_xsd_datetime(as.POSIXct(NA)), "POSIXct")
  expect_error(format_xsd_datetime(c(x, x)), "scalar")

  # Invalid utc argument
  expect_error(format_xsd_datetime(x, utc = NA), "utc")
  expect_error(format_xsd_datetime(x, utc = 1), "utc")
  expect_error(format_xsd_datetime(x, utc = c(TRUE, FALSE)), "utc")

  # Unspecified timezone
  unspecified <- as.POSIXct(0, origin = "1970-01-01", tz = "")
  attr(unspecified, "tzone") <- NULL
  expect_error(format_xsd_datetime(unspecified), "timezone")
  expect_identical(
    format_xsd_datetime(unspecified, utc = TRUE),
    "1970-01-01T00:00:00Z"
  )
})


# Fractional-second precision --------------------------------------------

test_that("format_xsd_datetime() preserves fractional seconds", {
  # Intended requirement: currently fails until formatter is extended
  x <- as.POSIXct("2026-11-11 12:30:00", tz = "UTC") + 0.125

  expect_identical(
    format_xsd_datetime(x, utc = TRUE),
    "2026-11-11T12:30:00.125Z"
  )

  # Local timezone with fractional seconds
  local <- as.POSIXct("2026-11-11 12:30:00",
    tz = "Europe/Budapest"
  ) + 0.125

  expect_identical(
    format_xsd_datetime(local),
    "2026-11-11T12:30:00.125+01:00"
  )

  # Whole seconds remain unchanged
  whole <- as.POSIXct("2026-11-11 12:30:00", tz = "UTC")

  expect_identical(
    format_xsd_datetime(whole, utc = TRUE),
    "2026-11-11T12:30:00Z"
  )
})
