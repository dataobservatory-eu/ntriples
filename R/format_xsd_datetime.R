#' Format a POSIXct value as an XSD dateTime
#'
#' Formats a POSIXct instant as an ISO 8601 datetime suitable for an
#' `xsd:dateTime` RDF literal. Preserves fractional seconds and the local
#' UTC offset, or optionally normalizes the instant to UTC.
#'
#' Accepts base R POSIXct objects, including those created by `lubridate`.
#' Other datetime classes must be converted to POSIXct by the caller.
#' Timezone-free values must be assigned a timezone explicitly.
#'
#' @param x A non-missing scalar POSIXct value.
#' @param utc Logical; normalize to UTC if TRUE (default FALSE).
#' @return An ISO 8601 datetime string with a UTC offset or `Z`.
#' @keywords internal
#' @noRd
format_xsd_datetime <- function(x, utc = FALSE) {
  if (!inherits(x, "POSIXct") || length(x) != 1 || is.na(x)) {
    stop("format_xsd_datetime(): x must be a non-missing POSIXct scalar.")
  }

  if (!is.logical(utc) || length(utc) != 1 || is.na(utc)) {
    stop("format_xsd_datetime(): utc must be TRUE or FALSE.")
  }

  # Preserve fractional seconds, removing unnecessary trailing zeros
  format_seconds <- function(x, tz) {
    seconds <- format(x, "%OS6", tz = tz)
    seconds <- sub("0+$", "", seconds)
    seconds <- sub("\\.$", "", seconds)
    paste0(format(x, "%Y-%m-%dT%H:%M:", tz = tz), seconds)
  }

  if (utc) {
    return(paste0(format_seconds(x, "UTC"), "Z"))
  }

  tz <- attr(x, "tzone")
  if (length(tz) == 0 || !nzchar(tz[1])) {
    stop("format_xsd_datetime(): timezone is unspecified; use utc = TRUE.")
  }

  offset <- format(x, "%z", tz = tz[1])
  offset <- paste0(substr(offset, 1, 3), ":", substr(offset, 4, 5))

  paste0(format_seconds(x, tz[1]), offset)
}
