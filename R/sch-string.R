# TODO Handle RDATE/EXDATE!

#' Convert a schedule into an iCalendar RRULE string
#'
#' `sch_as_ical_string()` converts a schedule of recurrence rules into a list
#' of character vectors of size 2. There are as many elements in the
#'
#' @export
#' @examples
#' rule <- daily(since = "2000-01-01", until = "2001-01-01") %>%
#'   recur_on_wday("Tuesday", nth = 3)
#'
#' # Turning a single rule into a iCalendar recurrence rule string
#' sch_as_ical_string(rule)
#'
#' # Multiple rules in a schedule
#' rule2 <- yearly() %>%
#'   recur_on_yday(200)
#'
#' sch <- schedule() %>%
#'   sch_rrule(rule) %>%
#'   sch_rrule(rule2) %>%
#'   sch_rdate(c("2019-01-01", "2019-01-02"))
#'
#' sch_as_ical_string(sch)
sch_as_ical_string <- function(schedule) {
  schedule <- as_schedule(schedule)
  recurrences <- schedule$recurrences

  call <- sch_as_ical_string_call(recurrences)

  out <- almanac_global_context$call(call)

  if (length(out) != 1L) {
    abort("Internal error: Length of JS result is not 1.")
  }

  out <- strsplit(out, "\n", fixed = TRUE)[[1L]]

  size <- length(out)

  starts <- seq_by(1L, size - 1L, by = 2L)
  stops <- seq_by(2L, size, by = 2L)

  map2(starts, stops, function(start, stop) out[start:stop])
}

sch_as_ical_string_call <- function(recurrences) {
  body <- as_js_call_body(recurrences)

  glue2("
    function() {
      [[body]]
      return ruleset.toString()
    }
  ")
}

# rlang::seq2() with a `by` argument
seq_by <- function(from, to, by) {
  if (length(from) != 1) {
    abort("`from` must be length one")
  }
  if (length(to) != 1) {
    abort("`to` must be length one")
  }
  if (from > to) {
    integer()
  }
  else {
    seq.int(from = from, to = to, by = by)
  }
}
