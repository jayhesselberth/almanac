# Built on top of a `cacher`, this adjusted version will apply
# an adjustment to the events that `cacher` returns whenever those events
# fall on the events that `adjustment_cacher` returns. This is appropriate
# for representing corporate holidays, where for example, Christmas is normally
# December 25th, unless it falls on a weekend, in which case it is rolled
# to the nearest workday.

# ------------------------------------------------------------------------------

radjusted <- function(cacher, adjuster, adjustment) {
  new_radjusted(cacher, adjuster, adjustment)
}

# ------------------------------------------------------------------------------

new_radjusted <- function(cacher, adjuster, adjustment) {
  validate_cacher(cacher, "cacher")
  validate_cacher(adjuster, "adjuster")
  validate_adjustment(adjustment, "adjustment")

  cache <- cache_radjusted$new(cacher, adjuster, adjustment)

  data <- list(
    cacher = cacher,
    adjuster = adjuster,
    adjustment = adjustment,
    cache = cache
  )

  new_cacher(data, "radjusted")
}