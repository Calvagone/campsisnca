get_stat_display_default <- function(categorical = FALSE) {
  if (categorical) {
    return("{n} / {N} ({p}%)")
  } else {
    return("{median} ({p5}\U2013{p95})") # En dash
  }
}
