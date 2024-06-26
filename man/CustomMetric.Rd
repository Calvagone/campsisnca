% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_metric.R
\name{CustomMetric}
\alias{CustomMetric}
\title{Custom metric (input data as time and value vectors).}
\usage{
CustomMetric(
  x = NULL,
  variable = NULL,
  fun,
  name = NULL,
  unit = NULL,
  categorical = FALSE,
  stat_display = getStatDisplayDefault(categorical),
  digits = NULL
)
}
\arguments{
\item{x}{Campsis/NONMEM dataframe}

\item{variable}{dependent variable}

\item{fun}{any custom function with exactly 2 arguments: time and value}

\item{name}{custom metric name (will be exported into table headers)}

\item{unit}{metric unit (will be exported into table headers if provided)}

\item{categorical}{categorical endpoint, logical}

\item{stat_display}{statistics display, default is '\{median\} [\{p5\}-\{p95\}]' for continuous data or '\{n\} / \{N\} (\{p\}\%)' for categorical data}

\item{digits}{rounding digits definitions (integer, function, purrr-style lambda function or list of these, 1 item per statistic), see README}
}
\description{
Custom metric (input data as time and value vectors).
}
