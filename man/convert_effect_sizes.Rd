% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effect_size_transformations.R
\name{convert_effect_sizes}
\alias{convert_effect_sizes}
\title{Convert effect sizes to common metric (r)}
\usage{
convert_effect_sizes(es_values, es_types, quiet = FALSE)
}
\arguments{
\item{es_values}{Numeric vector of effect sizes}

\item{es_types}{Character vector of effect size types (types/wordings that are not supported are flagged in warning)}

\item{quiet}{Logical. Should dataset warnings (unknown effect sizes and values not convertible to numeric)
and status messages be suppressed?}
}
\value{
Numeric vector of effect sizes in common metric (r)
}
\description{
Takes vectors of effect sizes and their types and converts them to a common metric (r).
It also converts test statistics, specifically \emph{t} and \emph{F} with 1 degree of in the numerator, to \emph{r}.
Other test statistics cannot be consistently converted, so are returned as \code{NA}.
}
