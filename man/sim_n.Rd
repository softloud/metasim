% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_n.R
\name{sim_n}
\alias{sim_n}
\title{simulate meta-analysis sample sizes}
\usage{
sim_n(k = 3, min_n = 20, max_n = 200, prop = 0.5,
  prop_error = 0.1, wide = FALSE)
}
\arguments{
\item{k}{number of studies, defaults to 3}

\item{min_n}{lower bound for sample sizes, defaults to 20}

\item{max_n}{upper bound for sample sizes, defaults to 200}

\item{prop}{proportion of n we expect}

\item{prop_error}{what do we expect 90% of proportions to fall within?}

\item{wide}{if true, case and control sample sizes are provided in one row}
}
\description{
Generate a set of meta-analysis sample sizes. We assume that control and intervention sizes are mostly within some proportion \code{prop} of the same sample size.
}
