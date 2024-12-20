% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recombinators.R
\name{recombine2}
\alias{recombine2}
\title{Recombine Length 2}
\usage{
recombine2(x)
}
\arguments{
\item{x}{\verb{<list>} of character vectors}
}
\value{
\verb{<character>} string
}
\description{
Recombine Length 2
}
\examples{
random_hcpcs() |>
   split_lengths() |>
   remove_redundant() |>
   split_first() |>
   process_groups() |>
   reduce_groups() |>
   recombine2()

}
