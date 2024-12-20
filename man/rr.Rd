% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redundant.R
\name{rr}
\alias{rr}
\title{Alias for \code{contrast()}}
\usage{
rr(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=contrast]{contrast}}
  \describe{
    \item{\code{long}}{\verb{<character>} vector}
    \item{\code{short}}{\verb{<character>} vector}
  }}
}
\value{
\verb{<character>} vector
}
\description{
Alias for \code{contrast()}
}
\examples{
contrast(
   long = c("39550", "92083", "C5274"),
   short = c("3", "5", "M", "U"))

}
