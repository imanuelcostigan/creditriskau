% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irb.R
\name{retail_capital}
\alias{retail_capital}
\title{Retail exposure capital requirements}
\usage{
retail_capital(pd, lgd, sub_class, correlation = NA)
}
\arguments{
\item{pd}{the probability of default expressed as a decimal between 0 and 1
(exclusive). It is assumed that these are already floored to 0.0003 for
corporate and bank exposures (B.2).}

\item{lgd}{the loss given default expressed as a decimal. It is assumed that
any applicable floors have already been applied.}

\item{sub_class}{the sub-class of the retail IRB asset class. Can be one of
the following: \code{mortgage} (including small business exposures secured by
residential mortgage), \code{qrr} (qualified revolving retail) and \code{other} (e.g.
personal lending and small business exposures not secured by residential
mortgage)}

\item{correlation}{the asset correlation factor. Defaults to 15\%, 4\% and
the APS prescribed formula (APS 113, Att. C, para. 37) for \code{mortgage},
\code{qrr} and \code{other} aub-asset classes respectively. These can be overridden
by supplying a numeric vector with alternate values and must be the same
length as \code{sub_class}. The latter is necessary as there is no longer a
one-to-one mapping of correlation to sub-asset class as a result of
\href{http://www.apra.gov.au/MediaReleases/Pages/15_19.aspx}{changes announced in July 2015}.}
}
\value{
a vector of capital requirement ratios
}
\description{
This calculates the capital requirements of retail exposures including
residential mortgages, qualifying revolving retail and others. All inputs are
vectors and whose values are recycled if necessary.
}
\examples{
retail_capital(0.04, 0.20, "mortgage")
retail_capital(0.04, 0.20, "qrr")
retail_capital(0.04, 0.20, "other")
retail_capital(0.04, 0.20, "mortgage", 0.25)
retail_capital(0.04, 0.20 , c("mortgage", "qrr"), c(0.25, NA))
}
\references{
\href{https://www.legislation.gov.au/Details/F2012L02329}{APS 113}
}
\seealso{
Other APS113 functions: \code{\link{non_retail_capital}},
  \code{\link{slotting_capital}}
}
