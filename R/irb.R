#' Non-retail exposure capital requirements
#'
#' This calculates the capital requirements of corporate, bank and sovereign
#' exposures that are not specialised lending exposures subject to the slotting
#' approach. All inputs are vectors and whose values are recycled if necessary.
#'
#' @param pd the probability of default expressed as a decimal between 0 and 1
#'   (exclusive). It is assumed that these are already floored to 0.0003 for
#'   corporate and bank exposures (B.2).
#' @param lgd the loss given default expressed as a decimal. It is assumed that
#'   any applicable floors have already been applied.
#' @param maturity the effective maturity expressed in years. It is assumed that
#'   any applicable floors or ceilings have already been applied.
#' @param size the size of the consolidated group to which an obligor belongs
#'   expressed in millions of dollars of sales
#' @param is_riskier_fi `TRUE` if an obligor is a regulated financial
#'   institution with total assets greater than A$100bn or in an unregulated
#'   financial institution. Otherwise is `FALSE`. See 113.B.77(a) and
#'   113.B.77(b)
#' @return a vector of capital requirement ratios subject to a floor of zero.
#' Note this assumes a Prudential Capital Ratio of 0.08
#' @examples
#' non_retail_capital(0.05, 0.45, 3, 100, FALSE)
#' non_retail_capital(c(0.05, 0.70), 0.45, 3, 100, FALSE)
#' @references
#' [APS 113](https://www.legislation.gov.au/Details/F2012L02329)
#' @export
non_retail_capital <- function (pd, lgd, size, maturity, is_riskier_fi) {
  # Correlation factor, B.77
  R <- 0.24 - 0.12 * (1 - exp(-50 * pd)) / (1 - exp(-50)) -
  # Size adjustment, B.81
    0.04 * (1 - (size - 5) / 45)
  # Scale up for regulated FIs > 100bn assets or for unregulated FIs, B.77
  R[is_riskier_fi] <- 1.25 * R[is_riskier_fi]
  # Maturity adjustment, B.77
  b <- (0.11852 - 0.05478 * log(pd)) ^ 2
  # Capital calculation
  q <- (stats::qnorm(pd) + sqrt(R) * stats::qnorm(0.999)) / sqrt(1 - R)
  maturity_adjustment <- (1 + (maturity - 2.5) * b) / (1 - 1.5 * b)
  pmax(0, ul(pd, lgd, R, maturity_adjustment))
}

#' Retail exposure capital requirements
#'
#' This calculates the capital requirements of retail exposures including
#' residential mortgages, qualifying revolving retail and SME Retail (other).
#' All inputs are vectors and whose values are recycled if necessary.
#'
#' @inherit non_retail_capital params references
#' @param sub_class the sub-class of the retail IRB asset class. Can be one of
#'   the following: `mortgage`, `qrr` (qualified revolving retail) and `other`
#'   (e.g. personal lending and small business exposures)
#' @return a vector of capital requirement ratios
#' @examples
#' retail_capital(0.04, 0.20, "mortgage")
#' retail_capital(0.04, 0.20, "qrr")
#' retail_capital(0.04, 0.20, "other")
#' @export
retail_capital <- function(pd, lgd, sub_class) {
  assertthat::assert_that(all(sub_class %in% c("mortgage", "qrr", "other")))
  R <- vector("numeric", max(length(pd), length(lgd), length(sub_class)))
  is_mortgage <- sub_class == "mortgage"
  is_qrr <- sub_class == "qrr"
  is_other <- sub_class == "other"
  R[is_mortgage] <- 0.15
  R[is_qrr] <- 0.04
  R[is_other] <- 0.16 - 0.13 * (1 - exp(-35 * pd[is_other])) / (1 - exp(-35))
  ul(pd, lgd, R, NULL)
}


#' Slotted specialised lending exposure capital
#'
#' This calculates the capital requirements of specialised lending exposures
#' subject to slotting.
#'
#' @inherit non_retail_capital return
#' @param category the slotting category must be one of the following values:
#' `strong`, `good`, `satisfactory`, `weak`, `default`
#' @examples
#' slotting_capital(c("strong", "weak"))
#' @export
slotting_capital <- function(category) {
  rw <- c("strong" = 0.7, "good" = 0.9, "satisfactory" = 1.15, "weak" = 2.5,
    "default" = 0)
  unname(rw[match(category, names(rw))]) * 0.08
}

ul <- function(pd, lgd, R, maturity_adjustment = NULL) {
  q <- (stats::qnorm(pd) + sqrt(R) * stats::qnorm(0.999)) / sqrt(1 - R)
  (stats::pnorm(q) - pd) * lgd * (maturity_adjustment %||% 1)
}

