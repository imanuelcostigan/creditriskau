#' CVA risk capital charge
#'
#' This method determines a portfolio's CVA capital charge in accordance with
#' APS 180, Att. A, para. 17. You supply three sets of arguments to this
#' function: counterparty (CP) exposures, CP exposure single name hedges and
#' index CVA hedges. The arguments to this function are vectorised so that each
#' element of CP exposure and single name hedge arguments relate to one
#' counterparty (or netting set if a counterparty has more than one netting set)
#' and each element of the macro CVA hedge arguments relate to one CDS index. If
#' a vector of length greater than one is supplied for \code{cp_rating}, the
#' multiple counterparty CVA capital charge is calculated. In this case, if the
#' \code{cp_hedge_wam} or \code{cva_hedge_weight} arguments are not \code{NULL},
#' hedging benefits accrue. The lengths of each of the CP exposures and single
#' name hedge arguments must be consistent (or zero in case of \code{df = NULL})
#' while the index CVA hedge arguments must have the same length (or zero in
#' case of \code{df = NULL}).
#'
#' @param cp_rating the CP's long term credit rating which are mapped to
#'   weighting factors in accordance with APS 180, Att. A, para. 18. The credit
#'   ratings are defined in APS 120, Att. F, para. 13. with unrated
#'   exposures mapped to credit rating 4. Must be a numeric vector with unrated
#'   exposure ratings set to `NA`.
#' @param cp_wam the weighted average maturity of derivative cash flows with the
#'   CP (\eqn{M}). Must be greater than zero.
#' @param cp_ead the exposure at default (\eqn{Exposure^total}) which must be no
#'   less than 0.0
#' @param cp_df the discount factor (\eqn{D}) which must be no more than 1.0 or
#'   \code{NULL}. Defaults to \code{NULL} which means that it is calculated from
#'   \code{cp_wam} per the prescribed formula.
#' @param cp_hedge_wam the maturity of single name CDS cash flows referencing
#'   the CPs (\eqn{M}). Must be greater than zero.
#' @param cp_hedge_notional notional amount of purchased single name CDS
#' @param cp_hedge_df the discount factor (\eqn{D}) which must be no more than
#'   1.0 or \code{NULL}. Defaults to \code{NULL} which means that it is
#'   calculated from \code{cp_hedge_wam} per the prescribed formula.
#' @param cva_hedge_rating the index CDS counterparty's long term external
#'   credit rating. See `cp_rating` for further details
#' @param cva_hedge_wam maturity in years of the index CDS
#' @param cva_hedge_notional the notional amount of the index CDS
#' @param cva_hedge_df the discount factor (\eqn{D}) which must be no more than
#'   1.0 or \code{NULL}. Defaults to \code{NULL} which means that it is
#'   calculated from \code{wam} per the prescribed formula.
#' @return the CVA capital charge for the counterparty or an error if an invalid
#'   argument values are supplied.
#' @export
#' @examples
#' # Single counterparty
#' cva_capital(1, 2.3, 1e6, 0.8)
#' # Multiple counterparties
#' cva_capital(c(1, 2), c(1, 0.5), c(100, 8000))
#' # Multiple counterparties with hedges
#' cva_capital(c(2, 3), c(1, 0.5), c(100, 8000), NULL,
#'   c(1, 0.5), c(50, 1000), NULL, 1, 1, 2000, NULL)

cva_capital <- function(cp_rating, cp_wam, cp_ead, cp_df = NULL,
  cp_hedge_wam = NULL, cp_hedge_notional = NULL, cp_hedge_df = NULL,
  cva_hedge_rating = NULL, cva_hedge_wam = NULL, cva_hedge_notional = NULL,
  cva_hedge_df = NULL) {

  assertthat::assert_that(
    all(cp_rating %in% names(cva_rts_to_wts)),
    all(cva_hedge_rating %in% names(cva_rts_to_wts)),
    all(cp_wam > 0), all(cp_ead >= 0),
    is.null(cp_df) || all(cp_df >= 0) && all(cp_df <= 1),
    are_equal_lengths(cp_rating, cp_wam, cp_ead),
    is.null(cp_df) || are_equal_lengths(cp_ead, cp_df),
    is.null(cp_hedge_wam) ||
      are_equal_lengths(cp_wam, cp_hedge_wam, cp_hedge_notional),
    is.null(cp_hedge_df) || are_equal_lengths(cp_hedge_df, cp_hedge_notional),
    is.null(cva_hedge_rating) ||
      are_equal_lengths(cva_hedge_rating, cva_hedge_wam, cva_hedge_notional),
    is.null(cva_hedge_df) || are_equal_lengths(cva_hedge_df, cva_hedge_rating)
  )

  weight <- cp_rating %=>% cva_rts_to_wts
  cp_df <- cp_df %||% discount_factor(cp_wam)
  if (length(weight) == 1) {
    # Att. A, para. 17(a). Multiplying by 2.33 below
    res <- weight * cp_wam * cp_df * cp_ead
  } else {
    if (is.null(cp_hedge_wam) && is.null(cva_hedge_wam)) {
      # Att. A, para. 17(b). Multiplying by 2.33 below
      k <- weight * cp_wam * cp_df * cp_ead
      res <- sqrt(0.25 * sum(k) ^ 2 + 0.75 * sum(k ^ 2))
    } else {
      # Att. A, para. 17(c). Multiplying by 2.33 below
      if (!is.null(cp_hedge_wam)) {
        cp_hedge_df <- cp_hedge_df %||% discount_factor(cp_hedge_wam)
        cp_hedge_offset <- cp_hedge_wam * cp_hedge_df * cp_hedge_notional
      } else {
        cp_hedge_offset <- 0
      }
      k <- weight * (cp_wam * cp_df * cp_ead - cp_hedge_offset)
      if (!is.null(cva_hedge_rating)) {
        cva_hedge_weight <- cva_hedge_rating %=>% cva_rts_to_wts
        cva_hedge_df <- cva_hedge_df %||% discount_factor(cva_hedge_wam)
        cva_hedge_offset <- sum(cva_hedge_weight * cva_hedge_wam *
            cva_hedge_df * cva_hedge_notional)
      } else {
        cva_hedge_offset <- 0
      }
      res <- sqrt((0.5 * sum(k) - cva_hedge_offset) ^ 2 + 0.75 * sum(k ^ 2))
    }
  }
  2.33 * res
}
