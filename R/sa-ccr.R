ns_ead <- function(rc, pfe, ceiling = NULL) {
  # Max EAD for margined netting set is EAD were it an unmargined netting set
  pmax(1.4 * (rc + pfe), ceiling %||% 0)
}

ns_rc <- function(npv, haircut_collateral, threshold = 0, min_transfer = 0, im = 0) {
  pmax(npv - haircut_collateral, threshold + min_transfer - im, 0)
}

ns_haircut_collateral <- function(value, type, holding_period, min_holding_period) {
  res <- vector("numeric", length(value))
  neg <- value < 0
  res[!neg] <- value[!neg] *
    (1 - collateral_haircut(type[!neg], holding_period[!neg], min_holding_period))
  res[neg] <- value[neg] *
    (1 + collateral_haircut(type[neg], holding_period[neg]))
  res
}


ns_pfe <- function(aggregate_add_on, npv, haircut_collateral) {
  m <- pmin(1, 0.05 + 0.95 * exp((npv - haircut_collateral) / 1.9 / aggregate_add_on))
  m * aggregate_add_on
}
