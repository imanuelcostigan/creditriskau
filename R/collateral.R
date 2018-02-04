new_collateral <- function(asset_class, ...) {
  assertthat::assert_that(assertthat::is.string(asset_class))
  structure(list(...), class = c(asset_class, "collateral"))
}


# Debt collateral ---------------------------------------------------------


new_debt_collateral <- function(issuer, rating, term) {
  assertthat::assert_that(
    assertthat::is.string(issuer),
    assertthat::is.string(term),
    assertthat::is.count(rating)
  )
  new_collateral("debt_collateral",
    issuer = issuer, rating = rating, term = term)
}

validate_debt_collateral <- function(x) {
  assertthat::assert_that(
    x$issuer %in% c("sovereign", "securitisation", "other", NA),
    x$term %in% c("short", "medium", "long", NA),
    x$rating %in% c(1:4, NA)
  )
  x
}

debt_collateral <- function(issuer, rating, term) {
  validate_debt_collateral(new_debt_collateral(issuer, rating, term))
}

# Equity collateral -------------------------------------------------------


new_equity_collateral <- function(exchange) {
  assertthat::assert_that(assertthat::is.string(exchange))
  new_collateral("equity_collateral", exchange = exchange)
}


validate_equity_collateral <- function(x) {
  assertthat::assert_that(x$exchange %in% c("main", "other"))
  x
}

equity_collateral <- function(exchange) {
  validate_equity_collateral(new_equity_collateral(exchange))
}


# Cash collateral ---------------------------------------------------------


new_cash_collateral <- function(is_domestic) {
  assertthat::assert_that(assertthat::is.flag(is_domestic))
  new_collateral("cash_collateral", is_domestic = is_domestic)
}

cash_collateral <- function(is_domestic) {
  new_cash_collateral(is_domestic)
}


# Gold collateral ---------------------------------------------------------

gold_collateral <- function() {
  new_collateral("gold_collateral")
}


# Trust collateral --------------------------------------------------------

new_trust_collateral <- function(worst_collateral) {
  assertthat::assert_that(inherits(worst_collateral, "collateral"))
  new_collateral("trust_collateral", worst_collateral = worst_collateral)
}

trust_collateral <- function(worst_collateral) {
  new_trust_collateral(worst_collateral)
}



# Collateral methods ------------------------------------------------------

format.collateral <- function(x, ...) {
  paste0("<", paste0(class(x), collapse = "_"), ">")
}

print.collateral <- function(x, ...) {
  cat(format(x), "\n")
}

tag <- function(x, ...) {
  UseMethod("tag")
}

tag.collateral <- function(x, ...) {
  NA
}

tag.debt_collateral <- function(x, ...) {
  paste(x$term, x$issuer, "debt", x$rating, sep = "_")
}

tag.equity_collateral <- function(x, ...) {
  paste(x$exchange, "equity", sep = "_")
}

tag.gold <- function(x, ...) {
  "gold"
}

tag.cash <- function(x, ...) {
  paste(if(x$is_domestic) "domestic" else "foreign", "cash")
}

collateral_haircut <- function(collateral, holding_period, min_holding_period) {
  hc <- tag(collateral) %=>% collateral_to_haircut
  # APS 112, Att. H, para. 41 (Aug 2017 DRAFT)
  hc * sqrt(holding_period / min_holding_period)
}

