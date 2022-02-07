#' Get the mirt fit indexes in tidy format
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
fit_mirt <- function(x) {
  fit_table <- tibble::tibble(
    AIC = mirt::extract.mirt(x, "AIC"),
    # AICc = mirt::extract.mirt(x, "AICc"), # https://github.com/philchalmers/mirt/commit/1c6aeb8b43f21054b62c0b01001b2f3184514029
    BIC = mirt::extract.mirt(x, "BIC"),
    SABIC = mirt::extract.mirt(x, "SABIC"),
    "-2LL" = (mirt::extract.mirt(x, "logLik"))*-2
  )

  return(fit_table)
}
