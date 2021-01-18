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
    AICc = mirt::extract.mirt(x, "AICc"),
    BIC = mirt::extract.mirt(x, "BIC"),
    SABIC = mirt::extract.mirt(x, "SABIC"),
    "-2LL" = (mirt::extract.mirt(x, "logLik"))*-2
  )

  return(fit_table)
}
