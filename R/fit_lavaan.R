#' Get the lavaan fit indexes in tidy format
#'
#' @description `fit_lavaan` allows to extract the fit indexes of a factorial
#'   model estimated with lavaan. The main feature of this function is that, if
#'   the estimator is robust, the fit indexes obtained will be the robust ones.
#'
#' @param x A lavaan object that is estimated by [lavaan::cfa()] or
#'   [lavaan::sem()].
#'
#' @return A [tibble::tibble()] with a row and each estimated parameter in a
#'   column
#'
#'   \item{nobs}{Number of analysed observations}
#'   \item{estimator}{Estimator used}
#'   \item{ngroups}{Number of groups in model}
#'   \item{converged}{Logical indicator about the convergence of the model}
#'   \item{chisq}{Model chi squared}
#'   \item{df}{degrees of freedom}
#'   \item{pvalue}{P-value associated with the model}
#'   \item{npar}{Number of parameters in the model}
#'   \item{CFI}{Comparative Fit Index}
#'   \item{TLI}{Tucker-Lewis Index also known as NNFI}
#'   \item{RMSEA}{Root Mean Square error of Approximation}
#'   \item{RMSEA.CI.LOWER}{95 percent lower bound on RMSEA}
#'   \item{RMSEA.CI.UPPER}{95 percent upper bound on RMSEA}
#'   \item{SRMR}{Standardized Root Mean squared Residual}
#'   \item{WRMR}{Weighted Root Mean Square Residual}
#'   \item{AIC}{Akaike information criterion}
#'   \item{BIC}{Bayesian information criterion}
#'   \item{missing_method}{Method for eliminating missing data}
#'
#' @export
#' @seealso [lavaan::cfa()], [lavaan::sem()], [lavaan::fitmeasures()]
#'
#' @examples
#'
#' library(lavaan)
#'
#' HS.model <- " visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 "
#'
#' fit <- cfa(
#'   model = HS.model,
#'   data = HolzingerSwineford1939
#' )
#'
#' fit_lavaan(fit)
fit_lavaan <- function(x) {
  type <- is_robust_estimator_lavaan(x)

  if (lavaan::lavInspect(x, "converged")) {
    if (type == "robust") {
      fit_measure <- x %>%
        lavaan::fitmeasures(
          fit.measures =
            c(
              "npar",
              "chisq.scaled",
              "df.scaled",
              "pvalue.scaled",
              "rmsea.scaled",
              "rmsea.ci.lower.scaled",
              "rmsea.ci.upper.scaled",
              "srmr",
              "wrmr",
              "tli.scaled",
              "cfi.scaled"
            )
        ) %>%
        tibble::enframe(name = "term") %>%
        tidyr::pivot_wider(
          id_cols = term,
          names_from = term,
          values_from = value
        ) %>%
        dplyr::mutate(
          dplyr::across(dplyr::everything(), as.numeric)
        ) %>%
        dplyr::bind_cols(
          tibble::tibble(
            converged = lavaan::lavInspect(x, "converged"),
            estimator = lavaan_estimator(x),
            ngroups = lavaan::lavInspect(x, "ngroups"),
            nobs = sum(lavaan::lavInspect(x, "nobs"))
          )
        ) %>%
        dplyr::select(
          nobs, estimator, ngroups, converged, chisq.scaled,
          df.scaled, pvalue.scaled, npar, cfi.scaled, tli.scaled,
          rmsea.scaled, rmsea.ci.lower.scaled, rmsea.ci.upper.scaled,
          srmr, wrmr
        ) %>%
        dplyr::rename_with(
          ~ stringr::str_remove(., ".scaled")
        )
    } else {
      fit_measure <- x %>%
        lavaan::fitmeasures(
          fit.measures =
            c(
              "npar",
              "chisq",
              "df",
              "pvalue",
              "rmsea",
              "rmsea.ci.lower",
              "rmsea.ci.upper",
              "srmr",
              "wrmr",
              "aic",
              "bic",
              "tli",
              "cfi"
            )
        ) %>%
        tibble::enframe(name = "term") %>%
        tidyr::pivot_wider(
          id_cols = term,
          names_from = term,
          values_from = value
        ) %>%
        dplyr::mutate(
          dplyr::across(dplyr::everything(), as.numeric)
        ) %>%
        dplyr::bind_cols(
          tibble::tibble(
            converged = lavaan::lavInspect(x, "converged"),
            estimator = lavaan::lavInspect(x, "options")$estimator,
            ngroups = lavaan::lavInspect(x, "ngroups"),
            missing_method = lavaan::lavInspect(x, "options")$missing,
            nobs = sum(lavaan::lavInspect(x, "nobs"))
          )
        ) %>%
        dplyr::relocate(
          nobs, estimator, ngroups, converged,
          chisq, df, pvalue, npar, cfi, tli, rmsea,
          rmsea.ci.lower, rmsea.ci.upper, srmr, wrmr,
          aic, bic
        ) %>%
        dplyr::rename(
          AIC = aic,
          BIC = bic
        )
    }
  } else {
    fit_measure <- tibble::tibble(
      nobs = sum(lavaan::lavInspect(x, "nobs")),
      estimator = lavaan_estimator(x),
      ngroups = lavaan::lavInspect(x, "ngroups"),
      converged = lavaan::lavInspect(x, "converged"),
      chisq = NA_real_,
      df = NA_real_,
      pvalue = NA_real_,
      nparameter = NA_real_,
      cfi = NA_real_,
      tli = NA_real_,
      rmsea = NA_real_,
      rmsea.ci.lower = NA_real_,
      rmsea.ci.upper = NA_real_,
      srmr = NA_real_,
      wrmr = NA_real_
    )
  }

  fit_measure <- fit_measure %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_to_upper(.),
      .cols = c(cfi:wrmr)
    )

  return(fit_measure)
}


# Internal function ------------------------------------------------------------

lavaan_estimator <- function(x) {
  if (lavaan::lavInspect(x, "options")$estimator == "DWLS") {
    if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
      estimator <- "WLSM"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
      estimator <- "WLSMVS"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
      estimator <- "WLSMV"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
      lavaan::lavInspect(x, "options")$test == "standard") {
      estimator <- "DWLS"
    } else {
      estimator <- "DWLS_variant"
    }
  } else if (lavaan::lavInspect(x, "options")$estimator == "ULS") {
    if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
      estimator <- "ULSM"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
      estimator <- "ULSMVS"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
      estimator <- "ULSMV"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
      lavaan::lavInspect(x, "options")$test == "standard") {
      estimator <- "ULS"
    } else {
      estimator <- "ULS_variant"
    }
  } else if (lavaan::lavInspect(x, "options")$estimator == "ML") {
    if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
      estimator <- "MLM"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.huber.white" &
      lavaan::lavInspect(x, "options")$test %in% c(
        "yuan.bentler.mplus",
        "yuan.bentler"
      )) {
      estimator <- "MLR"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
      estimator <- "MLMVS"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
      lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
      estimator <- "MLMV"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
      lavaan::lavInspect(x, "options")$test == "standard" &
      unique(lavaan::lavInspect(x, "options")$information) == "expected") {
      estimator <- "ML"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
      lavaan::lavInspect(x, "options")$test == "standard" &
      unique(lavaan::lavInspect(x, "options")$information) == "first.order") {
      estimator <- "MLF"
    } else {
      estimator <- "ML_variant"
    }
  } else {
    estimator <- lavaan::lavInspect(x, "options")$estimator
  }

  return(estimator)
}

is_robust_estimator_lavaan <- function(x) {
  if (lavaan::lavInspect(x, "options")$test %in% c(
    "satorra.bentler",
    "yuan.bentler",
    "yuan.bentler.mplus",
    "mean.var.adjusted",
    "scaled.shifted"
  )) {
    type <- "robust"
  } else {
    type <- "non-robust"
  }
  return(type)
}
