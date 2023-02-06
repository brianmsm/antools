#' Identify items for partial invariance
#'
#' @description This function uses 2 lavaan functions to generate
#'    a table (in tibble format) to identify which items and
#'    specifications among the groups could be generating
#'    problematic situations.
#'
#' @param fit A lavaan object that is estimated by [lavaan::cfa()],
#'    [lavaan::sem()] or [semTools::measEq.syntax()].
#' @param cutoff.value By default, only p-values below 0.10 are retained.
#'    If you want to obtain all of them in their entirety, indicate the value
#'    as `NULL`.
#' @param detailed Defaults to FALSE, and will display only the essential
#'    information requested. If you want the complete columns obtained with
#'    [lavaan::parTable()], set to TRUE.
#'
#' @return A [tibble::tibble()] to identify items and specifications that,
#'    when indicated, could solve non-invariance problems.
#'
#' @seealso [lavaan::parTable()], [lavaan::lavTestScore()]
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate select filter arrange left_join as_tibble
#' @importFrom lavaan lavTestScore parTable
#'
#' @export
#'
#' @examples
#'
#' library(semTools)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#'
#' fit_scalar <- measEq.syntax(configural.model = HS.model,
#'                             data = lavaan::HolzingerSwineford1939,
#'                             estimator = "MLR",
#'                             parameterization = "theta",
#'                             ID.fac = "std.lv",
#'                             ID.cat = "Wu.Estabrook.2016",
#'                             group = "sex",
#'                             group.equal = c("loadings", "intercepts"),
#'                             return.fit = TRUE)
#'
#' identify_items_partial(fit_scalar)
#'

identify_items_partial <- function(fit,
                                   cutoff.value = 0.10,
                                   detailed = FALSE) {

  univariate_test <- suppressWarnings(lavTestScore(fit)$uni)
  parameter_table <- parTable(fit)


  if (detailed) {
    parameter_table <- parameter_table %>%
      mutate(
        op1 = paste(.data$lhs, .data$op, .data$rhs),
        op2 = op1
      ) %>%
      select(.data$op1, .data$op2, .data$group,
             .data$plabel, .data$user:.data$label,
             .data$start:.data$se)
  } else {
    parameter_table <- parameter_table %>%
      mutate(
        op1 = paste(.data$lhs, .data$op, .data$rhs),
        op2 = op1
      ) %>%
      select(.data$op1, .data$op2, .data$group, .data$plabel)
  }


  if (!is.null(cutoff.value)) {
    univariate_test <- univariate_test %>%
      filter(.data$p.value < {{ cutoff.value }})
  }

  univariate_test %>%
    left_join(
      parameter_table %>%
        select(-.data$op2),
      by = c("lhs" = "plabel")
    ) %>%
    left_join(
      parameter_table %>%
        select(-.data$op1),
      by = c("rhs" = "plabel")
    ) %>%
    arrange(.data$p.value) %>%
    as_tibble()
}
