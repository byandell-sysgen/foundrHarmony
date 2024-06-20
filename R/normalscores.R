#' Normal Scores to Value Column of Object
#'
#' @param object data frame
#' @param standard standardize to mean 0 and variance 1 if `TRUE`
#'
#' @return data frame
normalscores <- function(object, standard = FALSE) {
  # Normal scores by trait
  dplyr::filter(
    dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(
          object,
          .data$trait),
        value = nqrank(.data$value, jitter = TRUE, standard = standard))),
    !is.na(.data$value),
    !is.nan(.data$value)) 
}