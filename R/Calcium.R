#' Title
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param basal include `basal` dataset if `TRUE`
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr bind_rows filter mutate rename select
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom rlang .data
CalciumHarmony <- function(dataset, links, basal = FALSE, ...) {
  filename <- linkpath(dataset, links)
  # Data are in sheets 1 and 2 starting on line 1.
  calcium <- 
    # Pivot traits, which now begin after `condition`.
    tidyr::pivot_longer(
      # Rename to harmonize.
      dplyr::rename(
        # Put key columns in front.
        dplyr::select(
          # Matlab by animal (sheet 2)
          readxl::read_excel(filename, sheet = 2),
          Strain, Sex, Animal, condition, everything()),
        strain = "Strain",
        sex = "Sex",
        animal = "Animal"),
      -(strain:condition), names_to = "trait", values_to = "value")
  
  if(basal) {
    # Filter out Basal, which will be in separate dataset.
    return(
      dplyr::select(
        dplyr::filter(calcium, .data$condition == "Basal"),
        -condition))
  }
    
  dplyr::bind_rows(
    # Pivot traits, which now begin after `condition`.
    tidyr::pivot_longer(
      # Put key columns in front.
      dplyr::select(# Rename to code-friendly names.
        dplyr::rename(
          # Spectral density data (sheet 1)
          readxl::read_excel(filename, sheet = 1),
          freq_8_1 = "8 1st freq.",
          freq_8_2 = "8 2nd freq.",
          ampl_8_1 = "8 1st freq. ampl.",
          ampl_8_2 = "8 2nd freq. ampl.",
          strain = "Strain",
          sex = "Sex"),
        strain, sex, animal, condition, everything()),
      -(strain:condition), names_to = "trait", values_to = "value"),

    # Filter out Basal, which will be in separate dataset.
    dplyr::filter(calcium, .data$condition != "Basal"))
}