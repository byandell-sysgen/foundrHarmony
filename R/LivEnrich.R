#' LivEnrich Harmony
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param annot annotation data frame
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr filter mutate rename
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom rlang .data
LivEnrichHarmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  dplyr::rename(
    tidyr::pivot_longer(
      dplyr::mutate(
        dplyr::filter(
          dplyr::rename(
            # Read data and pivot to longer format; rename animal and sex.
            readxl::read_excel(filename, sheet = 1, .name_repair = "minimal"),
            strain = "Strains",
            animal = "Number",
            sex = "Sexes",
            diet = "Diets"),
          !is.na(sex) & !is.na(diet)), # NZO_3 is not a valid animal.
        diet = str_replace(diet, "-", "_"),
        animal = as.character(animal)),
      -(strain:diet), names_to = "trait", values_to = "value"),
    condition = "diet")
}
