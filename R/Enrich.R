#' Enrich Harmony
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param annot annotation data frame
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr bind_rows count distinct left_join mutate rename select
#' @importFrom tidyr pivot_longer unite
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom rlang .data
EnrichHarmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  if(tools::file_ext(filename) == "csv") {
    readfn <- function(filename, skip, n_max = Inf, name_repair = "unique",
                       col_names = TRUE) {
      readr::read_csv(filename, skip = skip, n_max = n_max,
                      col_names = col_names, name_repair = name_repair)
    }
  } else {
    readfn <- function(filename, skip, n_max = Inf, name_repair = "unique",
                       col_names = TRUE) {
      readxl::read_excel(filename, sheet = 1, skip = skip, n_max = n_max,
                         col_names = col_names, .name_repair = name_repair)
    }
  }
  
  # Get trait names from 2nd row.
  traits <- unlist(readfn(filename, 1, 1, col_names = FALSE)[1,])
  traits <- unique(traits[!is.na(traits)])
  names(traits) <- NULL
  
  # Read data and pivot to longer format; rename animal and sex.
  out <- dplyr::rename(
    tidyr::pivot_longer(
      readfn(filename, 2, name_repair = "minimal"),
      -(strain:Sexes), names_to = "minutes", values_to = "value"),
    # Rename animal and sex.
    animal = "number", sex = "Sexes")
  
  # Add columns from annotation table.
  out <- dplyr::left_join(
    # Some sexes have changed
    dplyr::select(out, -sex),
    dplyr::select(
      dplyr::rename(annot, animal = "number", condition = "diet"),
      strain, animal, sex, condition),
    by = c("strain","animal")) 
  
  # Determine number of samples and number of time points.
  nsample <- dplyr::count(
    dplyr::distinct(out, .data$strain, .data$animal, .data$sex))
  ntime <- dplyr::count(dplyr::distinct(out, .data$minutes))
  
  # Add trait names from row 2 of file.
  # Assume here the same number of time points per trait.
  out <- dplyr::mutate(out, 
    trait = rep(rep(traits, rep(ntime, length(traits))), nsample))
  
  # Area under curve and other time summaries
  auc <- area_under_curve(out)
  
  # These are harmonized columns and their names.
  out <- dplyr::bind_rows(
    # Unite trait and minutes to form new trait by minutes.
    dplyr::select(tidyr::unite(out, trait, trait, minutes),
      strain, sex, animal, condition, trait, value),
    # Make sure animal is character.
    # Add `_18wk` to end of trait names.
    dplyr::mutate(
      dplyr::select(auc, strain, sex, animal, condition, trait, value),
      animal = as.character(animal),
      trait = paste(trait, "18wk", sep = "_")))
}
