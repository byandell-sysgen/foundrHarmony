#' Read Metabolite File for Harmonization
#'
#' There are  minor differences among metabolite files.
#' To harmonize, we assume the following:
#'    Some number of rows (3-4) at top of file are skipped
#'    Columns from “compound” to “data_type” precede mouse data
#'    Mouse ID always begins with FFF-nn with “FFF” = founder, “nn” = animal
#'    (the rest of Mouse ID seems to change from file to file)

#' @param dataset name of dataset
#' @param links dataframe of links to data files 
#' @param annot annotation data frame
#' @param skiprows number of rows to skip
#' @param ... ignore
#'
#' @return data frame with columns for strain,sex,animal,condition,trait,value
#' @export
#' @rdname userHarmony
#' @importFrom dplyr left_join mutate select
#' @importFrom tidyr pivot_longer unite
#' @importFrom readxl read_excel
#' @importFrom rlang .data
#'
MetHarmony <- function(dataset, links, annot, skiprows = 4, ...) {
  filename <- linkpath(dataset, links)

  out <-
    # Pivot traits, which begin in column 5.
    tidyr::pivot_longer(
      # Data are in sheet 1 starting on line 5.
      readxl::read_excel(filename, sheet = 1, skip = skiprows),
      -(compound:data_type), names_to = "mouse_id", values_to = "value")
  
  # Some data has minute embedded in `mouse_id`.
  if(is_minute <- any(str_detect(out$mouse_id, "min"))) {
    out <- dplyr::mutate(out,
      minute = str_replace(mouse_id, ".*_(\\d+)min.*", "\\1"))
  }
  out <- dplyr::mutate(out, mouse_id = str_remove(.data$mouse_id, "_.*"))
    
  # Join with `annot` to get strain, number, sex, diet
  out <- dplyr::left_join(out, annot, by = "mouse_id")
  
  # If `minute` in data, add to `trait` name and append `week`.
  # Assume here measurements are at 18 week (sacrifice).
  if(is_minute) {
    out <- dplyr::mutate(
      tidyr::unite(out, compound, compound, minute),
      compound = paste0(.data$compound, "_18wk"))
  }

  dplyr::select(
    dplyr::mutate(
      # Rename `compound` as `trait`, number as `animal`
      dplyr::rename(out, trait = "compound", animal = "number",
        condition = "diet"),
      # Make sure animal is character.
      animal = as.character(.data$animal)),
    # These are harmonized columns and their names.
    strain, sex, animal, condition, trait, value)
}