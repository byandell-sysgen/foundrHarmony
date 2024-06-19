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
#' @return data frame with columns for strain, sex, animal, condition, trait, value
#' @export
#'
MetHarmony <- function(dataset, links, annot, skiprows = 4, ...) {
  filename <- linkpath(dataset, links)

  out <-
    # Data are in sheet 1 starting on line 5.
    read_excel(filename, sheet = 1, skip = skiprows) %>%
    
    # Pivot traits, which begin in column 5.
    pivot_longer(-(compound:data_type),
                 names_to = "mouse_id", values_to = "value")
  
  # Some data has minute embedded in `mouse_id`.
  if(is_minute <- any(str_detect(out$mouse_id, "min"))) {
    out <- out %>%
      mutate(minute = str_replace(mouse_id, ".*_(\\d+)min.*", "\\1"))
  }
  out <- out %>%
    mutate(mouse_id = str_remove(mouse_id, "_.*"))
    
  # Join with `annot` to get strain, number, sex, diet
  out <- dplyr::left_join(out, annot, by = "mouse_id")
  
  # If `minute` in data, add to `trait` name and append `week`.
  # Assume here measurements are at 18 week (sacrifice).
  if(is_minute) {
    out <- out %>%
      unite(compound, compound, minute) %>%
      mutate(compound = paste0(compound, "_18wk"))
  }
  out %>%  
    # Rename `compound` as `trait`, number as `animal`
    rename(
      trait = "compound",
      animal = "number",
      condition = "diet") %>%
    mutate(
      # Make sure animal is character.
      animal = as.character(animal)) %>%

    # These are harmonized columns and their names.
    select(strain, sex, animal, condition, trait, value)
}