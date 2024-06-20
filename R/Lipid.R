#' Lipid Harmony
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param annot annotation data frame
#' @param sheetNum,skipNum,minCol,maxCol,charCols additional parameters
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr left_join mutate rename select
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom rlang .data
LipidHarmony <- function(dataset, links, annot,
                         sheetNum, skipNum, minCol, maxCol, charCols, ...) {
  filename <- linkpath(dataset, links)

  # Get data
  out <- readxl::read_excel(filename, sheet = sheetNum, skip = skipNum,
                    .name_repair = "minimal")
  
  # Remove unwanted columns
  out <- out[, -c(minCol:maxCol)]

  # Check for missing mouse IDs
  m <- match(out$mouse_id, annot$mouse_id)
  mm <- match(annot$mouse_id, out$mouse_id)
  if(any(is.na(m)) | any(is.na(mm)))
    stop(paste("missing mouse ids",
               paste(out$mouse_id[is.na(m)], collapse = ", "),
               " annot",
               paste(annot$mouse_id[is.na(mm)], collapse = ", ")))
  
  dplyr::select(
    dplyr::mutate(
      dplyr::rename(
        # Adding diet column
        dplyr::left_join(
          # Pivot to longer format
          tidyr::pivot_longer(out, -1, names_to = "lipids", values_to = "value"),
          dplyr::select(
            dplyr::rename(annot, animal = "number", condition = "diet"),
            mouse_id, strain, animal, sex, condition),
          by = "mouse_id"),
        trait = "lipids"),
      animal = as.character(.data$animal)),
    strain, sex, animal, condition, trait, value)
}
