#' Title
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param annot annotation data frame
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr bind_rows group_by left_join mutate rename select
#'             summarize ungroup
#' @importFrom tidyr pivot_longer unite
#' @importFrom readxl excel_sheets read_excel
#' @importFrom rlang .data
Enrich3Harmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  # Find all the sheets (3 of import).
  all_sheets <- readxl::excel_sheets(filename)[-1]
  
  # Data processing for all sheets in the file except first.
  combined_dataset <- list()
  for(i in all_sheets) {
    cat(i, "\n")
    combined_dataset[[i]] <- read_trait_sheet(filename, i)
  }
  combined_dataset <- dplyr::bind_rows(combined_dataset)

  # Adding strain, animal,sex and condition based on annotation file
  out <- dplyr::select(
    dplyr::mutate(
      dplyr::left_join(
        combined_dataset,
        dplyr::select(
          dplyr::rename(annot, animal = "number", condition = "diet"),
          mouse_id,  strain, animal, sex, condition),
        by = c("mouse_id")),
      animal = as.character(.data$animal)),
    # Only use latest run if duplicates done.
    strain, sex, animal, condition, trait, minute, value)
  
  # Add enrichment traits Mn
  out <- dplyr::select(
    dplyr::select(
      dplyr::mutate(
        tidyr::pivot_longer(
          dplyr::select(
            dplyr::select(
              dplyr::ungroup(
                dplyr::mutate(
                  dplyr::group_by(
                    dplyr::mutate(
                      out,
                      traitgp = str_remove(trait, "_C[0-9A-Z]+$")),
                    strain, sex, animal, condition, minute, traitgp),
                  mvalue = value / sum(value),
                  minute = minute,
                  trait = trait)),
              -traitgp),
            value, mvalue, everything()),
          value:mvalue, names_to = "enrich", values_to = "value"),
        trait = ifelse(enrich == "mvalue",
                       str_replace(trait, "_C([0-9]+)$", "_M\\1"),
                       trait)),
      -enrich),
    # Combine `minute` into `trait`.
    strain, sex, animal, condition, trait, minute, value)
  
  # Area under curve and other time summaries
  auc <- dplyr::select(
    area_under_curve(out, "minute"),
    strain, sex, animal, condition, trait, value)
    
  # Append "_18wk" to trait names and drop `minute` column.
  dplyr::mutate(
    bind_rows(tidyr::unite(out, trait, trait, minute), auc),
    trait = paste(trait, "18wk", sep = "_"))
}

read_trait_sheet <- function(filename, sheet) {
  traitname <- str_remove(sheet, "_labels")
  dplyr::ungroup(
    dplyr::summarize(
      dplyr::group_by(
        dplyr::select(
          dplyr::mutate(
            dplyr::rename(
              readr::read_excel(filename, sheet = sheet),
              mouse_id = "Strains",
              trait = "Label",
              minute = "Timepoints",
              value = "NA Corrected with zero",
              run = "Sample Name"),
            run = as.numeric(str_remove(str_remove(run, "-.*$"), "^run")),
            minute = str_remove(minute, "min$"),
            trait = paste0("C", str_remove(trait, "^.*\\-")),
            trait = ifelse(trait == "PARENT", "C0", trait),
            trait = paste(traitname, trait, sep = "_")),
          mouse_id, run, trait, minute, value),
        mouse_id, trait, minute),
      value = value[which.max(run)], .groups = "drop"))
}
