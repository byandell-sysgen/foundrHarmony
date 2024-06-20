#' Physio Harmony
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param annot annotation data frame
#' @param sheet,rename_function additional parameters
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr bind_rows count distinct left_join mutate rename select
#' @importFrom tidyr pivot_longer separate_wider_delim
#' @importFrom readxl read_excel
#' @importFrom rlang .data
PhysioHarmony <- function(dataset, links, annot, sheet = 2, rename_function, ...) {
  filename <- linkpath(dataset, links)
  
  # Read data and pivot to longer format; rename animal and condition.
  out <- readxl::read_excel(filename, sheet = sheet)
  
  # Rename columns if provided.
  if(!missing(rename_function) && is.function(rename_function)) {
    out <- rename_function(out)
  }
  
  # Traits begin in column 5
  out <- tidyr::pivot_longer(out, -(1:4),
                             names_to = "trait", values_to = "value")
  
  # Add columns from annotation table.
  out <- dplyr::left_join(
    # Some sexes have changed
    dplyr::select(out, -sex, -diet),
    dplyr::select(annot, strain, number, sex, diet),
    by = c("strain","number")) 
  
  out <- dplyr::select(
    dplyr::mutate(
      # Rename columns to harmonize data.
      dplyr::rename(out, condition = "diet", animal = "number"),
      animal = as.character(.data$animal)),
    # These are harmonized columns and their names.
    strain, sex, animal, condition, trait, value)

  # Add area under curve traits for measurements over minutes.
  GTT <-dplyr::mutate(
    # Separate out minutes and week; kludge to catch cpep ratio trait.
    tidyr::separate_wider_delim(
      dplyr::filter(out, grepl("_[0-9]+_[0-9]+wk$", trait)),
      trait,
      delim = "_",
      names = c("cpep1", "cpep2", "gtt","trait","minute","week"),
      too_few = "align_end"),
    trait = ifelse(.data$trait == "ratio",
                   paste(.data$cpep1, .data$cpep2, .data$gtt, .data$trait,
                         sep = "_"),
                   paste(.data$gtt, .data$trait, sep = "_")))
  
  # Filter to traits with >1 minute measurement.
  GTTct <- dplyr::filter(
    dplyr::count(
      dplyr::distinct(GTT, .data$trait, .data$minute, .data$week),
      .data$trait, .data$week),
    .data$n > 1)
  
  GTT <- dplyr::select(
    tidyr::unite(
      # Calculate AUC and other summaries.
      area_under_curve(
        dplyr::filter(GTT, trait %in% GTTct$trait & week %in% GTTct$week),
        "minute"),
      # Unite summary name with week.
      trait, trait, week),
    # Harmonize names.
    strain, sex, animal, condition, trait, value)
  
  # Add area under curve traits for measurements over weeks.
  wks <- dplyr::mutate(
    # Kludge to use AUC routine for now by calling weeks as minutes.
    tidyr::separate_wider_delim(
      dplyr::filter(out, grepl("_[0-9]+wk$", trait) &
                      !grepl("_([0-9]+|tAUC|iAUC)_[0-9]+wk$", trait)),
      trait,
      delim = "_",
      names = c("trait1","trait","week"),
      too_few = "align_end"),
    trait = ifelse(is.na(trait1), trait, paste(trait1, trait, sep = "_")),
    week = as.numeric(str_remove(week, "wk$")))
  
  # Filter to traits with >1 week measurement.
  wksct <- dplyr::filter(
    dplyr::count(dplyr::distinct(wks, .data$trait, .data$week), .data$trait),
    .data$n > 1)
  
  wks <- dplyr::select(
    # Calculate AUC and other summaries.
    area_under_curve(dplyr::filter(wks, trait %in% wksct$trait), "week"),
    # Harmonize names.
    strain, sex, animal, condition, trait, value)
  
  dplyr::bind_rows(out, GTT, wks)
}

GTT_look <- function(dataset, links, ...) {
  # Write a CSV file with various AUC calculations.
  out <- PhysioGTTHarmony(dataset, links)
  
  out <- out |>
    dplyr::filter(grepl("AUC", trait),
           trait != "cpep_ins_molar_ratio_iAUC_14wk") |>
    dplyr::mutate(trait = ifelse(grepl("GTT", trait),
                          str_remove(trait, "^GTT_"),
                          trait)) |>
    tidyr::separate_wider_delim(
      trait,
      delim = "_",
      names = c("trait","auc","week")) |> 
    pivot_wider(names_from = "auc", values_from = "value")
  
  write.csv(out, "auc.csv")
  invisible(out)
}