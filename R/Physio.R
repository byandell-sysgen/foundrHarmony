PhysioHarmony <- function(dataset, links, annot, sheet = 2, rename_function, ...) {
  filename <- linkpath(dataset, links)
  
  # Read data and pivot to longer format; rename animal and condition.
  out <- read_excel(filename, sheet = sheet)
  
  # Rename columns if provided.
  if(!missing(rename_function) && is.function(rename_function)) {
    out <- rename_function(out)
  }
  
  out <- out |>
    # Traits begin in column 5
    pivot_longer(-(1:4), names_to = "trait", values_to = "value")
  
  
  # Add columns from annotation table.
  out <- left_join(
    # Some sexes have changed
    out |>
      select(-sex, -diet),
    annot |>
      select(strain, number, sex, diet),
    by = c("strain","number")) 
  
  out <- out |>
    # Rename columns to harmonize data.
    rename(condition = "diet",
           animal = "number") |>
    mutate(animal = as.character(animal)) |>
    
    # These are harmonized columns and their names.
    select(strain, sex, animal, condition, trait, value)

  # Add area under curve traits for measurements over minutes.
  GTT <- out |>
    filter(grepl("_[0-9]+_[0-9]+wk$", trait)) |>
    # Separate out minutes and week.
    # Kludge to catch cpep ratio trait.
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("cpep1", "cpep2", "gtt","trait","minute","week"),
      too_few = "align_end") |>
    mutate(trait = ifelse(
      trait == "ratio",
      paste(cpep1, cpep2, gtt, trait, sep = "_"),
      paste(gtt, trait, sep = "_")))
  
  # Filter to traits with >1 minute measurement.
  GTTct <- GTT |>
    distinct(trait, minute, week) |>
    count(trait, week) |>
    filter(n > 1)
  
  GTT <- GTT |>
    filter(trait %in% GTTct$trait & week %in% GTTct$week) |>
    # Calculate AUC and other summaries.
    area_under_curve("minute") |>
    # Unite summary name with week.
    unite(trait, trait, week) |>
    # Harmonize names.
    select(strain, sex, animal, condition, trait, value)

  # Add area under curve traits for measurements over weeks.
  wks <- out |>
    filter(grepl("_[0-9]+wk$", trait) & !grepl("_([0-9]+|tAUC|iAUC)_[0-9]+wk$", trait)) |>
    # Kludge to use AUC routine for now by calling weeks as minutes.
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("trait1","trait","week"),
      too_few = "align_end") |>
    mutate(
      trait = ifelse(
        is.na(trait1),
        trait,
        paste(trait1, trait, sep = "_")),
      week = as.numeric(str_remove(week, "wk$")))
  
  # Filter to traits with >1 week measurement.
  wksct <- wks |>
    distinct(trait, week) |>
    count(trait) |>
    filter(n > 1)
  
  wks <- wks |>
    filter(trait %in% wksct$trait) |>
    # Calculate AUC and other summaries.
    area_under_curve("week") |>
    # Harmonize names.
    select(strain, sex, animal, condition, trait, value)
  
  bind_rows(out, GTT, wks)
}

GTT_look <- function(dataset, links, ...) {
  # Write a CSV file with various AUC calculations.
  out <- PhysioGTTHarmony(dataset, links)
  
  out <- out |>
    filter(grepl("AUC", trait),
           trait != "cpep_ins_molar_ratio_iAUC_14wk") |>
    mutate(trait = ifelse(grepl("GTT", trait),
                          str_remove(trait, "^GTT_"),
                          trait)) |>
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("trait","auc","week")) |> 
    pivot_wider(names_from = "auc", values_from = "value")
  
  write.csv(out, "auc.csv")
  invisible(out)
}