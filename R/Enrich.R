EnrichHarmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  if(tools::file_ext(filename) == "csv") {
    readfn <- function(filename, skip, n_max = Inf, name_repair = "unique",
                       col_names = TRUE) {
      read_csv(filename, skip = skip, n_max = n_max, col_names = col_names,
               name_repair = name_repair)
    }
  } else {
    readfn <- function(filename, skip, n_max = Inf, name_repair = "unique",
                       col_names = TRUE) {
      read_excel(filename, sheet = 1,
                 skip = skip, n_max = n_max, col_names = col_names,
                 .name_repair = name_repair)
    }
  }
  
  # Get trait names from 2nd row.
  traits <- unlist(readfn(filename, 1, 1, col_names = FALSE)[1,])
  traits <- unique(traits[!is.na(traits)])
  names(traits) <- NULL
  
  # Read data and pivot to longer format; rename animal and sex.
  out <- readfn(filename, 2, name_repair = "minimal") |> 
    pivot_longer(-(strain:Sexes),
                 names_to = "minutes", values_to = "value") |>
    
    # Rename animal and sex.
    rename(animal = "number",
           sex = "Sexes")
    
  # Add columns from annotation table.
  out <- left_join(
    # Some sexes have changed
    out |>
      select(-sex),
    annot |>
      rename(animal = "number",
             condition = "diet") |>
      select(strain, animal, sex, condition),
    by = c("strain","animal")) 
  
  # Determine number of samples and number of time points.
  nsample <- out |>
    distinct(strain,animal,sex) |>
    count()
  ntime <- out |>
    distinct(minutes) |> count()
  
  # Add trait names from row 2 of file.
  # Assume here the same number of time points per trait.
  out <- out |>
    mutate(
      trait =
        rep(
          rep(
            traits,
            rep(
              ntime,
              length(traits))),
          nsample))
  
  # Area under curve and other time summaries
  auc <- area_under_curve(out)
  
  # These are harmonized columns and their names.
  out <- bind_rows(
    out |>
      
      # Unite trait and minutes to form new trait by minutes.
      unite(
        trait,
        trait, minutes) |>
      select(strain, sex, animal, condition, trait, value),
    auc |>
      select(strain, sex, animal, condition, trait, value)) |>
    
    # Make sure animal is character.
    # Add `_18wk` to end of trait names.
    mutate(animal = as.character(animal),
           trait = paste(trait, "18wk", sep = "_"))
}
