Enrich3Harmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  # Find all the sheets (3 of import).
  all_sheets <- excel_sheets(filename)[-1]
  
  # Data processing for all sheets in the file except first.
  combined_dataset <- list()
  for(i in all_sheets) {
    cat(i, "\n")
    combined_dataset[[i]] <- read_trait_sheet(filename, i)
  }
  combined_dataset <- bind_rows(combined_dataset)

  # Adding strain, animal,sex and condition based on annotation file
  out <- 
    left_join(
      combined_dataset,
      annot %>% 
        rename(animal = "number", condition = "diet") %>% 
        select(mouse_id,  strain, animal, sex, condition),
      by = c("mouse_id")) %>%
    mutate(animal = as.character(animal)) %>%
    
    # Only use latest run if duplicates done.
    select(strain, sex, animal, condition, trait, minute, value)
  
  # Add enrichment traits Mn
  out <- out %>%
    mutate(traitgp = str_remove(trait, "_C[0-9A-Z]+$")) %>%
    group_by(strain, sex, animal, condition, minute, traitgp) %>%
    mutate(mvalue = value / sum(value),
           minute = minute,
           trait = trait) %>%
    ungroup() %>%
    select(-traitgp) %>%
    select(value, mvalue, everything()) %>%
    pivot_longer(value:mvalue, names_to = "enrich", values_to = "value") %>%
    mutate(trait = ifelse(enrich == "mvalue",
                          str_replace(trait, "_C([0-9]+)$", "_M\\1"),
                          trait)) %>%
    select(-enrich) %>%
    # Combine `minute` into `trait`.
    select(strain, sex, animal, condition, trait, minute, value)
  
  # Area under curve and other time summaries
  auc <- area_under_curve(out, "minute") %>%
    select(strain, sex, animal, condition, trait, value)
    
  # Append "_18wk" to trait names and drop `minute` column.
  bind_rows(
    out %>%
      unite(trait, trait, minute),
    auc) %>%
    mutate(trait = paste(trait, "18wk", sep = "_"))
}

read_trait_sheet <- function(filename, sheet) {
  traitname <- str_remove(sheet, "_labels")
  read_excel(filename, sheet = sheet) %>%
    rename(mouse_id = "Strains",
           trait = "Label",
           minute = "Timepoints",
           value = "NA Corrected with zero",
           run = "Sample Name") %>%
    mutate(run = as.numeric(str_remove(str_remove(run, "-.*$"), "^run")),
           minute = str_remove(minute, "min$"),
           trait = paste0("C", str_remove(trait, "^.*\\-")),
           trait = ifelse(trait == "PARENT", "C0", trait),
           trait = paste(traitname, trait, sep = "_")) %>%
    select(mouse_id, run, trait, minute, value) %>%
    group_by(mouse_id, trait, minute) %>%
    summarize(value = value[which.max(run)], .groups = "drop") %>%
    ungroup()
}
