CalciumHarmony <- function(dataset, links, basal = FALSE, ...) {
  filename <- linkpath(dataset, links)
  
  # Data are in sheets 1 and 2 starting on line 1.
  
  calcium <- 
    # Matlab by animal (sheet 2)
    read_excel(filename, sheet = 2) %>%
    
    # Put key columns in front.
    select(Strain, Sex, Animal, condition, everything()) %>%
    
    # Rename to harmonize.
    rename(strain = "Strain",
           sex = "Sex",
           animal = "Animal") %>%
    
    # Pivot traits, which now begin after `condition`.
    pivot_longer(-(strain:condition),
                 names_to = "trait", values_to = "value")
  
  if(basal) {
    return(
      calcium %>%
        # Filter out Basal, which will be in separate dataset.
        filter(condition == "Basal") %>%
        select(-condition))
  }
    
  bind_rows(
    # Spectral density data (sheet 1)
    read_excel(filename, sheet = 1) %>%
      
      # Rename to code-friendly names.
      rename(freq_8_1 = "8 1st freq.",
             freq_8_2 = "8 2nd freq.",
             ampl_8_1 = "8 1st freq. ampl.",
             ampl_8_2 = "8 2nd freq. ampl.",
             strain = "Strain",
             sex = "Sex") %>%
      
      # Put key columns in front.
      select(strain, sex, animal, condition, everything()) %>%
      
      # Pivot traits, which now begin after `condition`.
      pivot_longer(-(strain:condition),
                   names_to = "trait", values_to = "value"),

    calcium %>%
      # Filter out Basal, which will be in separate dataset.
      filter(condition != "Basal"))
}