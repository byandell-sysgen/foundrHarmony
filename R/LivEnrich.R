LivEnrichHarmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  # Read data and pivot to longer format; rename animal and sex.
  read_excel(filename, sheet = 1, .name_repair = "minimal") %>%
    rename(strain = "Strains",
           animal = "Number",
           sex = "Sexes",
           diet = "Diets") %>%
    filter(!is.na(sex) & !is.na(diet)) %>% # NZO_3 is not a valid animal.
    mutate(diet = str_replace(diet, "-", "_"),
           animal = as.character(animal)) %>% 
    pivot_longer(-(strain:diet),
                 names_to = "trait", values_to = "value") %>%
    rename(condition = "diet")
}
