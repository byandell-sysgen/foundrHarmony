ProteinHarmony <- function(dataset, links, ...) {
  filename <- linkpath(dataset, links)
  
  # Data are in sheet 3 starting on line 3.
  out <- read_excel(filename, sheet = 3, skip = 2) %>%
    # Select relevant columns
    select(ID, Gene.mes, "129.1.F":"WSB.9.F") %>%
    
    # Unite gene name and ID.
    unite(trait, Gene.mes, ID, na.rm = TRUE) %>%
    
    # Pivot traits, which now begin after `trait`.
    pivot_longer(-trait, names_to = "animal", values_to = "value") %>%
    
    # Extract `strain` and `sex` from animal column.
    mutate(strain = str_remove(animal, "\\..*$"),
           sex = str_remove(animal, "^.*\\.")) %>%
    
    # Remove NAs and do log10 transform
    filter(!is.na(value)) %>%
    mutate(value = log10(value)) %>%
    
    # Select harmonized columns in order.
    select(strain, sex, animal, trait, value)
  
  
  # Find genes with duplicated symbols.
  dupGenes <-
    (out %>%
       distinct(trait) %>%
       separate_wider_delim(trait, "_", names = c("gene", "ID"),
                            too_many = "merge") %>%
       mutate(dup = duplicated(gene)) %>%
       filter(dup))$gene
  
  # Change `trait` back to gene `SYMBOL` for unique gene entries.
  out <- dplyr::mutate(
    out,
    trait = ifelse(str_remove(trait, "_.*") %in% dupGenes,
                   trait, str_remove(trait, "_.*")))
  
  out
}