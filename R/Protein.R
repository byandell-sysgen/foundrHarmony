#' Pretein Harmony
#'
#' @param dataset name of dataset
#' @param links filename data frame
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @rdname userHarmony
#' @importFrom dplyr distinct filter mutate select
#' @importFrom tidyr pivot_longer unite
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove
#' @importFrom rlang .data
ProteinHarmony <- function(dataset, links, ...) {
  filename <- linkpath(dataset, links)
  
  out <- dplyr::select(
    dplyr::mutate(
      dplyr::filter(
        dplyr::mutate(
          tidyr::pivot_longer(
            tidyr::unite(
              dplyr::select(
                # Data are in sheet 3 starting on line 3.
                readxl::read_excel(filename, sheet = 3, skip = 2),
                # Select relevant columns
                ID, Gene.mes, "129.1.F":"WSB.9.F"),
              # Unite gene name and ID.
              trait, Gene.mes, ID, na.rm = TRUE),
            # Pivot traits, which now begin after `trait`.
            -trait, names_to = "animal", values_to = "value"),
          # Extract `strain` and `sex` from animal column.
          strain = str_remove(.data$animal, "\\..*$"),
          sex = str_remove(.data$animal, "^.*\\.")),
        # Remove NAs
        !is.na(.data$value)),
      # Do log10 transform
      value = log10(.data$value)),
    # Select harmonized columns in order.
    strain, sex, animal, trait, value)
  
  # Find genes with duplicated symbols.
  dupGenes <- (dplyr::filter(
    dplyr::mutate(
      separate_wider_delim(
        dplyr::distinct(out, trait),
        trait, "_", names = c("gene", "ID"),
        too_many = "merge"),
      dup = duplicated(.data$gene)),
    .data$dup))$gene
  
  # Change `trait` back to gene `SYMBOL` for unique gene entries.
  out <- dplyr::mutate(out,
    trait = ifelse(stringr::str_remove(.data$trait, "_.*") %in% dupGenes,
                   .data$trait, str_remove(.data$trait, "_.*")))
  
  out
}