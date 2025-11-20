# converts bol 1333 data to tidy format to include class limits into all standards

bol_1333_std = read_csv(here::here("data/standards/bolivian_standards_1333.csv"))
bol_tidy = tidy_bolivian_1333(bol_1333_std, metal_info)
write_csv(bol_tidy, here::here("data/standards/bol_tidy.csv"))


tidy_bolivian_1333 <- function(df1333, metal_info) {
  
  abbr_map <- purrr::map_chr(metal_info, "symbol")
  names(abbr_map) <- purrr::map_chr(metal_info, "name")
  
  df1333 %>%
    pivot_longer(
      cols = starts_with("Class"),
      names_to = "class",
      values_to = "value"
    ) %>%
    rowwise() %>%
    mutate(
      parsed = list(parse_parameter_and_fraction(Parameter)),
      parameter_clean = parsed$parameter,
      fraction = parsed$fraction
    ) %>%
    ungroup() %>%
    mutate(
      regulator = "Bolivian Law 1333",
      parameter = parameter_clean,
      abbr = abbr_map[parameter_clean] %||% NA_character_,
      unit = Unit,              # keep original units exactly
      media = "water",
      notes = paste0(class, " limit"),
      source = NA,
      hqcr = "hq"
    ) %>%
    select(regulator, parameter, abbr, value, unit, media, fraction,
           notes, source, hqcr)
}
