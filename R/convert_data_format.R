library(pacman)
p_load(dplyr)
p_load(tidyr)
p_load(stringr)
p_load(janitor)
p_load(readr)

ID_COLS = c("data_source",
            "Station","Date","Time","Campaign","Institution",
            "River","Latitude","Longitude","Latitude Decimal","Longitude Decimal",
            "Year", "Distance from Bank", "Sieve Size")

upload_sampled_data <- function(sample_data,
                                media = NA,
                                debug_prepped = FALSE,
                                format = NA,
                                src_lang = NA,
                                target_lang = NA) {
  
  message("[upload_sampled_data] START")
  message("[upload_sampled_data] format = ", format,
          " | media = ", media,
          " | debug_prepped = ", debug_prepped,
          " | src_lang = ", src_lang,
          " | target_lang = ", target_lang)
  
  if (is.null(sample_data) || !is.data.frame(sample_data)) {
    stop("[upload_sampled_data] sample_data is NULL or not a data.frame")
  }
  
  names(sample_data) <- fix_headers(names(sample_data))
  message("[upload_sampled_data] fixed headers")
  message("[upload_sampled_data] input dim = ", nrow(sample_data), " x ", ncol(sample_data))
  message("[upload_sampled_data] input names = ", paste(names(sample_data), collapse = ", "))
  
  cleaned_data <- sample_data
  
  if (identical(format, "pilco") && !isTRUE(debug_prepped) && identical(media, "water")) {
    message("[upload_sampled_data] running clean_water_data()")
    cleaned_data <- clean_water_data(cleaned_data, source = format)
    message("[upload_sampled_data] clean_water_data() done")
    
  } else if (identical(format, "pilco") && !isTRUE(debug_prepped) && identical(media, "sediment")) {
    message("[upload_sampled_data] running clean_sediment_data()")
    cleaned_data <- clean_sediment_data(cleaned_data, source = format)
    message("[upload_sampled_data] clean_sediment_data() done")
    
  } else if (identical(format, "parameter_long")) {
    message("[upload_sampled_data] parameter_long: skipping raw cleaner")
    
  } else {
    stop("[upload_sampled_data] unsupported format/media/debug_prepped combination")
  }
  
  if (nrow(cleaned_data) == 0 || ncol(cleaned_data) == 0) {
    stop("[upload_sampled_data] cleaned_data is empty")
  }
  
  message("[upload_sampled_data] cleaned dim = ", nrow(cleaned_data), " x ", ncol(cleaned_data))
  message("[upload_sampled_data] cleaned names = ", paste(names(cleaned_data), collapse = ", "))
  
  message("[upload_sampled_data] running translate_pilco_data()")
  translated_data <- translate_pilco_data(cleaned_data, src_lang, target_lang, media)
  message("[upload_sampled_data] translate_pilco_data() done")
  
  if (is.null(translated_data) || !is.data.frame(translated_data)) {
    stop("[upload_sampled_data] translated_data is NULL or not a data.frame")
  }
  if (nrow(translated_data) == 0 || ncol(translated_data) == 0) {
    stop("[upload_sampled_data] translated_data is empty")
  }
  
  message("[upload_sampled_data] translated dim = ", nrow(translated_data), " x ", ncol(translated_data))
  message("[upload_sampled_data] translated names = ", paste(names(translated_data), collapse = ", "))
  
  if (identical(format, "pilco")) {
    message("[upload_sampled_data] running pivot_pilcomayo_data()")
    formatted_data <- pivot_pilcomayo_data(translated_data, media_type = media)
    message("[upload_sampled_data] pivot_pilcomayo_data() done")
  } else if (identical(format, "parameter_long")) {
    formatted_data <- translated_data
  } else {
    stop("[upload_sampled_data] Data format not recognized")
  }
  
  if (nrow(formatted_data) == 0 || ncol(formatted_data) == 0) {
    stop("[upload_sampled_data] formatted_data is empty")
  }
  
  message("[upload_sampled_data] formatted dim = ", nrow(formatted_data), " x ", ncol(formatted_data))
  message("[upload_sampled_data] formatted names = ", paste(names(formatted_data), collapse = ", "))
  message("[upload_sampled_data] END")
  
  formatted_data
}

pivot_pilcomayo_data <- function(df, media_type = NA, date_format = "mdy") {
  
  message("[pivot_pilcomayo_data] START | media_type = ", media_type)
  
  if (is.null(df) || !is.data.frame(df)) stop("[pivot_pilcomayo_data] df NULL/not data.frame")
  if (nrow(df) == 0 || ncol(df) == 0) stop("[pivot_pilcomayo_data] df empty")
  
  message("[pivot_pilcomayo_data] input dim = ", nrow(df), " x ", ncol(df))
  message("[pivot_pilcomayo_data] input names = ", paste(names(df), collapse = ", "))
  
  if (!"data_source" %in% names(df)) df$data_source <- NA_character_
  df <- dplyr::relocate(df, data_source, .before = 1)
  
  if (!"Latitude Decimal" %in% names(df) && "Latitude" %in% names(df)) {
    df$`Latitude Decimal` <- to_decimal_loc(df$Latitude)
    message("[pivot_pilcomayo_data] created Latitude Decimal")
  }
  if (!"Longitude Decimal" %in% names(df) && "Longitude" %in% names(df)) {
    df$`Longitude Decimal` <- to_decimal_loc(df$Longitude)
    message("[pivot_pilcomayo_data] created Longitude Decimal")
  }
  
  id_cols <- c(
    "data_source",
    "Station", "Date", "Time", "Year", "Campaign", "Institution", "River",
    "Latitude", "Longitude", "Latitude Decimal", "Longitude Decimal",
    "Decimal latitude", "Decimal longitude",
    "Distance from Bank", "Sieve Size"
  )
  
  missing_ids <- setdiff(id_cols, names(df))
  if (length(missing_ids)) {
    message("[pivot_pilcomayo_data] missing ID cols: ", paste(missing_ids, collapse = ", "))
    for (nm in missing_ids) df[[nm]] <- NA
  }
  
  other_cols <- setdiff(names(df), id_cols)
  
  non_analyte_cols <- EXCLUDED_COLS
  
  value_cols <- setdiff(other_cols, non_analyte_cols)
  value_cols <- intersect(value_cols, names(df))
  
  message("[pivot_pilcomayo_data] value_cols count = ", length(value_cols))
  if (!length(value_cols)) stop("[pivot_pilcomayo_data] no value columns to pivot")
  
  df <- df[, c(id_cols, value_cols), drop = FALSE]
  
  df2 <- df %>%
    mutate(across(all_of(value_cols), ~ suppressWarnings(as.numeric(as.character(.)))))
  
  df_long <- df2 %>%
    pivot_longer(
      cols = all_of(value_cols),
      names_to = "raw_name",
      values_to = "concentration",
      values_drop_na = TRUE
    )
  
  message("[pivot_pilcomayo_data] after pivot_longer dim = ", nrow(df_long), " x ", ncol(df_long))
  if (nrow(df_long) == 0) stop("[pivot_pilcomayo_data] pivot_longer produced 0 rows")
  
  parsed_dates <- safe_parse_dates(df_long$Date)
  
  df_long <- df_long %>%
    mutate(
      fraction = case_when(
        str_detect(raw_name, regex("\\bSuspended\\b", ignore_case = TRUE)) ~ "Suspended",
        str_detect(raw_name, regex("\\bDissolved\\b", ignore_case = TRUE)) ~ "Dissolved",
        str_detect(raw_name, regex("\\bTotal\\b", ignore_case = TRUE)) ~ "Total",
        TRUE ~ NA_character_
      ),
      clean_name = str_remove_all(raw_name, regex("\\b(Total|Suspended|Dissolved)\\b", ignore_case = TRUE)),
      parameter = str_squish(str_remove(clean_name, "\\(.*\\)$")),
      unit_blob = dplyr::coalesce(stringr::str_match(clean_name, "\\((.*)\\)")[, 2], ""),
      unit = str_extract(unit_blob, "^[^ ]+(/[^ ]+)?"),
      media = media_type,
      date = parsed_dates,
      year = suppressWarnings(lubridate::year(date)),
      unit = case_when(
        str_detect(parameter, regex("^pH$", ignore_case = TRUE)) & (is.na(unit) | unit == "") ~ "pH units",
        TRUE ~ unit
      ),
      unit = str_replace_all(unit, fixed("°"), ""),
      unit = str_trim(unit),
      unit = case_when(
        str_detect(unit, regex("g\\s*/\\s*100|g\\s*per\\s*100", ignore_case = TRUE)) ~ "%",
        TRUE ~ unit
      ),
      unit = str_trim(unit),
      station = .data[["Station"]],
      latitude_decimal = .data[["Latitude Decimal"]],
      longitude_decimal = .data[["Longitude Decimal"]],
      time = .data[["Time"]],
      campaign = .data[["Campaign"]],
      institution = .data[["Institution"]],
      river = .data[["River"]],
      distance_from_bank = .data[["Distance from Bank"]],
      sieve_size = .data[["Sieve Size"]]
    ) %>%
    filter(
      !parameter %in% non_analyte_cols,
      !is.na(parameter),
      parameter != ""
    )
  
  message("[pivot_pilcomayo_data] unique parameters:")
  params <- sort(unique(df_long$parameter))
  print(params)
  
  bad_params <- params[params %in% c(
    "Lat_dd", "Long_dd",
    "Latitude Decimal", "Longitude Decimal",
    "Decimal latitude", "Decimal longitude",
    "Latitude", "Longitude",
    "Station", "Date", "Time", "Campaign", "Institution",
    "River", "Year", "Distance from Bank", "Sieve Size",
    "data_source"
  )]
  
  if (length(bad_params) > 0) {
    message("[pivot_pilcomayo_data] suspicious parameters detected:")
    print(bad_params)
  }
  
  message("[pivot_pilcomayo_data] after cleanup dim = ", nrow(df_long), " x ", ncol(df_long))
  if (nrow(df_long) == 0) stop("[pivot_pilcomayo_data] no rows remain after cleanup")
  
  message("[pivot_pilcomayo_data] unique parameters:")
  print(sort(unique(df_long$parameter)))
  
  out <- df_long %>%
    select(
      any_of(c(
        "data_source", "station", "date", "time", "campaign", "institution",
        "river", "latitude_decimal", "longitude_decimal", "year",
        "distance_from_bank", "sieve_size",
        "parameter", "fraction", "media", "concentration", "unit"
      ))
    ) %>%
    janitor::clean_names()
  
  message("[pivot_pilcomayo_data] output dim = ", nrow(out), " x ", ncol(out))
  message("[pivot_pilcomayo_data] output names = ", paste(names(out), collapse = ", "))
  
  if (!all(c("station", "year", "parameter", "media", "concentration", "unit") %in% names(out))) {
    stop("[pivot_pilcomayo_data] missing required output columns")
  }
  if (nrow(out) == 0) stop("[pivot_pilcomayo_data] output has 0 rows")
  
  message("[pivot_pilcomayo_data] END")
  out
}
#### Helpers

# fix to UTF-8
fix_headers <- function(nms) {
  nms <- enc2utf8(nms)                        # ensure UTF-8
  nms <- stringi::stri_trans_general(nms, "Latin-ASCII")  # á->a, µ->u (we'll fix µg below)
  nms <- str_replace_all(nms, "\\s+", " ")    # collapse spaces
  nms <- str_trim(nms)
  nms
}

# handle datasets with multiple methosd for date saving
safe_parse_dates <- function(x) {
  # robust, vectorized date parser for mixed formats:
  # - Accepts Date objects, numeric Excel serials, yyyymmdd integers,
  #   dd-mm-yy, d/m/yyyy, m/d/yyyy, and similar.
  # - Expands 2-digit years to 19xx/20xx using a 50-year pivot (<=50 => 20xx).
  # - Returns Date vector (NA where parsing failed).
  require(lubridate)
  require(stringr)
  
  # coerce to character (preserve NA)
  s <- ifelse(is.na(x), NA_character_, as.character(x))
  
  out <- as.Date(rep(NA_character_, length(s)))
  
  # keep easy case: already Date
  if (inherits(x, "Date")) return(x)
  
  # helper to expand two-digit year to 4-digit (pivot 50 -> 2000 if <=50)
  expand_two_digit_year <- function(year2) {
    y <- as.integer(year2)
    if (is.na(y)) return(NA_integer_)
    if (y <= 50) return(2000 + y)
    return(1900 + y)
  }
  
  for (i in seq_along(s)) {
    si <- s[i]
    if (is.na(si) || str_trim(si) == "") { next }
    
    # 1) numeric Excel serial (typical range ~ 1:60000)
    if (grepl("^\\d+$", si)) {
      num <- suppressWarnings(as.numeric(si))
      if (!is.na(num) && num > 0 && num < 60000) {
        # treat as Excel serial
        out[i] <- as.Date(num, origin = "1899-12-30")
        next
      }
      # treat 8-digit like yyyymmdd
      if (nchar(si) == 8) {
        maybe <- suppressWarnings(ymd(si))
        if (!is.na(maybe)) {
          out[i] <- maybe
          next
        }
      }
    }
    
    # 2) replace separators into single '/' to simplify
    s2 <- str_replace_all(si, "[\\.\\-\\s]+", "/")
    
    parts <- str_split(s2, "/", simplify = TRUE)
    if (ncol(parts) == 3) {
      p1 <- parts[1]; p2 <- parts[2]; p3 <- parts[3]
      # If year component is 2-digit, expand
      if (nchar(p3) == 2) {
        p3 <- as.character(expand_two_digit_year(p3))
      }
      # Recompose candidate strings in two interpretations:
      cand1 <- paste(p1, p2, p3, sep = "/") # assume d/m/y
      cand2 <- paste(p2, p1, p3, sep = "/") # assume m/d/y
      
      # try dmy then mdy then ymd
      parsed <- suppressWarnings(parse_date_time(cand1, orders = c("dmy","mdy","ymd"), quiet = TRUE))
      if (!is.na(parsed)) {
        out[i] <- as.Date(parsed)
        next
      }
      parsed2 <- suppressWarnings(parse_date_time(cand2, orders = c("mdy","dmy","ymd"), quiet = TRUE))
      if (!is.na(parsed2)) {
        out[i] <- as.Date(parsed2)
        next
      }
    }
    
    # 3) fallback: try lubridate parse_date_time with many orders
    parsed_any <- suppressWarnings(parse_date_time(si, orders = c("dmy","mdy","ymd","bdY","Ymd"), quiet = TRUE))
    if (!is.na(parsed_any)) {
      out[i] <- as.Date(parsed_any)
      next
    }
    
    # 4) final fallback: try common explicit formats
    candidates <- c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y","%d/%m/%y","%m/%d/%y")
    parsed_f <- NA
    for (fmt in candidates) {
      parsed_try <- suppressWarnings(as.Date(si, format = fmt))
      if (!is.na(parsed_try)) { parsed_f <- parsed_try; break }
    }
    out[i] <- parsed_f
  }
  
  out
}

# when taking in data from clean_style, make sure we call it with following flags
# clean_data = read.csv(here::here("data/water_clean_2016.csv"), check.names=FALSE)

## Convert a single vector of lat/lon values (various string formats or numeric) to decimal degrees
to_decimal_loc <- function(x) {
  # fast return for numeric vectors
  if (is.numeric(x)) return(as.numeric(x))
  x_orig <- x
  x <- as.character(x)
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  
  # Normalize characters commonly used: degree/min/second symbols, commas
  x <- str_replace_all(x, "[\u00B0\u00BA]", "°")     # degree symbols -> °
  x <- str_replace_all(x, "[\u2032\u2033\u2035\u2036'’`]", "'") # minute-like -> '
  x <- str_replace_all(x, "[\u201D\u201E\u201C\"”]", "\"")      # second-like -> "
  x <- str_replace_all(x, ",", ".")  # comma decimal -> dot
  x <- str_replace_all(x, "\\s+±\\s+.*$", "") # drop ± notes
  x <- str_squish(x)
  
  # find direction letters (N S E W); also accept trailing/leading
  dir <- str_extract(x, "(?i)[NSEW]$")
  dir2 <- str_extract(x, "(?i)^[NSEW]")
  dir_final <- ifelse(!is.na(dir), dir, dir2)
  dir_final <- toupper(dir_final)
  
  # remove non-numeric / non-separators except + - . ' " and numbers (keep signs)
  # but keep the direction info we already extracted
  x_clean <- str_replace_all(x, "(?i)[NSEW]", "")
  x_clean <- str_replace_all(x_clean, "[^0-9+\\-\\.\\'\" ]+", " ")
  x_clean <- str_squish(x_clean)
  
  # Extract numeric tokens from each entry
  tokens_list <- str_extract_all(x_clean, "[+-]?\\d+\\.?\\d*")
  
  convert_one <- function(tokens, dirchar, original) {
    if (is.na(original) || original == "") return(NA_real_)
    if (length(tokens) == 0) return(NA_real_)
    nums <- as.numeric(tokens)
    # if single numeric -> assume decimal degrees already
    if (length(nums) == 1) {
      dd <- nums[1]
    } else if (length(nums) == 2) {
      # deg + minutes
      dd <- nums[1] + nums[2] / 60
    } else {
      # deg + minutes + seconds (if more than 3 we only use first 3)
      dd <- nums[1] + nums[2] / 60 + nums[3] / 3600
    }
    # sign handling: explicit sign on degrees overrides dirchar
    if (grepl("^-", tokens[1])) {
      dd <- -abs(dd)
    } else if (!is.na(dirchar) && dirchar %in% c("S", "W")) {
      dd <- -abs(dd)
    }
    dd
  }
  
  res <- mapply(convert_one, tokens_list, dir_final, x_orig, SIMPLIFY = TRUE)
  as.numeric(-res)
}
