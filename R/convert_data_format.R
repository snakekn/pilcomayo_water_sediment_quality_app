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

upload_sampled_data = function(sample_data, media = NA, debug_prepped = FALSE, format = NA, src_lang = NA, target_lang = NA) {

  # in case we have weird non-UTF-8 characters
  names(sample_data) <- fix_headers(names(sample_data))

  # take raw pilco data and turn it into prepped clean version
  # will only not run if we're sending in pre-cleaned data, which shouldn't happen in production
  
  print("[upload_sampled_data] about to clean_water_data")
  if(format=="pilco" && !debug_prepped && media=="water") {
    sample_data = clean_water_data(sample_data, source=format)
  } else if (format=="pilco" && !debug_prepped && media=="sediment") {
    sample_data = clean_sediment_data(sample_data, source=format)
  } else { message("[upload_sampled_data] Reached an unexpected else") }
  
  # translate file to appropriate language - should probably make everything english, handle, then revert to es as desired in the front-facing app
  # english for backend work, es/en for front-end
  translated_data = translate_pilco_data(sample_data, src_lang, target_lang, media)
  View(translated_data) 
  
  if(format=="pilco") {
    formatted_data = pivot_pilcomayo_data(translated_data, media_type = media)
  } else if (format=="parameter_long") { formatted_data = translated_data} # we can just start from per-parameter if someone has it
  else { abort("Data format not recognized. Please check your file and try again")
  }
  print("showing notification that upload processed successfully")
  showNotification("Upload processed successfully!", type = "message")
  return(formatted_data)
}

pivot_pilcomayo_data <- function(df, id_cols_num = length(ID_COLS), media_type = NA, date_format = "mdy") {
  if (is.null(df) || !is.data.frame(df)) stop("df NULL/not data.frame")
  if (!"data_source" %in% names(df)) df$data_source <- NA_character_
  # force data_source to be first so id_cols catches it easily
  df <- dplyr::relocate(df, data_source, .before = 1)
  
  if(media_type == "sediment") {
    # replace ID_COLS with sediment specific one
    ID_COLS = c("data_source",
                "Station","Date","Time","Campaign","Institution",
                "River","Latitude","Longitude","Latitude Decimal","Longitude Decimal",
                "Year", "Distance from Bank", "Sieve Size")
    
    # also: replace g/100 with %, get decimal location, 
  }
  # create lat & lon in decimal if it doesn't already exist
  if(!"Latitude Decimal" %in% names(df) || !"Longitude Decimal" %in% names(df)) {
    df$"Latitude Decimal" = to_decimal_loc(df$Latitude)
    df$"Longitude Decimal" = to_decimal_loc(df$Longitude)
  }
  # recalc id columns (ensure data_source included)
  present_ids <- intersect(ID_COLS, names(df))
  missing_ids <- setdiff(ID_COLS, names(df))
  
  src_label <- if (!is.na(unique(df$data_source)[1])) paste0(" [", unique(df$data_source)[1], "]") else ""
  if (length(missing_ids)) {
    message("[pivot_pilcomayo_data]", src_label, " — missing ID columns: ", paste(missing_ids, collapse = ", "))
    # create missing ID cols (NA) so schema is consistent
    for (nm in missing_ids) df[[nm]] <- NA
  } else {
    message("[pivot_pilcomayo_data]", src_label, " — all ID columns present")
  }
  
  # final id column order (data_source kept first)
  id_cols <- unique(c("data_source", ID_COLS))
  # reorder df so ID columns are first, then everything else in original order
  other_cols <- setdiff(names(df), id_cols)
  df <- df[, c(id_cols, other_cols), drop = FALSE]
  
  # value columns = everything after the id block
  value_cols <- other_cols
  if (!length(value_cols)) abort("[pivot_pilcomayo_data] No value columns to pivot")

  # coerce numeric on value columns (safely)
  df2 <- df %>%
    mutate(across(any_of(value_cols), ~ suppressWarnings(as.numeric(as.character(.)))))
  
  # prep CR route
  cr_route = switch(media_type,
                    "water" = "oral",
                    "sediment" = "oral",
                    default = NA
  )
  
  # pivot
  df_long <- df2 %>%
    pivot_longer(cols = any_of(value_cols),
                 names_to = "raw_name",
                 values_to = "concentration",
                 values_drop_na = TRUE)
  
  date_lubridated = switch(media_type,
                              "water" = df_long$Date,
                              "sediment" = lubridate::ymd(df_long$Date),
                              .default = NULL)
  
  # parse and tidy
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
      unit_blob = stringr::str_match(clean_name, "\\((.*)\\)")[,2] %>% coalesce(""),
      unit = str_extract(unit_blob, "^[^ ]+(/[^ ]+)?"),
      media = media_type,
      Date = date_lubridated, ## NOTE: ymd ONLY WORKS WITH SED DATA. WATER DATA USES MDY. CAN'T IFELSE :/
      Year = lubridate::year(Date),
      unit = case_when(
        str_detect(parameter, regex("^pH$", ignore_case = TRUE)) & is.na(unit) ~ "pH units",
        TRUE ~ unit
      ),
      unit = str_replace_all(unit, fixed("°"), ""),
      unit = str_trim(unit),
      unit = case_when(
        str_detect(unit, regex("astm\\)", ignore_case = TRUE))            ~ "%",
        str_detect(unit, regex("g\\s*/\\s*100|g\\s*per\\s*100", ignore_case = TRUE)) ~ "%",
                         TRUE ~ unit  # fallback: keep original cleaned string
      ),
      unit = str_trim(unit),
      cr_route = cr_route
    )
  
  # remove odd parameters that aren't supposed to be there, manually.
  df_long |> filter(!parameter %in% c("Lat_dd", "Long_dd"))
  
  
  # keep id cols + standardized columns, preserve data_source
  keep <- c(intersect(id_cols, names(df_long)), "Date", "Year", "parameter", "fraction", "media", "concentration", "unit", "cr_route")
  df_long %>% select(all_of(keep)) %>% janitor::clean_names()
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
