# ---------------- One-time reads (pure; OK outside server) ----------------
pilco_line   <- sf::st_read("data/geojson/pilco_line.geojson")
bol_border   <- sf::st_read("data/geojson/bol_borders.geojson")

usgs_sqg     <- readr::read_csv("data/standards/USGS_SQG.csv") |>
  dplyr::mutate(match_name = c(
    "Arsenic (mg/kg As)","Cadmium (mg/kg Cd)","Copper (mg/kg Cu)",
    "Chromium (mg/kg Cr)","Lead (mg/kg Pb)","Mercury (mg/kg Hg)",
    "Nickel (mg/kg Ni)","Zinc (mg/kg Zn)"
  ))

bolivian_1333 <- readr::read_csv("data/standards/bolivian_standards_1333.csv") |>
  dplyr::mutate(match_name = c(
    "pH","pH","Color (u PtCo)","Total Dissolved Solids (mg/l)",
    "Oxygen Saturation (%)","Biochemical Oxygen Demand (mg/l O2)",
    "Chemical Oxygen Demand (mg/l O2)", NA, NA, NA,
    "Total Arsenic (ug/l As)", NA, NA, "Total Boron (ug/l B)",
    "Total Cadmium (ug/l Cd)","Total Calcium (mg/l Ca)",
    "Chlorides (mg/l Cl-)","Total Chromium (ug/l Cr)","Total Chromium (ug/l Cr)",
    NA,"Total Copper (ug/l Cu)","Total Iron (ug/l Fe)","Total Lead (ug/l Pb)", NA,
    "Total Magnesium (mg/l Mg)","Total Manganese (ug/l Mn)","Total Mercury (mg/l Hg)",
    "Total Nickel (ug/l Ni)","Nitrate (mg/l NO3)","Total Kjeldahl Nitrogen (mg/l N)",
    "Total Phosphorus (mg/l PO4)","Total Selenium (ug/l Se)","Total Silver (ug/l Ag)",
    "Total Sodium (mg/l Na)","Sulfates (mg/l SO4)", NA, NA, NA, "Total Zinc (ug/l Zn)"
  ))

# ---------------- Helpers used by loaders (pure) ----------------
.read_year_batch <- function(dir, pattern, date_fmt, station_fix = TRUE) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  dfs <- lapply(files, function(f) {
    yr <- stringr::str_extract(basename(f), "\\d{4}")
    df <- readxl::read_xlsx(f)
    df$Year <- as.integer(yr)
    if (!is.null(date_fmt)) df$Date <- as.Date(df$Date, date_fmt)
    df
  })
  out <- dplyr::bind_rows(dfs)
  if (station_fix) {
    out <- out |>
      dplyr::mutate(Station = stringr::str_replace(
        Station,"Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba","Tacobamba arriba Pilcomayo")) |>
      dplyr::mutate(Station = stringr::str_replace(
        Station,"Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba","Pilcomayo arriba Tacobamba"))
  }
  out
}

.reconcile_legacy_names <- function(df) {
  # make sure maps/downloads see the legacy coord/station/date keys
  rename_map <- c(
    "Decimal latitude"  = "Latitude Decimal",
    "Decimal longitude" = "Longitude Decimal",
    "Latitud Decimal"   = "Latitude Decimal",
    "Longitud Decimal"  = "Longitude Decimal",
    "Estación"          = "Station",
    "Fecha"             = "Date",
    "Hora"              = "Time",
    "Campaña"           = "Campaign"
  )
  hit <- intersect(names(rename_map), names(df))
  names(df)[match(hit, names(df))] <- rename_map[hit]
  df
}

# ---------------- Loader functions (return PLAIN tibbles) ----------------

# Uses your inline cleaner/translator (you already inlined those in global).
load_all_water_clean_tbl <- function(target_lang = "en") {
  fp <- file.path(water_data_path_clean, "water_clean_master.xlsx")
  df <- load_water_data(path = fp, is.clean = FALSE, translate_to = target_lang)
  df <- .reconcile_legacy_names(df)
  
  if (!inherits(df$Date, "Date")) {
    suppressWarnings({
      a <- try(as.Date(df$Date, "%Y-%m-%d"))
      b <- try(as.Date(df$Date, "%d/%m/%Y"))
      if (all(!is.na(a))) df$Date <- a else if (all(!is.na(b))) df$Date <- b else df$Date <- as.Date(df$Date)
    })
  }
  df$Year <- as.integer(format(df$Date, "%Y"))
  df$Station <- gsub("Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba","Tacobamba arriba Pilcomayo", df$Station, fixed = TRUE)
  df$Station <- gsub("Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba","Pilcomayo arriba Tacobamba", df$Station, fixed = TRUE)
  
  if ("Latitude Decimal" %in% names(df)) {
    df <- df[!is.na(df[["Latitude Decimal"]]), , drop = FALSE]
  }
  df
}

load_all_water_1333_tbl <- function() {
  dat <- .read_year_batch(water_data_path_1333, "^water_\\d{4}_1333\\.xlsx$", "%Y-%m-%d") |>
    dplyr::filter(!is.na(`Latitude Decimal`))
  cls <- dplyr::select(dat, tidyselect::ends_with("Class"))
  dat$num_unclass <- rowSums(cls == "Unclassified", na.rm = TRUE)
  dat$num_class_d <- rowSums(cls == "Class D",    na.rm = TRUE)
  dat$num_class_c <- rowSums(cls == "Class C",    na.rm = TRUE)
  dat$num_class_b <- rowSums(cls == "Class B",    na.rm = TRUE)
  dat
}

load_all_sed_clean_tbl <- function() {
  .read_year_batch(sed_data_path_clean, "^sed_\\d{4}_clean\\.xlsx$", "%d/%m/%Y", station_fix = TRUE)
}

load_all_sed_usgs_tbl <- function() {
  df <- .read_year_batch(sed_data_path_usgs, "^sed_\\d{4}_usgs\\.xlsx$", "%d/%m/%Y") |>
    dplyr::left_join(
      dplyr::select(load_all_sed_clean_tbl(), Station, Date, `Sieve Size`, `Distance from Bank`),
      by = c("Station","Date","Sieve Size")
    )
  usgs_cols <- dplyr::select(df, tidyselect::ends_with("USGS"))
  df$n_params       <- rowSums(!is.na(usgs_cols))
  df$num_above_tel  <- rowSums(usgs_cols == "Above TEL", na.rm = TRUE)
  df$num_above_pel  <- rowSums(usgs_cols == "Above PEL", na.rm = TRUE)
  df$sed_score      <- (df$num_above_tel + 2*df$num_above_pel) / df$n_params
  df$unique         <- paste(df$Station, df$Date, sep = " - ")
  df
}

# ---- optional: parameter name mapping (leave empty if you don't need translation) ----
# Example shape: param_mapping <- c("Color (u PtCo)" = "Color (u PtCo)", "pH" = "pH", ...)
if (!exists("param_mapping")) param_mapping <- c()

# ---- cleaner for TNC-style water files (header across first two rows; handles SIN DATOS and < / >) ----
clean_water_data <- function(data) {
  raw <- data
  # keep columns that have at least one non-NA (skip trailing empty columns)
  raw_clean <- raw |> dplyr::filter(dplyr::if_any(-c(1, 2), ~ !is.na(.)))
  df <- as.data.frame(t(raw_clean))
  df <- df %>% dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  
  new_names <- ifelse(!is.na(df[2, ]), paste0(df[1, ], " (", df[2, ], ")"), df[1, ])
  colnames(df) <- new_names
  df <- df[-c(1, 2), , drop = FALSE]
  
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ na_if(., "SIN DATOS"))) %>%
    dplyr::mutate(dplyr::across(
      where(~ any(grepl("^[<>]", .[!is.na(.)]))),
      ~ dplyr::case_when(
        grepl("^<", .) ~ 0.5 * as.numeric(gsub("^<", "", .)),
        grepl("^>", .) ~ 1.5 * as.numeric(gsub("^>", "", .)),
        TRUE ~ suppressWarnings(as.numeric(.))
      )
    ))
  df
}

# ---- ES<->EN column translator (no-op if mapping missing) ----
translate_water_data <- function(data, source_lang = "es", target_lang = "en") {
  if (identical(source_lang, target_lang)) return(data)
  if (!length(param_mapping)) return(data)  # no mapping provided → skip
  
  if (source_lang == "es" && target_lang == "en") {
    map <- param_mapping
  } else {
    # invert mapping
    map <- stats::setNames(names(param_mapping), unname(param_mapping))
  }
  nm <- colnames(data)
  colnames(data) <- ifelse(nm %in% names(map), unname(map[nm]), nm)
  data
}

# path: .csv or .xlsx. If xlsx is a raw TNC sheet, set is.clean = FALSE to run the cleaner.
# translate_to: "en" / "es" / NULL. If NULL, no translation.
load_water_data <- function(path, is.clean = FALSE, translate_to = NULL) {
  fmt <- tolower(tools::file_ext(path))
  if (fmt == "csv") {
    data_raw <- readr::read_csv(path, show_col_types = FALSE)
  } else if (fmt %in% c("xlsx", "xls")) {
    # TNC xlsx often ships without headers in row 1; we read raw to allow cleaning
    data_raw <- readxl::read_xlsx(path, col_names = !isTRUE(!is.clean))
  } else {
    stop("Unsupported file type: ", fmt)
  }
  
  df <- if (is.clean) data_raw else clean_water_data(data_raw)
  
  if (!is.null(translate_to)) {
    src <- if (translate_to == "en") "es" else "en"
    df <- translate_water_data(df, source_lang = src, target_lang = translate_to)
  }
  df
}
