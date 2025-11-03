### manually save all water files into a single file (per type), using jackson's _clean files
# not to be run in shiny during production, just to prep the files

# --- 1) Header normalizer -----------------------------------------------------
standardize_headers <- function(nms) {
  # strip odd chars/whitespace first
  nms <- gsub("°", "", nms, fixed = TRUE)
  nms <- gsub("\\s+", " ", nms)
  nms <- trimws(nms)
  
  # canonical map for known variants (add to this over time)
  canon <- c(
    "Temperature (C)"                            = "Temperature (C)",
    "Specific Conductivity (T ref. 25C) (uS/cm)" = "Specific Conductivity (T ref. 25C) (uS/cm)",
    "Cyanides (mg/l CN-)"                        = "Cyanides (mg/l CN-)",
    "Free Cyanides (mg/l CN-)"                   = "Cyanides (mg/l CN-)", # collapse to one
    # Mesophilic aerobes: keep two canonical columns (CFU vs MPN)
    "Total Mesophilic Aerobes (CFU) (CFU/100 ml)" = "Total Mesophilic Aerobes (CFU) (CFU/100 ml)",
    "Total Mesophilic Aerobes (MPN) (MPN/100 ml)" = "Total Mesophilic Aerobes (MPN) (MPN/100 ml)",
    "Total Coliforms (CFU) (CFU/100 ml)"          = "Total Coliforms (CFU) (CFU/100 ml)",
    "Fecal Coliforms (CFU) (CFU/100 ml)"          = "Fecal Coliforms (CFU) (CFU/100 ml)",
    "Total Coliforms (MPN) (MPN/100 ml)"          = "Total Coliforms (MPN) (MPN/100 ml)",
    "Fecal Coliforms (MPN) (MPN/100 ml)"          = "Fecal Coliforms (MPN) (MPN/100 ml)"
  )
  
  # apply map where we know it
  hit <- match(nms, names(canon), nomatch = 0L)
  nms[hit > 0] <- unname(canon[hit])
  
  nms
}

# --- 2) Light type coercion for key columns -----------------------------------
.coerce_key_types <- function(df) {
  # Station
  if ("Station" %in% names(df)) df$Station <- as.character(df$Station)
  
  # Coords
  for (nm in c("Latitude Decimal","Longitude Decimal","Lat_dd","Long_dd")) {
    if (nm %in% names(df) && !is.numeric(df[[nm]])) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }
  }
  df
}

# --- 3) Align columns across data frames (union; keep order of first) ---------
.align_cols <- function(dfs) {
  all_cols <- Reduce(union, lapply(dfs, names))
  lapply(dfs, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  })
}

# --- 4) Reader (path or already-loaded df) ------------------------------------
.read_any <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  # character path-like
  if (length(x) == 1L && is.character(x)) {
    ext <- tolower(tools::file_ext(x))
    if (ext %in% c("csv","tsv")) {
      dialect <- if (ext == "csv") readr::read_csv else readr::read_tsv
      return(dialect(x, show_col_types = FALSE))
    } else if (ext %in% c("xlsx","xls")) {
      return(readxl::read_xlsx(x))
    } else {
      stop("Unsupported file type: ", ext)
    }
  }
  stop("Unsupported input to .read_any()")
}

# --- 5) Main: merge any number of files with schema-flex ----------------------
# for each file...
merge_measurement_files <- function(files_or_dfs) {
  # If they're file paths, set names to file paths
  if (is.character(files_or_dfs)) {
    names(files_or_dfs) <- files_or_dfs
  } else if (is.list(files_or_dfs) && is.null(names(files_or_dfs))) {
    # Give unnamed list elements synthetic names
    names(files_or_dfs) <- paste0("df_", seq_along(files_or_dfs))
  }
  
  # load everything
  lst <- lapply(files_or_dfs, .read_any)
  
  # drop NULLs, standardize headers, reconcile names, coerce types
  lst <- Map(function(df, nm) {
    if (is.null(df)) return(NULL)
    names(df) <- standardize_headers(names(df))

    df <- .coerce_key_types(df)
    df$data_source <- basename(nm)   # now we can safely use the name
    df$Date = as.Date(df$Date, format = "%d/%m/%Y")
    str(df$Date)
    df
  }, lst, names(lst))
  
  # drop any NULLs
  lst <- Filter(Negate(is.null), lst)
  if (!length(lst)) return(tibble::tibble())
  
  # align unknown/missing columns -> NA, then bind
  aligned <- .align_cols(lst)
  dplyr::bind_rows(aligned)
}


# take in a whole folder of data
merge_measurement_folder <- function(folder_path) {
  # get all Excel/CSV files in that directory. If merged files exist, manually delete those before starting
  files <- list.files(
    path = folder_path,
    pattern = "\\.(csv|tsv|xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(files)) stop("No data files found in ", folder_path)
  
  # filter out junk: Excel lock files (~$...), hidden dotfiles, zero-byte files
  base <- basename(files)
  keep <- !grepl("^~\\$", base) &            # Excel lock files
    !grepl("^\\.",   base)             # hidden dotfiles like .DS_Store
  if (any(!keep)) {
    message("Skipping junk files: ", paste(base[!keep], collapse = ", "))
  }
  files <- files[keep]
  
  # also drop zero-byte files
  infos <- file.info(files)
  files <- files[infos$size > 0]
  if (!length(files)) stop("All files were junk or empty in ", folder_path)
  
  
  message("Merging ", length(files), " files from: ", folder_path)
  merge_measurement_files(files)
}

##### See if there are repeat columns ####
normalize_headers <- function(h) {
  h <- iconv(as.character(h), to = "UTF-8", sub = "") 
  h <- trimws(h)
  h <- gsub("[\r\n\t]+", " ", h)
  h <- gsub("\\s+", " ", h)
  h <- gsub("\u00B0", "", h, fixed = TRUE)   # °
  h <- gsub("\u00B5", "u", h, fixed = TRUE)  # µ -> u
  h <- gsub("°C", "C", h, fixed = TRUE)
  h <- gsub("\\(\\s+", "(", h); h <- gsub("\\s+\\)", ")", h)
  h <- gsub("\\s*/\\s*", "/", h)
  trimws(h)
}

base_name_from_header <- function(h) {
  hn <- normalize_headers(h)
  bn <- gsub("\\([^)]*\\)", "", hn)   # drop parenthetical unit blocks
  bn <- gsub("\\s+", " ", bn)
  trimws(bn)
}

# Read just the header names from a file path (safe, returns character vector or NULL)
.read_headers_from_path <- function(path) {
  if (!file.exists(path)) return(NULL)
  # skip temp / hidden files
  bn <- basename(path)
  if (grepl("^~\\$|^\\.", bn)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    if (ext %in% c("xlsx","xls")) {
      df0 <- readxl::read_xlsx(path, n_max = 0, .name_repair = "minimal")
      names(df0)
    } else if (ext %in% c("csv","tsv","txt")) {
      # readr n_max=0 returns column names
      df0 <- readr::read_csv(path, n_max = 0, show_col_types = FALSE)
      names(df0)
    } else {
      # unsupported file extension -> skip
      NULL
    }
  }, error = function(e) {
    warning("Failed to read headers from: ", path, " : ", conditionMessage(e))
    NULL
  })
}
find_merge_candidates_from_files <- function(files_or_dfs) {
  # normalize input to a named list where names are file identifiers
  if (is.character(files_or_dfs)) {
    files <- files_or_dfs
    # use basename so names are just the filename (no path)
    names(files) <- basename(files)
    # read headers only
    header_list <- lapply(files, .read_headers_from_path)
  } else if (is.list(files_or_dfs)) {
    # list of data.frames (or tibbles)
    if (is.null(names(files_or_dfs))) {
      names(files_or_dfs) <- paste0("df_", seq_along(files_or_dfs))
    }
    # ensure names are simple (if they were file paths, convert to basename)
    names(files_or_dfs) <- basename(names(files_or_dfs))
    header_list <- lapply(files_or_dfs, function(df) {
      if (is.data.frame(df)) names(df) else NULL
    })
  } else {
    stop("files_or_dfs must be a character vector of paths or a list of data.frames.")
  }
  
  # flatten to tibble of (col_name, file) with col_name first
  rows <- lapply(names(header_list), function(fn) {
    cols <- header_list[[fn]]
    if (is.null(cols) || length(cols) == 0) return(NULL)
    tibble::tibble(col_name = as.character(cols), file = fn)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    message("No readable headers found in input.")
    return(list())
  }
  hdr_tbl <- dplyr::bind_rows(rows)
  
  # compute base names
  hdr_tbl <- hdr_tbl %>%
    mutate(base = base_name_from_header(col_name)) %>%
    filter(nzchar(base))   # drop purely-empty bases
  
  # group by base and keep only those with >1 distinct column name
  groups <- hdr_tbl %>%
    group_by(base) %>%
    summarize(n_names = n_distinct(col_name),
              rows = list(tibble(col_name = col_name, file = file)),
              .groups = "drop") %>%
    filter(n_names > 1)
  
  if (!nrow(groups)) {
    message("No candidate groups found (no base name had >1 distinct header).")
    return(list())
  }
  
  # assemble named list: base -> tibble(col_name, file) with col_name first
  out <- setNames(groups$rows, groups$base)
  
  # print friendly summary with col_name first
  for (b in names(out)) {
    cat("\n-- Base name:", b, "\n")
    print(out[[b]])
  }
  
  invisible(out)
}


#### merge clean data files ####

# merge the files - water
merged_df_water <- merge_measurement_folder(here::here("data/water/clean"))
# save to a file we can look at later
save_path_water = here::here("data/merged_water_clean.csv")
dir.create(dirname(save_path_water), recursive = TRUE, showWarnings = FALSE)

write_csv(merged_df_water, file=save_path_water)

# merge the files - sediment
merged_df_sed <- merge_measurement_folder(here::here("data/sed/clean"))
# save to a file we can look at later
save_path_sed = here::here("data/merged_sed_clean.csv")
dir.create(dirname(save_path_sed), recursive = TRUE, showWarnings = FALSE)

write_csv(merged_df_sed, file=save_path_sed)

## checks!

# check if any columns are completely NA -- for sanity!
merged_df = merged_df_water
names(merged_df)[vapply(merged_df, function(col) all(is.na(col)), logical(1))] # char(0) means no columns!

# print column names 
print(names(merged_df))
# check for similar columns
files <- list.files(here::here("data/sed/clean"), full.names = TRUE)
cands = find_merge_candidates_from_files(files)

#### convert data into by_parameter type ####
pivot_merged_samples = function(merged_df, media, date_format = "mdy") {
  pivot_pilcomayo_data(merged_df, media_type = media, date_format=date_format)
}

# pivot the data & include the pivot method
pivoted_water = pivot_merged_samples(merged_df_water, "drinking water")
pivoted_sed = pivot_merged_samples(merged_df_sed, "sediment", date_format="ymd")

#### calculate HQ & CR from each media ####
water_scored = score_data(pivoted_water)
sed_scored = score_data(pivoted_sed)

# save these scored files
saveRDS(water_scored, "data/processed/water_scored.rds")
saveRDS(sed_scored, "data/processed/sed_scored.rds")

# pull them up -- can place into the code
water_scored_data = readRDS(here::here("data/processed/water_scored.rds"))
sed_scored_data = readRDS(here::here("data/processed/sed_scored.rds"))

## save by locyear - from get_risk_scores.R
water_locyear = score_by_loc_year(pivoted_water, loc_col = "station", year_col = "year", lat_col = "latitude_decimal", lon_col = "longitude_decimal")
sed_locyear = score_by_loc_year(pivoted_sed, loc_col = "station", year_col = "year", lat_col = "latitude_decimal", lon_col = "longitude_decimal")
