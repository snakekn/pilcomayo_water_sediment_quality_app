library(dplyr)
library(purrr)
library(tidyr)
library(rlang)

# ---------- Helpers ----------
safe_num <- function(x) as.numeric(gsub("[,\\s()]", "", as.character(x)))

make_param_id <- function(df, param_key_cols = c("parameter","media","unit","cr_route")) {
  df2 <- df %>% mutate(across(all_of(param_key_cols), ~ as.character(.x)))
  apply(df2[, param_key_cols, drop = FALSE], 1, function(r) paste0(replace(r, is.na(r), ""), collapse = "|"))
}


#### Merge different scored dfs together, save all rows & cols ####
# used by the upload function
merge_scored <- function(existing,
                         uploaded,
                         key_cols = c("station", "year"),
                         param_key_cols = c("parameter", "media", "unit", "cr_route"),
                         replace = FALSE,          # if TRUE uploaded rows replace matching existing rows
                         keep_param_id = FALSE) { # whether to keep helper param_id column in returned df
  
  # basic checks
  if (!is.data.frame(existing) || !is.data.frame(uploaded)) stop("existing and uploaded must be data.frames/tibbles")
  
  # ensure param key columns exist in both frames (fill with NA if missing)
  for (pc in param_key_cols) {
    if (!pc %in% names(existing)) existing[[pc]] <- NA_character_
    if (!pc %in% names(uploaded)) uploaded[[pc]] <- NA_character_
  }
  
  # helper to make a stable param_id (replace NA with "")
  make_param_id <- function(df, keys) {
    # coerce to character and replace NA with ""
    vals <- lapply(df[keys], function(col) if (is.factor(col)) as.character(col) else as.character(col))
    vals <- lapply(vals, function(v) ifelse(is.na(v), "", v))
    # paste with separator unlikely to appear in values
    do.call(paste0, c(vals, sep = ""))
  }
  
  existing <- as_tibble(existing)
  uploaded <- as_tibble(uploaded)
  
  existing$param_id <- make_param_id(existing, param_key_cols)
  uploaded$param_id <- make_param_id(uploaded, param_key_cols)
  
  # determine join keys we will use: key_cols that exist in both inputs
  join_keys <- intersect(key_cols, intersect(names(existing), names(uploaded)))
  if (length(join_keys) == 0) {
    warning("No matching key_cols found in both data.frames. De-duplication will be by param_id only.")
    join_keys <- character(0)
  }
  
  # build the full dedupe key (join_keys + param_id)
  dedupe_keys <- c(join_keys, "param_id")
  
  if (replace && nrow(uploaded) > 0) {
    # anti-join: keep existing rows that DO NOT match uploaded on dedupe_keys
    existing_keep <- if (length(dedupe_keys) == 0) existing else {
      anti_join(existing, uploaded %>% select(all_of(dedupe_keys)) %>% distinct(), by = dedupe_keys)
    }
  } else {
    existing_keep <- existing
  }
  
  merged <- bind_rows(existing_keep, uploaded)
  
  # restore original column ordering (param_id optional)
  if (!keep_param_id && "param_id" %in% names(merged)) merged <- merged %>% select(-param_id)
  
  # arrange by join keys if present (stable output)
  if (length(join_keys) > 0) merged <- merged %>% arrange(across(all_of(join_keys)))
  
  # informational message
  message("merge_scored: existing_rows=", nrow(existing), " uploaded_rows=", nrow(uploaded), " result_rows=", nrow(merged),
          ifelse(replace, " (replace=TRUE)", ""))
  
  merged
}



merge_and_recompute_locyear = function(existing_locyear,
         uploaded,
         key_cols = c("station", "year"),
         param_key_cols = c("parameter","media","unit","cr_route"),
         detail_col = "detail_rows",
         replace = TRUE,
         detail_id_col = NULL) {
  # ----------------- helpers -----------------
  safe_num <- function(x) {
    x <- as.character(x)
    x[x %in% c("", "NA", "na", "NaN")] <- NA_character_
    suppressWarnings(as.numeric(gsub("[,()\\s]", "", x)))
  }
  
  make_param_id <- function(df, param_key_cols) {
    if (nrow(df) == 0) return(character(0))
    df2 <- df %>% mutate(across(all_of(param_key_cols), ~ as.character(.x)))
    apply(df2[, param_key_cols, drop = FALSE], 1, function(r) paste0(replace(r, is.na(r), ""), collapse = "|"))
  }
  
  detect_col <- function(df, candidates) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    present <- intersect(candidates, names(df))
    if (length(present) == 0) return(NULL)
    present[[1]]
  }
  
  recompute_by_parameter_from_details <- function(details_df, param_key_cols) {
    # expects per-sample HQ/CR/WL columns already present in details_df (or detect candidate names)
    if (is.null(details_df) || nrow(details_df) == 0) return(tibble())
    
    # candidate names - add more if your detail_rows use other names
    HQ_cands <- c("HQ","hazard_quotient","hq","HQ_value")
    CR_cands <- c("CR_cases_10k","CR","cr_cases_10k","cr_per_10k")
    WL_cands <- c("WL","wl","wl_index","wl_value")
    
    HQ_col <- detect_col(details_df, HQ_cands)
    CR_col <- detect_col(details_df, CR_cands)
    WL_col <- detect_col(details_df, WL_cands)
    
    if (!is.null(HQ_col)) details_df[[HQ_col]] <- safe_num(details_df[[HQ_col]])
    if (!is.null(CR_col)) details_df[[CR_col]] <- safe_num(details_df[[CR_col]])
    if (!is.null(WL_col)) details_df[[WL_col]] <- safe_num(details_df[[WL_col]])
    
    # ensure all param key columns exist to avoid errors
    for (k in param_key_cols) if (!k %in% names(details_df)) details_df[[k]] <- NA_character_
    
    byp <- details_df %>%
      group_by(across(all_of(param_key_cols))) %>%
      summarise(
        HQ_max    = if (!is.null(HQ_col)) max(.data[[HQ_col]], na.rm = TRUE) else NA_real_,
        HQ_median = if (!is.null(HQ_col)) { t <- .data[[HQ_col]]; if (all(is.na(t))) NA_real_ else median(t, na.rm = TRUE) } else NA_real_,
        HQ_n      = if (!is.null(HQ_col)) sum(!is.na(.data[[HQ_col]])) else 0L,
        
        CR_max       = if (!is.null(CR_col)) max(.data[[CR_col]], na.rm = TRUE) else NA_real_,
        CR_cases_10k = if (!is.null(CR_col)) sum(.data[[CR_col]], na.rm = TRUE) else NA_real_,
        CR_n         = if (!is.null(CR_col)) sum(!is.na(.data[[CR_col]])) else 0L,
        
        WL_max    = if (!is.null(WL_col)) max(.data[[WL_col]], na.rm = TRUE) else NA_real_,
        WL_median = if (!is.null(WL_col)) { t <- .data[[WL_col]]; if (all(is.na(t))) NA_real_ else median(t, na.rm = TRUE) } else NA_real_,
        WL_n      = if (!is.null(WL_col)) sum(!is.na(.data[[WL_col]])) else 0L,
        
        n_detail_rows = n(),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))
    
    byp
  }
  
  compute_loc_scalars_from_byparam <- function(byp) {
    if (is.null(byp) || nrow(byp) == 0) {
      return(list(hazard_index = NA_real_, total_CR_cases_10k = NA_real_, wl_index = NA_real_))
    }
    
    # hazard_index: prefer explicit hazard_component if present, else sum(HQ_max)
    if ("hazard_component" %in% names(byp)) {
      hazard_index <- sum(safe_num(byp$hazard_component), na.rm = TRUE)
    } else if ("HQ_max" %in% names(byp)) {
      hazard_index <- sum(safe_num(byp$HQ_max), na.rm = TRUE)
    } else hazard_index <- NA_real_
    
    total_CR_cases_10k <- if ("CR_cases_10k" %in% names(byp)) sum(safe_num(byp$CR_cases_10k), na.rm = TRUE) else NA_real_
    
    # wl_index: conservative site-level metric -> max WL_max
    wl_index <- if ("WL_max" %in% names(byp)) max(safe_num(byp$WL_max), na.rm = TRUE) else NA_real_
    
    list(hazard_index = ifelse(is.finite(hazard_index), hazard_index, NA_real_),
         total_CR_cases_10k = ifelse(is.finite(total_CR_cases_10k), total_CR_cases_10k, NA_real_),
         wl_index = ifelse(is.finite(wl_index), wl_index, NA_real_))
  }
  
  # ----------------- normalize inputs -----------------
  if (is.null(existing_locyear) || nrow(existing_locyear) == 0) {
    existing_locyear <- tibble()
  }
  
  # if uploaded is pivoted / sample-level (has parameter col but no detail_rows),
  # nest into detail_rows grouped by key_cols
  uploaded_prep <- uploaded
  if (!is.null(uploaded) && nrow(uploaded) > 0 && !("detail_rows" %in% names(uploaded))) {
    # require that key_cols are present
    missing_keys <- setdiff(key_cols, names(uploaded))
    if (length(missing_keys)) abort(paste0("Uploaded pivoted missing grouping columns: ", paste(missing_keys, collapse = ", ")))
    uploaded_prep <- uploaded %>%
      group_by(across(all_of(key_cols))) %>%
      nest(!!detail_col := everything()) %>%
      ungroup()
  }
  
  # ensure existing_locyear has the detail_col (list of tibbles) - if not, try to create single-row groups
  if (!("detail_rows" %in% names(existing_locyear))) {
    # attempt to create locyear from raw existing_pivoted if possible (if pivoted has key_cols)
    if (!is.null(existing_locyear) && all(key_cols %in% names(existing_locyear))) {
      # if existing_locyear is actually pivoted samples, nest it
      existing_locyear <- existing_locyear %>%
        group_by(across(all_of(key_cols))) %>%
        nest(detail_rows = everything()) %>%
        ungroup()
    } else {
      # empty frame with keys
      existing_locyear <- tibble()
    }
  }
  
  # unify schemas: both should now be locyear-like with key_cols + detail_col
  # if uploaded_prep is NULL/empty set to empty tibble
  if (is.null(uploaded_prep) || nrow(uploaded_prep) == 0) {
    uploaded_prep <- tibble()
  }
  
  # ----------------- combine groups (existing + uploaded) -----------------
  # create a full key index: union of groups present
  keys_existing <- if (nrow(existing_locyear)) existing_locyear %>% select(all_of(key_cols)) else tibble()
  keys_uploaded <- if (nrow(uploaded_prep)) uploaded_prep %>% select(all_of(key_cols)) else tibble()
  all_keys <- bind_rows(keys_existing, keys_uploaded) %>% distinct()
  
  # helper to fetch detail tibble for a key
  get_details_for_key <- function(kv, df, detail_col) {
    if (nrow(df) == 0) return(tibble())
    matched <- df %>% filter(across(all_of(names(kv)), ~ .x == kv[[cur_column()]]))
    if (nrow(matched) == 0) return(tibble())
    # if matched row has the detail_col nested, return its first element
    if (detail_col %in% names(matched)) {
      dlist <- matched[[detail_col]]
      if (length(dlist) >= 1) {
        return( as_tibble(dlist[[1]]) )
      } else return(tibble())
    } else return(tibble())
  }
  # --- Replace the pmap_dfr(...) block with this map_dfr implementation ---
  
  merged_rows <- purrr::map_dfr(seq_len(nrow(all_keys)), function(i) {
    # key row as a list, e.g. list(station = "Agropil", year = 2021)
    keys <- as.list(all_keys[i, , drop = TRUE])
    
    # helper to filter a locyear-like df for this key and return the detail tibble
    fetch_details <- function(df, key_cols, detail_col) {
      if (nrow(df) == 0) return(tibble())
      f <- df
      for (kc in key_cols) {
        f <- dplyr::filter(f, !!rlang::sym(kc) == keys[[kc]])
      }
      if (nrow(f) == 0) return(tibble())
      # if detail_col exists and is a list-column then return its first element
      if (detail_col %in% names(f)) {
        dlist <- f[[detail_col]]
        if (length(dlist) >= 1 && !is.null(dlist[[1]])) return(as_tibble(dlist[[1]]))
      }
      tibble()
    }
    
    existing_details <- fetch_details(existing_locyear, key_cols, detail_col)
    uploaded_details <- fetch_details(uploaded_prep, key_cols, detail_col)
    
    # optional dedupe by sample id
    if (!is.null(detail_id_col)) {
      if (detail_id_col %in% names(existing_details)) existing_details <- dplyr::distinct(existing_details, !!rlang::sym(detail_id_col), .keep_all = TRUE)
      if (detail_id_col %in% names(uploaded_details)) uploaded_details <- dplyr::distinct(uploaded_details, !!rlang::sym(detail_id_col), .keep_all = TRUE)
    }
    
    # replacement policy: uploaded params replace existing params
    if (replace && nrow(uploaded_details) > 0 && nrow(existing_details) > 0) {
      up_ids <- make_param_id(uploaded_details, param_key_cols)
      if (length(up_ids) > 0) {
        ex_ids <- make_param_id(existing_details, param_key_cols)
        keep_mask <- !(ex_ids %in% up_ids)
        existing_details <- if (length(keep_mask)) existing_details[keep_mask, , drop = FALSE] else existing_details[FALSE, ]
      }
    }
    
    merged_details <- dplyr::bind_rows(existing_details, uploaded_details)
    
    # recompute per-parameter summary and scalars
    merged_byparam <- recompute_by_parameter_from_details(merged_details, param_key_cols)
    scalars <- compute_loc_scalars_from_byparam(merged_byparam)
    
    # lat/lon: prefer existing_locyear then uploaded_prep
    lat_val <- NA_real_; lon_val <- NA_real_
    if (nrow(existing_locyear)) {
      f <- existing_locyear
      for (kc in key_cols) f <- dplyr::filter(f, !!rlang::sym(kc) == keys[[kc]])
      if (nrow(f) > 0) { if ("lat" %in% names(f)) lat_val <- f$lat[1]; if ("lon" %in% names(f)) lon_val <- f$lon[1] }
    }
    if (is.na(lat_val) && nrow(uploaded_prep)) {
      f2 <- uploaded_prep
      for (kc in key_cols) f2 <- dplyr::filter(f2, !!rlang::sym(kc) == keys[[kc]])
      if (nrow(f2) > 0) { if ("lat" %in% names(f2)) lat_val <- f2$lat[1]; if ("lon" %in% names(f2)) lon_val <- f2$lon[1] }
    }
    
    out <- tibble::tibble(!!!keys)
    out$lat <- lat_val
    out$lon <- lon_val
    out$hazard_index <- scalars$hazard_index
    out$total_CR_cases_10k <- scalars$total_CR_cases_10k
    out$wl_index <- scalars$wl_index
    out[[ "by_parameter" ]] <- list(merged_byparam)
    out[[ detail_col ]] <- list(merged_details)
    
    out$N_existing_details <- nrow(existing_details)
    out$N_uploaded_details <- nrow(uploaded_details)
    out$N_merged_details <- nrow(merged_details)
    out$N_params_merged <- nrow(merged_byparam)
    
    out
  })
  
  
  # create a simple report showing replaced counts (how many groups had uploaded detail rows)
  report <- merged_rows %>%
    transmute(across(all_of(key_cols)),
              N_existing_details, N_uploaded_details, N_merged_details, N_params_merged)
  
  # arrange by key columns
  merged_rows <- merged_rows %>% arrange(across(all_of(key_cols)))
  
  list(merged = merged_rows, report = report)
}
