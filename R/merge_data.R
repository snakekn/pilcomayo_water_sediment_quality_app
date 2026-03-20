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