clean_water_data <- function(data, source = "TNC") {
  
  if (source == "TNC") {
    
    # STEP 1: load raw water data
    raw <- data
    
    # STEP 2: remove blank rows
    raw_clean <- raw |> filter(if_any(-c(1, 2), ~ !is.na(.)))
    
    # STEP 3: Swap rows and columns and convert to data frame
    df <- as.data.frame(t(raw_clean))
    
    # STEP 4: remove blank rows
    df_clean <- df %>% filter(if_any(everything(), ~ !is.na(.)))
    
    # remove intermediate data frames
    rm(raw_clean, df)
    
    # move unit from row 2 to end of row 1
    new_names <- ifelse(!is.na(df_clean[2, ]), 
                        paste0(df_clean[1, ], " (", df_clean[2, ], ")"), 
                        df_clean[1, ])
    
    # Assign new names to data frame
    colnames(df_clean) <- new_names
    
    # Remove first 2 rows (duplicate column names and units)
    df_clean <- df_clean[-c(1,2),]
    
    # Replace "SIN DATOS" with NA
    df_clean <- df_clean %>%
      mutate(across(everything(), ~ na_if(., "SIN DATOS")))
    
    # Replace <n with 0.5*n and >n with 1.5*n (only for numeric-looking columns)
    df_clean <- df_clean %>%
      mutate(across(where(~ any(grepl("^[<>]", .[!is.na(.)]))), 
                    ~ case_when(
                      grepl("^<", .) ~ 0.5 * as.numeric(gsub("^<", "", .)),
                      grepl("^>", .) ~ 1.5 * as.numeric(gsub("^>", "", .)),
                      TRUE ~ as.numeric(.)
                    )))
    
  }
  return(df_clean)
}
