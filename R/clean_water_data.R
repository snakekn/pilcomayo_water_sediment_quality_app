#' Clean raw water data from pilco format
#' @param data Raw data frame from pilco
#' @param source Data source ("pilco" currently supported)
clean_water_data <- function(data, source = "pilco") {
  
  if (source == "pilco") {
    
    # STEP 1: load raw water data
    raw <- data
    
    # STEP 2: remove blank rows
    raw_clean <- raw |> filter(if_any(-c(1, 2), ~ !is.na(.)))
    
    # STEP 3: Swap rows and columns and convert to data frame
    df <- as.data.frame(t(raw_clean))
    
    # STEP 4: remove blank rows
    df_clean <- df %>% filter(if_any(everything(), ~ !is.na(.)))
    
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
    
    # Replace "ND" with NA
    df_clean <- df_clean %>%
      mutate(across(everything(), ~ na_if(., "ND")))
    
    # Replace "Ausencia" with NA
    df_clean <- df_clean %>%
      mutate(across(everything(), ~ na_if(., "Ausencia")))
    
    # Replace <n with 0.5*n and >n with 1.5*n (only for numeric-looking columns)
    df_clean <- df_clean %>%
      mutate(across(where(~ any(grepl("^[<>]", .[!is.na(.)]))), 
                    ~ case_when(
                      grepl("^<", .) ~ 0.5 * as.numeric(gsub("^<", "", .)),
                      grepl("^>", .) ~ 1.5 * as.numeric(gsub("^>", "", .)),
                      TRUE ~ as.numeric(.)
                    )))
  
    # make numeric-looking columns numeric
    df_clean <- df_clean %>%
      mutate(across(where(~ is.character(.) && 
                            all(grepl("^-?[0-9.]+([eE][+-]?[0-9]+)?$", .[!is.na(.)], perl = TRUE))), 
                    as.numeric))
    
    # set "Fecha" column to data-type Date
    df_clean$Fecha <- as.Date(df_clean$Fecha, format = "%d/%m/%Y")
    
    # create "data_source" column 
    df_clean$data_source <- rep("pilcomayo.net", nrow(df_clean))

    # shorten name of long station names
    df_clean$Estación <- gsub("Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba", 
                              "Pilcomayo arriba Tacobamba",
                              df_clean$Estación)
    
    df_clean$Estación <- gsub("Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba", 
                              "Tacobamba arriba Pilcomayo",
                              df_clean$Estación)
    
    return(df_clean)
  }
  stop(paste("Function clean_water_data: Source", source, "not supported"))

}
