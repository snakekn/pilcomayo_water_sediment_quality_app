clean_sediment_data <- function(data, source = "pilco") {
  
  if (source == "pilco") {
    
    # STEP 1: load raw sediment data
    raw <- data
    
    # STEP 2: remove blank rows and columns
    raw_clean <- raw |> 
      filter(if_any(-c(1, 2), ~ !is.na(.))) |>
      select(where(~!all(is.na(.))))
    
    # STEP 3: Swap rows and columns and convert to data frame
    df_clean <- as.data.frame(t(raw_clean))
    
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
    
    # Replace <n with 0.5*n and >n with 1.5*n (only for numeric-looking columns and exclude Metales (Tamiz) column)
    df_clean <- df_clean %>%
      mutate(across(where(~ any(grepl("^[<>]", .[!is.na(.)]))) & 
                      !all_of("Metales (Tamiz)"), 
                    ~ case_when(
                      grepl("^<", .) ~ 0.5 * as.numeric(gsub("^<", "", .)),
                      grepl("^>", .) ~ 1.5 * as.numeric(gsub("^>", "", .)),
                      TRUE ~ as.numeric(.)
                    )))
    
    # Remove "(mg/kg fraccion)" from all column names
    names(df_clean) <- gsub("\\(mg/kg fraccion\\) ", "", names(df_clean))
    # Remove "(ug/kg fraccion)" from all column names
    names(df_clean) <- gsub("\\(ug/kg fraccion\\) ", "", names(df_clean))
    # Remove "(g/100 g fraccion)" from all column names
    names(df_clean) <- gsub("\\(g/100 g fraccion\\) ", "", names(df_clean))
    
    fill_cols <- c("Estación", "Fecha", "Hora", "Campaña", "Institución", "Río", "Latitud", "Longitud", "Latitud Decimal", "Longitud Decimal", "Velocidad Media (m/s)", "Caudal (m3/s)", "Distancia al margen", "Conductividad en pasta (uS/cm)", "Densidad Aparente (g/cm3)", "Densidad Real (g/cm3)", "Humedad (%)", "Materia Orgánica (%)", "pH en pasta (u pH)", "Arena (%)", "Limo (%)", "Arcilla (%)", "Clasificación textural (Texto)", "0.032 mm - N° 450 (ASTM) (%)", "0.063 mm - N° 230 (ASTM) (%)", "0.125 mm - N° 120 (ASTM) (%)", "0.250 mm - N° 060 (ASTM) (%)", "0.500 mm - N° 035 (ASTM) (%)", "1.00 mm - N° 018 (ASTM) (%)", "2.00 mm - N° 010 (ASTM) (%)", "Residuo (%)", "Metales (Tamiz)")
    
    # Fill only the columns that exist in both fill_cols and df_clean
    df_clean <- df_clean %>% 
      fill(any_of(fill_cols), .direction = "down")
    
    # Calculate decimal lat and long
    df_clean <- df_clean |>
      mutate(
        `Latitud Decimal` = -sapply(Latitud, dms_to_decimal),
        `Longitud Decimal` = -sapply(Longitud, dms_to_decimal)
      ) |>
      relocate(`Latitud Decimal`, .after = Longitud) |>
      relocate(`Longitud Decimal`, .after = `Latitud Decimal`)
    
    # make numeric-looking columns into data-type numeric
    df_clean <- df_clean %>%
      mutate(across(where(~ is.character(.) && all(grepl("^-?[0-9.]+$", .[!is.na(.)], perl = TRUE))), 
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
  stop(paste("Function clean_sediment_data: Source", source, "not supported"))
  
}
