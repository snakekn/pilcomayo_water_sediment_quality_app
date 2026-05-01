clean_sediment_data <- function(data, source) {
  
  message("[clean_sediment_data] START | source = ", source)
  
  raw <- data
  if (is.null(raw) || !is.data.frame(raw)) {
    stop("[clean_sediment_data] input is NULL or not a data.frame")
  }
  
  message("[clean_sediment_data] raw dim = ", nrow(raw), " x ", ncol(raw))
  # message("[clean_sediment_data] raw names = ", paste(names(raw), collapse = ", "))
  
  raw_clean <- raw |>
    filter(if_any(-c(1, 2), ~ !is.na(.))) |>
    select(where(~ !all(is.na(.))))
  
  message("[clean_sediment_data] after blank row/col removal = ", nrow(raw_clean), " x ", ncol(raw_clean))
  
  if (nrow(raw_clean) == 0 || ncol(raw_clean) == 0) {
    stop("[clean_sediment_data] nothing left after removing blanks")
  }
  
  df_clean <- as.data.frame(t(raw_clean))
  message("[clean_sediment_data] after transpose = ", nrow(df_clean), " x ", ncol(df_clean))
  
  if (nrow(df_clean) < 3) {
    stop("[clean_sediment_data] transposed data too small to build headers")
  }
  
  new_names <- ifelse(
    !is.na(df_clean[2, ]),
    paste0(df_clean[1, ], " (", df_clean[2, ], ")"),
    df_clean[1, ]
  )
  colnames(df_clean) <- new_names
  df_clean <- df_clean[-c(1, 2), ]
  
  message("[clean_sediment_data] after header build = ", nrow(df_clean), " x ", ncol(df_clean))
  
  if (nrow(df_clean) == 0) {
    stop("[clean_sediment_data] no rows remain after removing header rows")
  }
  
  df_clean <- df_clean %>%
    mutate(across(everything(), ~ na_if(., "SIN DATOS")))
  
  message("[clean_sediment_data] replaced SIN DATOS")
  
  # safer numeric conversion for < and > values
  target_cols <- setdiff(names(df_clean), "Metales (Tamiz)")
  numericish_cols <- target_cols[vapply(df_clean[target_cols], function(x) {
    x <- x[!is.na(x)]
    length(x) > 0 && any(grepl("^[<>]?[0-9.]+$", x))
  }, logical(1))]
  
  # message("[clean_sediment_data] numeric-ish cols = ", paste(numericish_cols, collapse = ", "))
  
  df_clean <- df_clean %>%
    mutate(across(
      all_of(numericish_cols),
      ~ {
        x <- as.character(.)
        case_when(
          grepl("^<", x) ~ 0.5 * suppressWarnings(as.numeric(gsub("^<", "", x))),
          grepl("^>", x) ~ 1.5 * suppressWarnings(as.numeric(gsub("^>", "", x))),
          TRUE ~ suppressWarnings(as.numeric(x))
        )
      }
    ))
  
  message("[clean_sediment_data] converted < and > values")
  
  names(df_clean) <- gsub("\\(mg/kg fraccion\\) ", "", names(df_clean))
  names(df_clean) <- gsub("\\(ug/kg fraccion\\) ", "", names(df_clean))
  names(df_clean) <- gsub("\\(g/100 g fraccion\\) ", "", names(df_clean))
  
  # message("[clean_sediment_data] cleaned names = ", paste(names(df_clean), collapse = ", "))
  
  fill_cols <- c(
    "Estación", "Fecha", "Hora", "Campaña", "Institución", "Río",
    "Latitud", "Longitud", "Latitud Decimal", "Longitud Decimal",
    "Velocidad Media (m/s)", "Caudal (m3/s)", "Distancia al margen",
    "Conductividad en pasta (uS/cm)", "Densidad Aparente (g/cm3)",
    "Densidad Real (g/cm3)", "Humedad (%)", "Materia Orgánica (%)",
    "pH en pasta (u pH)", "Arena (%)", "Limo (%)", "Arcilla (%)",
    "Clasificación textural (Texto)", "0.032 mm - N° 450 (ASTM) (%)",
    "0.063 mm - N° 230 (ASTM) (%)", "0.125 mm - N° 120 (ASTM) (%)",
    "0.250 mm - N° 060 (ASTM) (%)", "0.500 mm - N° 035 (ASTM) (%)",
    "1.00 mm - N° 018 (ASTM) (%)", "2.00 mm - N° 010 (ASTM) (%)",
    "Residuo (%)", "Metales (Tamiz)"
  )
  
  existing_fill_cols <- intersect(fill_cols, names(df_clean))
  message("[clean_sediment_data] fill cols = ", paste(existing_fill_cols, collapse = ", "))
  
  df_clean <- df_clean %>%
    fill(any_of(fill_cols), .direction = "down")
  
  if (!"Latitud" %in% names(df_clean) || !"Longitud" %in% names(df_clean)) {
    stop("[clean_sediment_data] missing Latitud or Longitud")
  }
  
  message("[clean_sediment_data] computing decimal coordinates")
  df_clean <- df_clean |>
    mutate(
      `Latitud Decimal` = -sapply(Latitud, dms_to_decimal),
      `Longitud Decimal` = -sapply(Longitud, dms_to_decimal)
    ) |>
    relocate(`Latitud Decimal`, .after = Longitud) |>
    relocate(`Longitud Decimal`, .after = `Latitud Decimal`)
  
  message("[clean_sediment_data] decimal coordinates added")
  
  df_clean <- df_clean %>%
    mutate(across(where(~ is.character(.) && all(grepl("^-?[0-9.]+$", .[!is.na(.)], perl = TRUE))), as.numeric))
  
  if (!"Fecha" %in% names(df_clean)) stop("[clean_sediment_data] missing Fecha")
  df_clean$Fecha <- as.Date(df_clean$Fecha, format = "%d/%m/%Y")
  
  df_clean$data_source <- rep("pilcomayo.net", nrow(df_clean))
  
  if ("Estación" %in% names(df_clean)) {
    df_clean$Estación <- gsub(
      "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
      "Pilcomayo arriba Tacobamba",
      df_clean$Estación
    )
    df_clean$Estación <- gsub(
      "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
      "Tacobamba arriba Pilcomayo",
      df_clean$Estación
    )
  }
  
  message("[clean_sediment_data] END dim = ", nrow(df_clean), " x ", ncol(df_clean))
  # message("[clean_sediment_data] END names = ", paste(names(df_clean), collapse = ", "))
  df_clean
}