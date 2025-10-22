# /C:/Users/nadav/OneDrive/Documents/GitHub/riverremedy#/scripts/risk_analysis/weights.R

# Divide a numeric value by a dictionary (named numeric vector) entry matched by type.
# Returns a numeric vector (NA where type not found or weight == 0).
# need to save a set of WHO guidelines somewhere in here
calculate_hazard_q <- function(value, parameter) {
  # --- Input checks ---
  if (missing(value) || missing(parameter)) stop("Both 'value' and 'parameter' are required.")
  if (!is.numeric(value)) stop("'value' must be numeric.")
  parameter <- as.character(parameter)

  # --- Load WHO standards ---
  stds <- readr::read_csv("data/standards/strict_standards.csv")

  # --- Ensure same length recycling ---
  n <- max(length(value), length(parameter))
  value <- rep(value, length.out = n)
  parameter <- rep(parameter, length.out = n)

  # --- Match parameters ---
  matched_guidelines <- guidelines$guideline[match(parameter, guidelines$parameter)]
  matched_guidelines <- as.numeric(matched_guidelines)

  # --- Handle missing or zero guidelines ---
  missing_mask <- is.na(matched_guidelines)
  zero_mask <- matched_guidelines == 0 & !is.na(matched_guidelines)

  if (any(missing_mask))
    warning("Some parameters not found in WHO guidelines; returning NA for those positions.")
  if (any(zero_mask))
    warning("Some guidelines are zero; returning NA for those positions to avoid division by zero.")

  # --- Calculate hazard quotient ---
  result <- rep(NA_real_, n)
  ok <- !missing_mask & !zero_mask
  result[ok] <- value[ok] / matched_guidelines[ok]

  result
}
