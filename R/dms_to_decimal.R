# Function to convert DMS to decimal degrees
dms_to_decimal <- function(dms_string) {
  # Remove quotes and split by degree, minute, second symbols
  parts <- gsub("\"", "", dms_string)
  parts <- strsplit(parts, "[°']")[[1]]
  
  # Extract degrees, minutes, seconds
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  
  # Calculate decimal degrees
  decimal <- degrees + minutes/60 + seconds/3600
  
  return(decimal)
}