library(qs2)
files <- list.files("data/processed", pattern = "\\.rds$", full.names = TRUE)
for (f in files) {
  obj <- readRDS(f)
  qs2::qs_save(obj, gsub("\\.rds$", ".qs2", f))
}
