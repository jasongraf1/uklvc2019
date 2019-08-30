# -------------------------------------------------------------------------
# File: 03_VADIS_line2_analysis.R
# Calculate statistics for the second line of the VADIS analysis
# -------------------------------------------------------------------------

# Load datasets
source("R/00_load_data.R")

# Load model list or run the analysis if model list doesn't exist
if (file.exists("output/gen_brms_list.rds")){
  gen_brms_list <- readRDS("output/gen_brms_list.rds")
} else {
  source("R/01_brms_analysis.R")
}

coef_line <- vadis_line2(gen_brms_list, path = "output/vadis_line2.rds")

coef_line$coef.table %>%
  round(2)

write.csv(coef_line$coef.table, file = "output/coef_table.csv", quote = FALSE)

coef_line$distance.matrix %>%
  round(3)

coef_line$distance.matrix %>%
  as.matrix() %>%
  as.data.frame() %>%
  write.csv(
    file = "output/line2_dist_matrix.csv",
    quote = FALSE,
    row.names = FALSE
    )

coef_line$similarity.scores

