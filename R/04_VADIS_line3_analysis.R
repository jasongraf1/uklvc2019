# -------------------------------------------------------------------------
# File: 04_VADIS_line3_analysis.R
# Calculate statistics for the third line of the VADIS analysis
# -------------------------------------------------------------------------

# Load datasets
source("R/00_load_data.R")

# Load model varimp list
if (file.exists("output/gen_rf_varimp_list.rds")){
  gen_rf_varimp_list <- readRDS("output/gen_rf_varimp_list.rds")
}
# I don't use `vadis_line3()` since it takes too much time. Random forest models
# were run remotely on the University of Birmingham's High Performance Computing
# services. See `random_forest_batch.R` for the code, which can be replicated
# without the VADIS package.

varimp_table <- do.call("cbind", gen_rf_varimp_list) %>%
  as.data.frame()

write.csv(varimp_table,
          file = "output/varimp_table.csv",
          quote = FALSE)

varimp_table %>%
  round(3)

rank_table <- lapply(varimp_table, function(x) rank(-x)) %>%
  as.data.frame()

rownames(rank_table) <- rownames(varimp_table)

write.csv(rank_table,
          file = "output/rank_table.csv",
          quote = FALSE)

distance_matrix <- 1 - cor(varimp_table, method = "spearman") %>%
  as.data.frame()

write.csv(distance_matrix,
          file = "output/line3_dist_matrix.csv",
          quote = FALSE,
          row.names = FALSE)

distance_matrix %>%
  as.dist() %>%
  round(3)

