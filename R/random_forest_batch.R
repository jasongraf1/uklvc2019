# -------------------------------------------------------------------------
# File: random_forest_batch.R
# Run random forest analyses with conditional variable importance
# calculations for third line of VADIS analysis.
# -------------------------------------------------------------------------


# load dataset
source(".Rprofile")
source("R/00_load_data.R")

# formula
fmla <- Type ~ Speaker_ID +
  Possessor.Animacy3 +
  Possessor.Length +
  Possessum.Length +
  Final.Sibilant +
  TTR +
  Possessor.Givenness +
  Possessor.Expression.Type +
  PersistenceBinary +
  ProtoSemanticRelation +
  Possessor.Thematicity

# model function
rf_fun <- function(d){
  cforest(fmla, data = d, control = cforest_control(mtry = 4, ntree = 2000))
}

# fit models
# system.time(test_m <- rf_fun(data_list[[1]]))
# Single model runtime:
# user  system elapsed
# 2.91    0.01    3.41
if (!file.exists("models/gen_rf_list.rds")) {
  rf_list <- lapply(data_list, FUN = rf_fun)
  saveRDS(rf_list, "models/gen_rf_list.rds")
}

# get responses
# system.time(test_m_resp <- treeresponse(test_m))
# Single model runtime:
# user  system elapsed
# 8.44    0.01    8.84
if (!file.exists("models/gen_rf_pred_list.rds")) {
  rf_pred_list <- lapply(rf_list, FUN = treeresponse)
  saveRDS(rf_pred_list, "models/gen_rf_pred_list.rds")
}

# get variable importances
# system.time(test_m_varimp <- varimpAUC(test_m, conditional = T))
# Single model runtime:
# user  system elapsed
# 407.26    0.23  408.87
if (!file.exists("models/gen_rf_varimp_list.rds")) {
  rf_varimp_list <- lapply(
    rf_list,
    FUN = function(m) varimpAUC(m, conditional = TRUE)
    )
  saveRDS(rf_varimp_list, "models/gen_rf_varimp_list.rds")
}