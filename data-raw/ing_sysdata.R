
l2_model <- readRDS("data-raw/xgb_model.rds")
l2_probs <- readRDS("data-raw/probs.rds")
usethis::use_data(l2_model,
                  l2_probs,
                  overwrite = TRUE,
                  internal = TRUE)
