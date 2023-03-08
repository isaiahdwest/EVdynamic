##########################################################################
# Ingest data/models/objects to sysdata to be used by package functions ##
# Steps: Read in object, add to usethis::use_data(...), do not overwrite##
#        objects that are being ingested to sysdata already.##############
##########################################################################

l2_model.xgb <- readRDS("data-raw/xgb_model.rds")
l2_probs <- readRDS("data-raw/probs.rds")
l2_model.nn <- readRDS("data-raw/nn_model.rds")
usethis::use_data(l2_model.xgb,
                  l2_model.nn,
                  l2_probs,
                  overwrite = TRUE,
                  internal = TRUE)
