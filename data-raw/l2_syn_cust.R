l2_custs <- readRDS("data-raw/syn_cust.rds")[,c("cust", "int.start", "interval.kwh")]
usethis::use_data(l2_custs)
