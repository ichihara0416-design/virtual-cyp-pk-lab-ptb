source("R/01_model_core.R")
REF <- readRDS("outputs/reference_fit.rds")

lit <- LITERATURE_FIXED_INPUTS[
  LITERATURE_FIXED_INPUTS$input_group == "external_transportability", ,
  drop = FALSE
]

required_ids <- c(
  "Field40_duration","Field50_onset","Field50_duration",
  "Kato25_Cp60","Kato25_duration","Means50_duration","Means50_k"
)
if (!all(required_ids %in% lit$input_id)) {
  stop("literature_fixed_inputs.csv is missing one or more external-transportability inputs.")
}
lit <- lit[match(required_ids, lit$input_id), ]

external <- data.frame(
  source = lit$source,
  endpoint = c(
    "LRR duration","LRR onset","LRR duration",
    "serum PTB at 60 min","sleep duration","sleep duration","plasma decline k"
  ),
  dose = as.numeric(lit$dose_mgkg),
  published = as.numeric(lit$value),
  stringsAsFactors = FALSE
)

external$model <- NA_real_
for (i in seq_len(nrow(external))) {
  d <- external$dose[i]
  e <- external$endpoint[i]
  if (e == "LRR duration")
    external$model[i] <- lrr_endpoints(REF$F_eff, REF$ka, REF$C_RR_thr, d, max_time_min = 600)["duration_min"]
  if (e == "LRR onset")
    external$model[i] <- lrr_endpoints(REF$F_eff, REF$ka, REF$C_RR_thr, d, max_time_min = 600)["onset_min"]
  if (e == "serum PTB at 60 min")
    external$model[i] <- plasma_at(REF$F_eff, REF$ka, REF$C_RR_thr, d, 60)
  if (e == "sleep duration")
    external$model[i] <- lrr_endpoints(REF$F_eff, REF$ka, REF$C_RR_thr, d, max_time_min = 600)["duration_min"]
  if (e == "plasma decline k")
    external$model[i] <- model_k(REF$F_eff, REF$ka, REF$C_RR_thr, 50, c(30,70,100,150))
}
external$model_to_published <- external$model / external$published
write.csv(external, "outputs/external_transportability.csv", row.names = FALSE)
