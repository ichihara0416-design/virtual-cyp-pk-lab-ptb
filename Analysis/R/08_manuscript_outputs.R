
# 08_manuscript_outputs.R
# Public manuscript-output generator.
# This script does not refit model parameters. It reads finalized outputs from
# Codes 02-07 and writes manuscript-oriented tables plus data-driven figures.
#
# Main manuscript outputs targeted:
#   Fig. 2  Reference/control calibration
#   Fig. 3  External transportability
#   Fig. 4  Evidence A/B behavioral calculations
#   Fig. 5  Virtual CYP-PK Lab Monte Carlo
#   Table 2 Reference/control model parameters/calibration
#   Table 3 External transportability
#   Table 4 Evidence A PK/behavior summary
#   Table 5 Evidence A+B compact summary
#
# Fig. 1 is a conceptual workflow diagram and is intentionally not generated here.

options(stringsAsFactors = FALSE)
source("R/00_constants.R")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

# ------------------------- helpers -------------------------
find_input <- function(fname_or_vec) {
  fname_or_vec <- as.character(fname_or_vec)
  dirs <- c(".", "outputs", "tables", "results", "data")
  for (nm in fname_or_vec) {
    candidates <- file.path(dirs, nm)
    hit <- candidates[file.exists(candidates)]
    if (length(hit)) return(hit[1])
  }
  stop("Input file not found. Tried: ", paste(fname_or_vec, collapse = ", "))
}

safe_png <- function(filename, width, height, res = 600, pointsize = 14) {
  ok <- FALSE
  try({
    png(filename = filename, width = width, height = height,
        units = "in", res = res, type = "cairo-png",
        family = "Arial", pointsize = pointsize, bg = "white")
    ok <- TRUE
  }, silent = TRUE)
  if (!ok) {
    png(filename = filename, width = width, height = height,
        units = "in", res = res, family = "Arial",
        pointsize = pointsize, bg = "white")
  }
}

safe_svg <- function(filename, width, height, pointsize = 14) {
  svg(filename = filename, width = width, height = height,
      pointsize = pointsize, bg = "white")
}

save_both <- function(stem, fun, w = 7.2, h = 5.4, pointsize = 13) {
  safe_png(file.path("figures", paste0(stem, ".png")), w, h, pointsize = pointsize)
  fun(); dev.off()
  safe_svg(file.path("figures", paste0(stem, ".svg")), w, h, pointsize = pointsize)
  fun(); dev.off()
}

base_par <- function(mar = c(4.8, 5.0, 1.8, 0.8),
                     oma = c(0, 0, 0, 0),
                     cex_axis = 0.9, cex_lab = 1.0, las = 1) {
  par(family = "Arial", mar = mar, oma = oma,
      mgp = c(2.7, 0.8, 0), tcl = -0.25,
      cex.axis = cex_axis, cex.lab = cex_lab,
      bty = "l", las = las)
}

pretty_condition <- function(x) {
  map <- c(
    PHB10_Means1978 = "Phenobarbital\n10 mg/kg",
    PHB21_5_Means1978 = "Phenobarbital\n21.5 mg/kg",
    OME40_Henry1986 = "Omeprazole\n40 mg/kg",
    SKF4_Means1978 = "SKF-525A\n4 mg/kg",
    SKF12_5_Means1978 = "SKF-525A\n12.5 mg/kg",
    DZP100_deRepentigny1976 = "Diazepam\n100 mg/kg"
  )
  z <- unname(map[as.character(x)])
  z[is.na(z)] <- as.character(x)[is.na(z)]
  z
}


short_condition <- function(x) {
  map <- c(
    PHB10_Means1978 = "PHB 10",
    PHB21_5_Means1978 = "PHB 21.5",
    OME40_Henry1986 = "OME 40",
    SKF4_Means1978 = "SKF 4",
    SKF12_5_Means1978 = "SKF 12.5",
    DZP100_deRepentigny1976 = "DZP 100"
  )
  z <- unname(map[as.character(x)])
  z[is.na(z)] <- as.character(x)[is.na(z)]
  z
}

fmt_ratio_string <- function(x, digits = 3) {
  vapply(strsplit(as.character(x), ";", fixed = TRUE), function(z) {
    znum <- suppressWarnings(as.numeric(z))
    if (anyNA(znum)) return(paste(z, collapse = "; "))
    paste(formatC(znum, format = "f", digits = digits), collapse = "; ")
  }, character(1))
}

# ------------------------- finalized inputs -------------------------
cal <- read.csv(find_input(c("control_calibration.csv", "control_calibration_replay.csv")),
                stringsAsFactors = FALSE, check.names = FALSE)
ext <- read.csv(find_input("external_transportability.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
pk  <- read.csv(find_input("pk_fits_A_B.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
beh <- read.csv(find_input("calculated_vs_reported_behavior_A_B.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
mrr <- read.csv(find_input("M_RR_EvidenceA.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
varq <- read.csv(find_input("control_variability_qualification.csv"),
                 stringsAsFactors = FALSE, check.names = FALSE)
ord <- read.csv(find_input("ordering_probabilities.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
mc  <- read.csv(find_input("monte_carlo_group_summaries.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
mc_dist <- read.csv(find_input("monte_carlo_group_distribution_summary.csv"),
                    stringsAsFactors = FALSE, check.names = FALSE)
mc_rep <- read.csv(find_input("monte_carlo_representative_experiment.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE)

# ------------------------- Table 2 -------------------------
REF <- readRDS(find_input("reference_fit.rds"))

table2_params <- data.frame(
  parameter = c(
    "BW_ref","k12","k21","k13","k31","Vmax_ref","Km_ref",
    "Vp_ref","Qe_ref","Kp,e","Ve_ref","F_eff","k_a","C_RR,thr"
  ),
  value = c(
    SOURCE_PARAMETERS$BW_ref, SOURCE_PARAMETERS$k12, SOURCE_PARAMETERS$k21,
    SOURCE_PARAMETERS$k13, SOURCE_PARAMETERS$k31, SOURCE_PARAMETERS$Vmax_ref,
    SOURCE_PARAMETERS$Km_ref, SOURCE_PARAMETERS$Vp_ref, SOURCE_PARAMETERS$Qe_ref,
    SOURCE_PARAMETERS$Kpe, SOURCE_PARAMETERS$Ve_ref,
    REF$F_eff, REF$ka, REF$C_RR_thr
  ),
  unit = c(
    "kg","h^-1","h^-1","h^-1","h^-1","mg/h","mg",
    "L","L/h","dimensionless","kg","dimensionless","h^-1","latent concentration scale"
  ),
  source_or_role = c(
    rep("Literature-derived source model", 11),
    "Reference/control calibration","Reference/control calibration","Reference/control calibration"
  ),
  stringsAsFactors = FALSE
)

cal_out <- cal
names(cal_out)[names(cal_out) == "published"] <- "literature_target"
names(cal_out)[names(cal_out) == "model"] <- "model_calculated"

write.csv(table2_params, "tables/Table2_reference_model_parameters.csv", row.names = FALSE)
write.csv(cal_out, "tables/Table2_reference_control_calibration.csv", row.names = FALSE)

# ------------------------- Table 3 -------------------------
write.csv(ext, "tables/Table3_external_transportability.csv", row.names = FALSE)

# ------------------------- Table 4 -------------------------
A <- beh[beh$evidence_tier == "A", , drop = FALSE]
keyA <- paste(A$condition_id, A$model_form, sep = "||")
keyM <- paste(mrr$condition_id, mrr$model_form, sep = "||")
idxM <- match(keyA, keyM)
if (anyNA(idxM)) stop("Evidence A rows missing matching M_RR rows.")

A$M_RR <- mrr$M_RR[idxM]
A$adjudication_status <- mrr$status[idxM]

status_label <- c(
  primary = "Primary",
  model_form_sensitivity = "Sensitivity",
  retained_model_form_uncertainty = "Retained: model-form uncertainty",
  retained_no_unique_model_form = "Retained: no unique model-form assignment"
)

table4 <- data.frame(
  condition = gsub("\\n", " ", pretty_condition(A$condition_id)),
  study = A$study,
  model_status = unname(status_label[A$adjudication_status]),
  PK_model_form = ifelse(A$model_form == "Vmax", "Vmax-based", "Km-based"),
  PK_multiplier = round(A$multiplier, 6),
  observed_PK_ratio = fmt_ratio_string(A$observed_pk_ratio, 3),
  predicted_PK_ratio = fmt_ratio_string(A$predicted_pk_ratio, 3),
  PK_objective = signif(A$objective, 4),
  boundary_hit = ifelse(A$boundary_hit, "Yes", "No"),
  observed_behavior_ratio = round(A$observed_behavior_ratio, 3),
  PK_only_calculated_behavior_ratio = round(A$calculated_behavior_ratio, 3),
  fold_error = round(A$fold_error, 3),
  direction_concordant = ifelse(A$direction_concordant, "Yes", "No"),
  M_RR = round(A$M_RR, 3),
  stringsAsFactors = FALSE
)


condition_order_A <- c(
  "PHB10_Means1978","PHB21_5_Means1978","OME40_Henry1986",
  "SKF4_Means1978","SKF12_5_Means1978","DZP100_deRepentigny1976"
)
A$.condition_order <- match(A$condition_id, condition_order_A)
A$.form_order <- match(A$model_form, c("Vmax","Km"))
ord4 <- order(A$.condition_order, A$.form_order)
table4 <- table4[ord4, , drop = FALSE]
row.names(table4) <- NULL
write.csv(table4, "tables/Table4_EvidenceA_PK_behavior_summary.csv", row.names = FALSE)

# ------------------------- Table 5 -------------------------
table5 <- beh[, c(
  "condition_id","drug","study","evidence_tier","model_form","multiplier",
  "observed_pk_ratio","predicted_pk_ratio","observed_behavior_ratio",
  "calculated_behavior_ratio","fold_error","direction_concordant",
  "prediction_status"
)]
table5$model_form <- ifelse(table5$model_form == "Vmax", "Vmax-based", "Km-based")
table5$observed_pk_ratio <- fmt_ratio_string(table5$observed_pk_ratio, 3)
table5$predicted_pk_ratio <- fmt_ratio_string(table5$predicted_pk_ratio, 3)
table5$observed_behavior_ratio <- round(table5$observed_behavior_ratio, 3)
table5$calculated_behavior_ratio <- round(table5$calculated_behavior_ratio, 3)
table5$fold_error <- round(table5$fold_error, 3)

table5$.tier_order <- match(table5$evidence_tier, c("A","B"))
table5$.A_condition_order <- match(table5$condition_id, condition_order_A)
table5$.A_condition_order[is.na(table5$.A_condition_order)] <- 999
table5$.form_order <- match(table5$model_form, c("Vmax-based","Km-based"))
table5 <- table5[order(table5$.tier_order, table5$.A_condition_order,
                       table5$condition_id, table5$.form_order), , drop = FALSE]
table5$.tier_order <- NULL
table5$.A_condition_order <- NULL
table5$.form_order <- NULL
row.names(table5) <- NULL
write.csv(table5, "tables/Table5_EvidenceA_B_summary.csv", row.names = FALSE)


# ------------------------- Figure 2 -------------------------
save_both("Fig2_control_calibration", function() {
  base_par(mar = c(7.2, 5.1, 1.0, 0.6))
  r <- if ("model_to_published" %in% names(cal)) cal$model_to_published else {
    cal$model_calculated / cal$literature_target
  }
  labs <- cal$endpoint
  plot(seq_along(r), r, pch = 19, xaxt = "n",
       xlab = "", ylab = "Model / literature-reported target",
       ylim = range(c(1, r), finite = TRUE))
  axis(1, at = seq_along(r), labels = labs, las = 2, cex.axis = 0.75)
  abline(h = 1, lty = 2)
}, w = 7.0, h = 5.6)

# ------------------------- Figure 3 -------------------------
save_both("Fig3_external_transportability", function() {
  base_par(mar = c(7.4, 5.1, 1.0, 0.6))
  r <- ext$model_to_published
  labs <- paste(ext$source, ext$endpoint, sep = "\n")
  plot(seq_along(r), r, pch = 19, xaxt = "n",
       xlab = "", ylab = "Model / literature-reported value",
       ylim = range(c(1, r), finite = TRUE))
  axis(1, at = seq_along(r), labels = labs, las = 2, cex.axis = 0.66)
  abline(h = 1, lty = 2)
}, w = 8.0, h = 6.0)

# ------------------------- Figure 4 -------------------------
# Explicit four-panel plotting. The plotting state is set once per device so
# all four panels are written to the same PNG/SVG page.
draw_fig4 <- function() {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(
    mfrow = c(2, 2),
    family = "Arial",
    oma = c(0.4, 0.4, 0.4, 0.4),
    mgp = c(2.6, 0.8, 0),
    tcl = -0.25,
    bty = "l",
    las = 1
  )

  # A: Evidence A Vmax-based calculations
  xA <- A[A$model_form == "Vmax", , drop = FALSE]
  par(mar = c(4.8, 5.0, 2.8, 0.8))
  limA <- range(c(xA$observed_behavior_ratio, xA$calculated_behavior_ratio), finite = TRUE)
  plot(
    xA$observed_behavior_ratio, xA$calculated_behavior_ratio,
    pch = 19, xlim = limA, ylim = limA,
    xlab = "Literature-reported LRR/hypnosis ratio",
    ylab = "Model-calculated LRR/hypnosis ratio",
    main = "A  Evidence A: Vmax-based calculations",
    cex.main = 0.95, cex.lab = 0.90, cex.axis = 0.82
  )
  abline(0, 1, lty = 2)
  text(
    xA$observed_behavior_ratio, xA$calculated_behavior_ratio,
    labels = short_condition(xA$condition_id),
    pos = c(4,4,4,2,2,2)[seq_len(nrow(xA))],
    offset = 0.45, cex = 0.66, xpd = NA
  )

  # B: Evidence A Km-based calculations
  xB <- A[A$model_form == "Km", , drop = FALSE]
  par(mar = c(4.8, 5.0, 2.8, 0.8))
  limB <- range(c(xB$observed_behavior_ratio, xB$calculated_behavior_ratio), finite = TRUE)
  plot(
    xB$observed_behavior_ratio, xB$calculated_behavior_ratio,
    pch = 1, xlim = limB, ylim = limB,
    xlab = "Literature-reported LRR/hypnosis ratio",
    ylab = "Model-calculated LRR/hypnosis ratio",
    main = "B  Evidence A: Km-based calculations",
    cex.main = 0.95, cex.lab = 0.90, cex.axis = 0.82
  )
  abline(0, 1, lty = 2)
  text(
    xB$observed_behavior_ratio, xB$calculated_behavior_ratio,
    labels = short_condition(xB$condition_id),
    pos = c(4,4,4,2,2,2)[seq_len(nrow(xB))],
    offset = 0.45, cex = 0.66, xpd = NA
  )

  # C: Evidence B finite-duration calculations
  Bfinite <- beh[
    beh$evidence_tier == "B" &
      beh$prediction_status == "finite_duration",
    , drop = FALSE
  ]
  par(mar = c(4.8, 5.0, 2.8, 0.8))
  limC <- range(c(Bfinite$observed_behavior_ratio,
                  Bfinite$calculated_behavior_ratio), finite = TRUE)
  plot(
    NA, xlim = limC, ylim = limC,
    xlab = "Literature-reported LRR/hypnosis ratio",
    ylab = "Model-calculated LRR/hypnosis ratio",
    main = "C  Evidence B: finite-duration calculations",
    cex.main = 0.95, cex.lab = 0.90, cex.axis = 0.82
  )
  abline(0, 1, lty = 2)
  is_vmax <- Bfinite$model_form == "Vmax"
  points(Bfinite$observed_behavior_ratio[is_vmax],
         Bfinite$calculated_behavior_ratio[is_vmax], pch = 19)
  points(Bfinite$observed_behavior_ratio[!is_vmax],
         Bfinite$calculated_behavior_ratio[!is_vmax], pch = 1)
  legend("bottomright", c("Vmax-based", "Km-based"),
         pch = c(19, 1), bty = "n", cex = 0.72)

  # D: Evidence B calculation status
  Ball <- beh[beh$evidence_tier == "B", , drop = FALSE]
  lev <- c("finite_duration", "no_threshold_crossing",
           "no_recovery_within_horizon")
  lab <- c("Finite\nduration", "No threshold\ncrossing",
           "No recovery\nby 720 min")
  n <- as.integer(table(factor(Ball$prediction_status, levels = lev)))
  par(mar = c(5.2, 5.0, 2.8, 0.8))
  bp <- barplot(
    n, names.arg = lab,
    ylab = "Model-condition combinations",
    main = "D  Evidence B: calculation status",
    ylim = c(0, max(40, max(n) * 1.12)),
    cex.main = 0.95, cex.lab = 0.90, cex.axis = 0.82,
    cex.names = 0.78
  )
  text(bp, n, labels = n, pos = 3, cex = 0.80)
}

safe_png("figures/Fig4_PK_to_behavior_EvidenceA_EvidenceB.png",
         width = 10.0, height = 7.6, res = 600, pointsize = 13)
draw_fig4()
dev.off()

safe_svg("figures/Fig4_PK_to_behavior_EvidenceA_EvidenceB.svg",
         width = 10.0, height = 7.6, pointsize = 13)
draw_fig4()
dev.off()

# ------------------------- Figure 5 -------------------------
# If Code 07 already wrote the final Fig. 5, do not regenerate a different
# version here. Copy its source tables into tables/ for auditability and provide
# an explicit QA snapshot.
if (!file.exists("figures/Fig_5_VirtualLab_MonteCarlo.png")) {
  warning("Code 07 Fig. 5 PNG was not found. Run R/07_monte_carlo.R before Code 08.")
}
if (!file.exists("figures/Fig_5_VirtualLab_MonteCarlo.svg")) {
  warning("Code 07 Fig. 5 SVG was not found. Run R/07_monte_carlo.R before Code 08.")
}

write.csv(ord, "tables/Fig5_source_ordering_probabilities.csv", row.names = FALSE)
write.csv(mc, "tables/Fig5_source_monte_carlo_group_summaries.csv", row.names = FALSE)
write.csv(mc_dist, "tables/Fig5_source_group_distribution_summary.csv", row.names = FALSE)
write.csv(mc_rep, "tables/Fig5_source_representative_experiment.csv", row.names = FALSE)

# ------------------------- Additional auditable source tables -------------------------
write.csv(cal, "tables/Fig2_source_control_calibration.csv", row.names = FALSE)
write.csv(ext, "tables/Fig3_source_external_transportability.csv", row.names = FALSE)
write.csv(A, "tables/Fig4_source_EvidenceA.csv", row.names = FALSE)
write.csv(beh[beh$evidence_tier=="B", ], "tables/Fig4_source_EvidenceB.csv", row.names = FALSE)
write.csv(varq, "tables/variability_qualification.csv", row.names = FALSE)

# ------------------------- Output QA -------------------------
qa <- data.frame(
  check = c(
    "Table2 calibration rows available",
    "Table3 has seven transportability endpoints",
    "Table4 has 12 Evidence A model-condition rows",
    "Table5 has 52 A+B model-condition rows",
    "Evidence B has 35 finite-duration calculations",
    "Evidence B has 5 no-threshold-crossing calculations",
    "Evidence B has 0 no-recovery-within-horizon calculations",
    "Monte Carlo has 10000 replicates per group",
    "Monte Carlo ordering includes six plasma time points"
  ),
  pass = c(
    nrow(cal) == 4L,
    nrow(ext) == 7L,
    nrow(A) == 12L,
    nrow(beh) == 52L,
    sum(beh$evidence_tier=="B" & beh$prediction_status=="finite_duration") == 35L,
    sum(beh$evidence_tier=="B" & beh$prediction_status=="no_threshold_crossing") == 5L,
    sum(beh$evidence_tier=="B" & beh$prediction_status=="no_recovery_within_horizon") == 0L,
    nrow(mc) == MC_REPS * 3L,
    all(c("Cp15","Cp30","Cp60","Cp120","Cp180","Cp240") %in% ord$endpoint)
  ),
  stringsAsFactors = FALSE
)

write.csv(qa, "outputs/manuscript_output_QA.csv", row.names = FALSE)
print(qa)
if (any(!qa$pass)) stop("Manuscript-output QA failed.")

cat("Code 08 manuscript outputs completed.\n")
cat("Generated manuscript-oriented Tables 2-5 source CSVs and Figs. 2-4.\n")
cat("Fig. 5 is generated by Code 07 and its source data were copied to tables/.\n")
