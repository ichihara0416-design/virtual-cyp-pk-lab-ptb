# 07_monte_carlo.R
# Unified Monte Carlo analysis for the Virtual CYP-PK Lab.
# Plasma PTB endpoints at 15, 30, 60, 120, 180, and 240 min are generated,
# measurement-error perturbed, summarized, and plotted within one common
# Monte Carlo workflow. No legacy-stream preservation is attempted.
# The illustrative experiment remains on its own fixed RNG stream so that it
# does not alter the 10,000-practical Monte Carlo results.
# Outputs the manuscript 6-panel Fig. 5 (PNG/SVG) and unified result tables.

options(stringsAsFactors = FALSE)
source("R/01_model_core.R")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

REF  <- readRDS("outputs/reference_fit.rds")
bank <- read.csv("outputs/virtual_rat_bank.csv", stringsAsFactors = FALSE)
app  <- read.csv("outputs/app_parameters.csv", stringsAsFactors = FALSE)

# Required columns use the same nomenclature as the manuscript and the current pipeline.
required_bank <- c("RatID", "BW_kg", "F_eff", "ka", "Vmax_iiv", "C_RR_thr")
required_app  <- c("group", "Vmax_multiplier", "Km_multiplier", "M_RR")
if (!all(required_bank %in% names(bank))) {
  stop("virtual_rat_bank.csv is missing required columns: ",
       paste(setdiff(required_bank, names(bank)), collapse = ", "))
}
if (!all(required_app %in% names(app))) {
  stop("app_parameters.csv is missing required columns: ",
       paste(setdiff(required_app, names(app)), collapse = ", "))
}

message("Code 07 threshold column: C_RR_thr")
message("Code 07 threshold-multiplier column: M_RR")

mult_ln <- function(cv, z) exp(-0.5 * log1p(cv^2) + sqrt(log1p(cv^2)) * z)

hepatic <- function(r, vm, km) {
  s <- r$BW_kg / SOURCE_PARAMETERS$BW_ref
  Vmax <- SOURCE_PARAMETERS$Vmax_ref * s^0.75 * r$Vmax_iiv * vm
  Vp   <- SOURCE_PARAMETERS$Vp_ref * s
  KmC  <- (SOURCE_PARAMETERS$Km_ref * s * km) / Vp
  Vmax * HEPATIC_CSTD_UG_ML / (KmC + HEPATIC_CSTD_UG_ML)
}

# All plasma sampling times are handled identically in one workflow.
sample_times <- c(15, 30, 60, 120, 180, 240)
groups <- c("Control", "PHB", "OME")

# Cache model-derived endpoints for all fixed-bank rats. This step uses no RNG.
cache <- list()
for (g in seq_len(nrow(app))) {
  mat <- matrix(NA_real_, nrow(bank), 2 + length(sample_times))
  colnames(mat) <- c("sleep", paste0("Cp", sample_times), "hepatic")
  for (i in seq_len(nrow(bank))) {
    r <- bank[i, ]
    Cthr_i <- r$C_RR_thr
    MRR_g <- app$M_RR[g]
    z <- simulate_rat(
      r$F_eff, r$ka, Cthr_i, APP_PTB_DOSE_MGKG,
      app$Vmax_multiplier[g], app$Km_multiplier[g], MRR_g,
      r$BW_kg, r$Vmax_iiv, max_time_min = 720
    )
    lr <- lrr_from_trajectory(z, Cthr_i * MRR_g)
    cps <- approx(z$time_min, z$Cp, xout = sample_times)$y
    mat[i, ] <- c(
      lr["duration_min"], cps,
      hepatic(r, app$Vmax_multiplier[g], app$Km_multiplier[g])
    )
  }
  cache[[app$group[g]]] <- mat
}

# Fail early if LRR duration could not be calculated. This prevents the later
# plotting error "finite 'ylim' values are needed" from hiding the real cause.
cache_sleep_finite <- vapply(
  cache,
  function(m) sum(is.finite(m[, "sleep"])),
  integer(1)
)
message("Finite cached sleep durations: ",
        paste(names(cache_sleep_finite), cache_sleep_finite, sep = "=", collapse = ", "))
if (any(cache_sleep_finite == 0L)) {
  stop(
    "No finite LRR/hypnosis durations were generated for: ",
    paste(names(cache_sleep_finite)[cache_sleep_finite == 0L], collapse = ", "),
    ". Check the C_RR_thr/M_RR mapping and rerun Code 06 before Code 07."
  )
}

# Unified Monte Carlo simulation.
# A single deterministic RNG stream is used for rat sampling and measurement
# error across all six plasma sampling times and the hepatic virtual assay.
# Because all six plasma time points are now handled in the same loop, results
# are internally reproducible from MC_SEED but are not expected to reproduce
# the former four-time-point Code 07 values exactly.
set.seed(MC_SEED)
endpoint_names <- c("sleep", paste0("Cp", sample_times), "hepatic")
res <- vector("list", MC_REPS * length(groups))
ord <- matrix(FALSE, MC_REPS, length(endpoint_names))
colnames(ord) <- endpoint_names
k <- 0L

for (rep in seq_len(MC_REPS)) {
  means <- matrix(
    NA_real_, length(groups), length(endpoint_names),
    dimnames = list(groups, endpoint_names)
  )

  for (g in seq_along(groups)) {
    ids <- sample(seq_len(nrow(bank)), 3, replace = FALSE)
    x <- cache[[groups[g]]][ids, , drop = FALSE]

    # Apply independent lognormal plasma-measurement error at every sampling time.
    for (nm in paste0("Cp", sample_times)) {
      x[, nm] <- x[, nm] * mult_ln(PLASMA_MEASUREMENT_CV, rnorm(3))
    }

    # Apply independent lognormal error to the standardized hepatic assay.
    x[, "hepatic"] <- x[, "hepatic"] *
      mult_ln(HEPATIC_VIRTUAL_ASSAY_CV, rnorm(3))

    means[g, ] <- colMeans(x[, endpoint_names, drop = FALSE])

    k <- k + 1L
    res[[k]] <- data.frame(
      replicate = rep,
      group = groups[g],
      mean_sleep = means[g, "sleep"],
      mean_Cp15 = means[g, "Cp15"],
      mean_Cp30 = means[g, "Cp30"],
      mean_Cp60 = means[g, "Cp60"],
      mean_Cp120 = means[g, "Cp120"],
      mean_Cp180 = means[g, "Cp180"],
      mean_Cp240 = means[g, "Cp240"],
      mean_hepatic = means[g, "hepatic"]
    )
  }

  # Expected ordering: PHB < Control < OME for sleep and plasma,
  # and OME < Control < PHB for the standardized hepatic assay.
  ord[rep, "sleep"] <-
    means["PHB", "sleep"] < means["Control", "sleep"] &&
    means["Control", "sleep"] < means["OME", "sleep"]

  for (nm in paste0("Cp", sample_times)) {
    ord[rep, nm] <-
      means["PHB", nm] < means["Control", nm] &&
      means["Control", nm] < means["OME", nm]
  }

  ord[rep, "hepatic"] <-
    means["OME", "hepatic"] < means["Control", "hepatic"] &&
    means["Control", "hepatic"] < means["PHB", "hepatic"]
}

MC <- do.call(rbind, res)

if (!any(is.finite(MC$mean_sleep))) {
  stop(
    "MC$mean_sleep contains no finite values. ",
    "This is not a plotting-scale problem; sleep duration was not calculated upstream."
  )
}
if (any(!is.finite(MC$mean_sleep))) {
  warning(sum(!is.finite(MC$mean_sleep)),
          " Monte Carlo group-mean sleep values are non-finite.")
}

ordering <- data.frame(
  endpoint = colnames(ord),
  probability = colMeans(ord),
  repetitions = MC_REPS
)

write.csv(MC, "outputs/monte_carlo_group_summaries.csv", row.names = FALSE)
write.csv(ordering, "outputs/ordering_probabilities.csv", row.names = FALSE)

# Group-level distribution summary for audit/reporting.
endpoint_map <- c(
  sleep = "mean_sleep",
  Cp15 = "mean_Cp15", Cp30 = "mean_Cp30", Cp60 = "mean_Cp60",
  Cp120 = "mean_Cp120", Cp180 = "mean_Cp180", Cp240 = "mean_Cp240",
  hepatic = "mean_hepatic"
)

dist_summary <- do.call(rbind, lapply(groups, function(gr) {
  d <- MC[MC$group == gr, , drop = FALSE]
  do.call(rbind, lapply(names(endpoint_map), function(ep) {
    x <- d[[endpoint_map[[ep]]]]
    data.frame(
      group = gr, endpoint = ep, n = length(x),
      mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
      q025 = unname(quantile(x, 0.025, na.rm = TRUE)),
      q25 = unname(quantile(x, 0.25, na.rm = TRUE)),
      median = median(x, na.rm = TRUE),
      q75 = unname(quantile(x, 0.75, na.rm = TRUE)),
      q975 = unname(quantile(x, 0.975, na.rm = TRUE))
    )
  }))
}))
write.csv(dist_summary,
          "outputs/monte_carlo_group_distribution_summary.csv",
          row.names = FALSE)

# Save the replicate-level ordering indicators for auditability.
ord_rep <- data.frame(replicate = seq_len(MC_REPS), ord, check.names = FALSE)
write.csv(ord_rep, "outputs/monte_carlo_ordering_by_replicate.csv", row.names = FALSE)

# Representative/illustrative experiment.
# It is generated from a separate fixed RNG stream so that it cannot alter the
# 10,000-practical Monte Carlo results. All six plasma times are generated in
# exactly the same way within this illustrative experiment.
REPRESENTATIVE_EXPERIMENT_ID <- 5163L
set.seed(MC_SEED + REPRESENTATIVE_EXPERIMENT_ID)

rep_rat <- list()
rr <- 0L

for (g in seq_along(groups)) {
  ids <- sample(seq_len(nrow(bank)), 3, replace = FALSE)
  x <- cache[[groups[g]]][ids, , drop = FALSE]

  for (nm in paste0("Cp", sample_times)) {
    x[, nm] <- x[, nm] * mult_ln(PLASMA_MEASUREMENT_CV, rnorm(3))
  }

  x[, "hepatic"] <- x[, "hepatic"] *
    mult_ln(HEPATIC_VIRTUAL_ASSAY_CV, rnorm(3))

  for (i in seq_len(nrow(x))) {
    rr <- rr + 1L
    rep_rat[[rr]] <- data.frame(
      experiment_id = REPRESENTATIVE_EXPERIMENT_ID,
      group = groups[g],
      rat_id = ids[i],
      sleep = x[i, "sleep"],
      Cp15 = x[i, "Cp15"],
      Cp30 = x[i, "Cp30"],
      Cp60 = x[i, "Cp60"],
      Cp120 = x[i, "Cp120"],
      Cp180 = x[i, "Cp180"],
      Cp240 = x[i, "Cp240"],
      hepatic = x[i, "hepatic"]
    )
  }
}

rep_rat <- do.call(rbind, rep_rat)
write.csv(
  rep_rat,
  "outputs/monte_carlo_representative_experiment.csv",
  row.names = FALSE
)

# Compact manuscript-oriented table: probability plus MC group means.
means_by_group <- do.call(rbind, lapply(names(endpoint_map), function(ep) {
  v <- sapply(groups, function(gr) {
    mean(MC[MC$group == gr, endpoint_map[[ep]], drop = TRUE], na.rm = TRUE)
  })
  data.frame(endpoint = ep,
             Control_mean = v["Control"], PHB_mean = v["PHB"], OME_mean = v["OME"])
}))
mc_table <- merge(ordering, means_by_group, by = "endpoint", sort = FALSE)
mc_table <- mc_table[match(endpoint_names, mc_table$endpoint), ]
write.csv(mc_table, "tables/Table_MC.csv", row.names = FALSE)

# Internal QA for the unified six-time-point analysis.
stopifnot(
  identical(colnames(ord), endpoint_names),
  nrow(MC) == MC_REPS * length(groups),
  all(c("mean_Cp15", "mean_Cp30", "mean_Cp60",
        "mean_Cp120", "mean_Cp180", "mean_Cp240") %in% names(MC)),
  all(is.finite(ordering$probability))
)

write.csv(
  data.frame(
    endpoint = endpoint_names,
    probability = ordering$probability,
    n_replicates = MC_REPS
  ),
  "outputs/MC_QA.csv",
  row.names = FALSE
)

qa_rich <- merge(
  data.frame(endpoint = colnames(ord), probability_recomputed = colMeans(ord)),
  ordering,
  by = "endpoint", all = TRUE
)
qa_rich$absolute_difference <- abs(qa_rich$probability_recomputed - qa_rich$probability)
qa_rich$pass <- is.finite(qa_rich$absolute_difference) & qa_rich$absolute_difference < 1e-12
write.csv(qa_rich, "outputs/monte_carlo_rich_QA.csv", row.names = FALSE)
if (!all(qa_rich$pass)) stop("Monte Carlo QA failed: ordering probabilities changed.")

# Cumulative group-mean sleep stability (same content as prior Fig. 5C).
conv_n <- unique(pmin(MC_REPS, round(exp(seq(log(1), log(MC_REPS), length.out = 160)))))
conv_n <- sort(unique(c(1:10, conv_n, MC_REPS)))
conv_sleep <- do.call(rbind, lapply(groups, function(gr) {
  d <- MC[MC$group == gr, , drop = FALSE]
  d <- d[order(d$replicate), ]
  cm <- cumsum(d$mean_sleep) / seq_len(nrow(d))
  data.frame(repetitions = conv_n, group = gr, value = cm[conv_n])
}))

write.csv(conv_sleep, "outputs/monte_carlo_convergence.csv", row.names = FALSE)

# ---------- plotting helpers ----------
safe_png <- function(filename, width, height, res = 600, pointsize = 14) {
  ok <- FALSE
  try({
    png(filename, width = width, height = height, units = "in", res = res,
        type = "cairo-png", family = "Arial", pointsize = pointsize, bg = "white")
    ok <- TRUE
  }, silent = TRUE)
  if (!ok) png(filename, width = width, height = height, units = "in", res = res,
               family = "Arial", pointsize = pointsize, bg = "white")
}
safe_svg <- function(filename, width, height, pointsize = 14) {
  svg(filename, width = width, height = height, pointsize = pointsize, bg = "white")
}
base_par <- function(mar = c(4.8, 5.5, 1.8, 0.8), cex_axis = 0.9, cex_lab = 0.98) {
  par(family = "Arial", mar = mar, mgp = c(2.6, 0.8, 0), tcl = -0.25,
      cex.axis = cex_axis, cex.lab = cex_lab, bty = "l", las = 1)
}
cap_segment <- function(x, y0, y1, cap = 1.5, lwd = 1) {
  segments(x, y0, x, y1, lwd = lwd)
  segments(x - cap, y0, x + cap, y0, lwd = lwd)
  segments(x - cap, y1, x + cap, y1, lwd = lwd)
}

ord_label <- c(
  sleep = "LRR/\nhypnosis", Cp15 = "PTB\n15 min", Cp30 = "PTB\n30 min",
  Cp60 = "PTB\n60 min", Cp120 = "PTB\n120 min", Cp180 = "PTB\n180 min",
  Cp240 = "PTB\n240 min", hepatic = "Hepatic PTB\nactivity"
)

draw_fig <- function() {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(2, 3))

  # A: MC distribution of group mean LRR/hypnosis duration; y starts at 0.
  base_par(mar = c(4.8, 6.0, 2.0, 0.8))
  ymax <- max(MC$mean_sleep, na.rm = TRUE) * 1.08
  boxplot(mean_sleep ~ factor(group, levels = groups), data = MC,
          xlab = "Treatment", ylab = "Group mean LRR/hypnosis duration (min)",
          outline = FALSE, ylim = c(0, ymax))
  mtext("A  10,000 virtual practicals", side = 3, line = 0.35, adj = 0, cex = 0.96)

  # B: expected ordering probabilities; includes 180 and 240 min.
  base_par(mar = c(9.5, 5.6, 2.0, 0.6), cex_axis = 0.84)
  bp <- barplot(ordering$probability, names.arg = rep("", nrow(ordering)),
                ylim = c(0, 1.10), ylab = "Probability of expected group ordering",
                xaxt = "n", space = 0.25)
  usr <- par("usr")
  text(bp, usr[3] - 0.045 * diff(usr[3:4]), labels = unname(ord_label[ordering$endpoint]),
       xpd = NA, adj = c(0.5, 1), cex = 0.68)
  text(bp, pmin(ordering$probability + 0.035, 1.065),
       labels = sprintf("%.3f", ordering$probability), cex = 0.70)
  mtext("B  Expected group ordering", side = 3, line = 0.35, adj = 0, cex = 0.96)

  # C: convergence/stability; y starts at 0.
  base_par(mar = c(5.0, 5.8, 2.0, 0.8))
  ymax_c <- max(conv_sleep$value, na.rm = TRUE) * 1.08
  plot(NA, xlim = range(conv_sleep$repetitions), ylim = c(0, ymax_c), log = "x",
       xlab = "Number of simulated practicals",
       ylab = "Cumulative mean LRR/hypnosis duration (min)")
  lty_mc <- c(Control = 1, PHB = 2, OME = 3)
  for (gr in groups) {
    z <- conv_sleep[conv_sleep$group == gr, ]
    lines(z$repetitions, z$value, lty = lty_mc[gr], lwd = 2)
  }
  legend("topright", groups, lty = lty_mc, lwd = 2, bty = "n", cex = 0.76)
  mtext("C  Stability of LRR/hypnosis duration", side = 3, line = 0.35, adj = 0, cex = 0.96)

  # D: representative plasma time course through 240 min; y starts at 0.
  base_par(mar = c(5.0, 5.8, 2.0, 0.8))
  allp <- unlist(rep_rat[paste0("Cp", sample_times)])
  ymax_d <- max(allp, na.rm = TRUE) * 1.15
  plot(NA, xlim = c(10, 245), ylim = c(0, ymax_d),
       xlab = "Nominal post-dose time (min)",
       ylab = expression(paste("Virtual measured plasma PTB (", mu, "g/mL)")))
  lty <- c(Control = 1, PHB = 2, OME = 3)
  pch <- c(Control = 16, PHB = 17, OME = 15)
  for (gr in groups) {
    d <- rep_rat[rep_rat$group == gr, ]
    mm <- as.matrix(d[paste0("Cp", sample_times)])
    mu <- colMeans(mm); ss <- apply(mm, 2, sd)
    lines(sample_times, mu, lty = lty[gr], lwd = 2)
    points(sample_times, mu, pch = pch[gr], cex = 0.9)
    for (j in seq_along(sample_times)) {
      cap_segment(sample_times[j], max(0, mu[j] - ss[j]), mu[j] + ss[j], cap = 2.0)
    }
  }
  legend("topright", groups, lty = lty, pch = pch, bty = "n", cex = 0.70)
  mtext("D  Illustrative experiment", side = 3, line = 0.35, adj = 0, cex = 0.96)

  # E: representative LRR duration; y starts at 0.
  base_par(mar = c(5.0, 5.8, 2.0, 0.8))
  ymax_e <- max(rep_rat$sleep, na.rm = TRUE) * 1.12
  boxplot(sleep ~ factor(group, levels = groups), data = rep_rat,
          xlab = "Treatment", ylab = "LRR/hypnosis duration (min)",
          outline = FALSE, ylim = c(0, ymax_e))
  stripchart(sleep ~ factor(group, levels = groups), data = rep_rat,
             vertical = TRUE, method = "jitter", pch = 16, cex = 0.82, add = TRUE)
  mtext("E  LRR/hypnosis endpoint", side = 3, line = 0.35, adj = 0, cex = 0.96)

  # F: representative hepatic activity; y starts at 0.
  base_par(mar = c(5.0, 5.8, 2.0, 0.8))
  ymax_f <- max(rep_rat$hepatic, na.rm = TRUE) * 1.12
  boxplot(hepatic ~ factor(group, levels = groups), data = rep_rat,
          xlab = "Treatment", ylab = "Virtual hepatic PTB activity (mg/h)",
          outline = FALSE, ylim = c(0, ymax_f))
  stripchart(hepatic ~ factor(group, levels = groups), data = rep_rat,
             vertical = TRUE, method = "jitter", pch = 16, cex = 0.82, add = TRUE)
  mtext("F  Standardized hepatic PTB activity", side = 3, line = 0.35, adj = 0, cex = 0.96)
}

png_file <- "figures/Fig_5_VirtualLab_MonteCarlo.png"
svg_file <- "figures/Fig_5_VirtualLab_MonteCarlo.svg"
safe_png(png_file, width = 14.2, height = 8.8, pointsize = 13)
draw_fig(); dev.off()
safe_svg(svg_file, width = 14.2, height = 8.8, pointsize = 13)
draw_fig(); dev.off()

cat("Code 07 completed.\n")
cat("Plasma sampling times: ", paste(sample_times, collapse = ", "), " min\n", sep = "")
cat("Figure: ", png_file, " and ", svg_file, "\n", sep = "")
cat("Table: tables/Table_MC.csv\n")
