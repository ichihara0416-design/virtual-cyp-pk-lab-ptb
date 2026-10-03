# ============================================================
# Virtual CYP-PK Lab — 日本語版 最終モデル
# RStudioではこのファイルを app.R として開き、"Run App" を押すだけで起動できます。
# ============================================================


.REQUIRED_PACKAGES <- c("shiny", "deSolve")
for (.pkg in .REQUIRED_PACKAGES) {
  if (!requireNamespace(.pkg, quietly = TRUE)) {
    install.packages(.pkg, repos = "https://cloud.r-project.org")
  }
}
rm(.pkg)

library(shiny)

RAT_BANK_CSV <- paste(c(
  "\"RatID\",\"BW_kg\",\"S_IP\",\"ka_h_1\",\"Vmax_IIV_mult\",\"C_RR_thr\"",
  "\"Rat_01\",0.296843107192152,0.527461094930437,10.66922107958,1.04554060785456,15.9574824293914",
  "\"Rat_02\",0.29356875620379,0.451629304831773,7.7071712321836,1.18300312447096,15.9574824293914",
  "\"Rat_03\",0.279294354399512,0.466833483621015,7.57739343264958,1.0497424753259,15.9574824293914",
  "\"Rat_04\",0.275178236888498,0.65122896977061,9.36132461040367,1.23059323150717,15.9574824293914",
  "\"Rat_05\",0.335272721446319,0.475732893582614,11.1796732278902,0.967177806337288,15.9574824293914",
  "\"Rat_06\",0.286344204094543,0.549816473931418,9.46899118893634,1.32901488891786,15.9574824293914",
  "\"Rat_07\",0.29863782569535,0.477915355237834,10.6825959955658,0.772015525618088,15.9574824293914",
  "\"Rat_08\",0.281418973177598,0.579249890763202,9.81558033790987,1.17371506044467,15.9574824293914",
  "\"Rat_09\",0.299432828843208,0.542420943028959,7.81578400937028,0.891194442631258,15.9574824293914",
  "\"Rat_10\",0.311811947309409,0.56116409927793,8.65272216504721,0.81904970622284,15.9574824293914",
  "\"Rat_11\",0.307062673682706,0.550272163197327,9.21357980092312,1.04943467311206,15.9574824293914",
  "\"Rat_12\",0.301528142168883,0.517514559763624,8.67107410213917,1.13505125590067,15.9574824293914",
  "\"Rat_13\",0.317244932446837,0.51620864100884,10.2851514693892,1.13799712281999,15.9574824293914",
  "\"Rat_14\",0.279092329360903,0.542544077449844,8.9906870034025,1.14048305449449,15.9574824293914",
  "\"Rat_15\",0.294292477560624,0.62738492003555,8.31244785843037,0.763193837486552,15.9574824293914",
  "\"Rat_16\",0.286547264891939,0.491218526304819,9.53432413568251,1.28258093162381,15.9574824293914",
  "\"Rat_17\",0.316669377593529,0.536678154037167,8.30185381939089,0.987875934129272,15.9574824293914",
  "\"Rat_18\",0.295193348136673,0.513038166825766,10.34983511559,0.788677592660513,15.9574824293914",
  "\"Rat_19\",0.291708039509008,0.545474575948524,9.0823581251,0.8125739896982,15.9574824293914",
  "\"Rat_20\",0.31118389328048,0.546276513880637,8.06864538657159,1.1003248110618,15.9574824293914",
  "\"Rat_21\",0.283900428548583,0.520899823539418,9.94550996111421,0.758155669865892,15.9574824293914",
  "\"Rat_22\",0.295705677189677,0.573078744563466,9.36402214535331,1.00074102770845,15.9574824293914",
  "\"Rat_23\",0.296628294086372,0.560102435935374,10.0567705626662,0.977291365502169,15.9574824293914",
  "\"Rat_24\",0.33950692530689,0.568440858728225,8.09170208229226,0.90894320729966,15.9574824293914",
  "\"Rat_25\",0.316365891890668,0.515636634948531,9.34030944536088,0.775102111569717,15.9574824293914",
  "\"Rat_26\",0.316642872192502,0.492341099197374,7.41729496655668,0.946626284249795,15.9574824293914",
  "\"Rat_27\",0.287704058802503,0.553883394921746,10.2897506645625,0.869671706219266,15.9574824293914",
  "\"Rat_28\",0.271138418043124,0.531121151962746,9.05667709030922,1.22272775425267,15.9574824293914",
  "\"Rat_29\",0.310272077071476,0.568123096262183,8.59669450024917,0.977314889919342,15.9574824293914",
  "\"Rat_30\",0.293721354477368,0.498299269119853,10.4105567453234,0.834577811573884,15.9574824293914"
), collapse = "
")


if (!requireNamespace("deSolve", quietly = TRUE)) {
  stop("Package 'deSolve' could not be installed or loaded.")
}

MODEL_DEFAULTS <- list(
  BW_ref = 0.300,       # kg
  k12 = 223.0,         # h^-1
  k21 = 13.5,          # h^-1
  k13 = 20.6,          # h^-1
  k31 = 1.37,          # h^-1
  Vmax_ref = 2.19,     # mg/h at BW_ref
  Km_ref = 0.0593,     # mg at BW_ref
  Qe_ref = 0.0246,    # L/h at BW_ref
  Vp_ref = 0.0111,     # L at BW_ref
  Kp_e = 1.38,           # effect-site:plasma equilibrium ratio
  Ve_ref = 0.00174,   # kg at BW_ref
  S_IP = 0.525942329656965,
  ka = 9.25998639080055,            # h^-1
  C_RR_thr = 15.9574824293914,   # ug/g; calibrated common righting-reflex threshold
  assay_cv = 0.06,
  brain_assay_cv = 0.06,
  default_dose_mgkg = 50,
  treatment = list(
    Control = list(vmax_mult = 1.0, km_mult = 1.0, M_RR = 1.0),
    PHB = list(vmax_mult = 2.01140248578383, km_mult = 1, M_RR = 1.07016650036501),
    OME = list(vmax_mult = 1, km_mult = 3.45637481317192, M_RR = 1.03604595595828)
  )
)


CODE03_VARIABILITY_POLICY <- list(
  IIV_BW_cv = 0.05,
  IIV_S_IP_cv = 0.10,
  IIV_ka_cv = 0.10,
  IIV_Vmax_cv = 0.15,
  blood_assay_cv = 0.06,
  brain_assay_cv = 0.06,
  hepatic_assay_cv = 0.10,
  rat_bank_seed = 20260821
)

validate_rat <- function(rat) {
  req <- c("RatID", "BW_kg", "S_IP", "ka_h_1", "Vmax_IIV_mult", "C_RR_thr")
  miss <- setdiff(req, names(rat))
  if (length(miss)) stop("Rat record missing: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

make_rat_bank <- function(n = 30, seed = 20260821,
                          bw_cv = 0.05, S_IP_cv = 0.10,
                          ka_cv = 0.10, vmax_cv = 0.15,
                          pars = MODEL_DEFAULTS) {
  set.seed(seed)
  rln_mean1 <- function(n, cv) {
    s2 <- log(1 + cv^2)
    x <- rlnorm(n, meanlog = -s2/2, sdlog = sqrt(s2))
    x / mean(x)
  }
  bw_m <- rln_mean1(n, bw_cv)
  S_IP_m <- rln_mean1(n, S_IP_cv)
  ka_m <- rln_mean1(n, ka_cv)
  vm_m <- rln_mean1(n, vmax_cv)

  data.frame(
    RatID = sprintf("Rat_%02d", seq_len(n)),
    BW_kg = pars$BW_ref * bw_m,
    S_IP = pars$S_IP * S_IP_m,
    ka_h_1 = pars$ka * ka_m,
    Vmax_IIV_mult = vm_m,
    C_RR_thr = pars$C_RR_thr,
    stringsAsFactors = FALSE
  )
}

scaled_rat_parameters <- function(rat, treatment = "Control", pars = MODEL_DEFAULTS) {
  validate_rat(rat)
  if (!treatment %in% names(pars$treatment)) stop("Unknown treatment: ", treatment)
  tr <- pars$treatment[[treatment]]
  scale <- rat$BW_kg / pars$BW_ref
  list(
    BW_kg = rat$BW_kg,
    S_IP = rat$S_IP,
    ka = rat$ka_h_1,
    k12 = pars$k12, k21 = pars$k21, k13 = pars$k13, k31 = pars$k31,
    Vp = pars$Vp_ref * scale,
    Ve = pars$Ve_ref * scale,
    Qe = pars$Qe_ref * scale^0.75,
    Kp_e = pars$Kp_e,
    Km = pars$Km_ref * scale * tr$km_mult,
    Vmax = pars$Vmax_ref * scale^0.75 * rat$Vmax_IIV_mult * tr$vmax_mult,
    C_RR_thr = pars$C_RR_thr * tr$M_RR
  )
}

.pkpd_rhs <- function(t, state, p) {
  with(as.list(c(state, p)), {
    Cp <- X1 / Vp
    Ce_state <- Xe / Ve
    input_rate <- S_IP * ka * Adep
    elim <- if (X1 <= 0) 0 else Vmax * X1 / (Km + X1)
    dAdep <- -ka * Adep
    dX1 <- input_rate - (k12 + k13) * X1 + k21 * X2 + k31 * X3 - elim
    dX2 <- k12 * X1 - k21 * X2
    dX3 <- k13 * X1 - k31 * X3
    dXe <- Qe * (Cp - Ce_state / Kp_e)
    list(c(dAdep, dX1, dX2, dX3, dXe))
  })
}

simulate_rat <- function(rat, dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
                         treatment = "Control", times_min,
                         pars = MODEL_DEFAULTS) {
  if (length(times_min) == 0 || any(!is.finite(times_min)) || any(times_min < 0)) {
    stop("times_min must contain finite non-negative times.")
  }
  p <- scaled_rat_parameters(rat, treatment, pars)
  dose_mg <- dose_mgkg * rat$BW_kg
  req_times <- sort(unique(c(0, times_min))) / 60
  y0 <- c(Adep = dose_mg, X1 = 0, X2 = 0, X3 = 0, Xe = 0)
  out <- deSolve::ode(y = y0, times = req_times, func = .pkpd_rhs,
                      parms = p, method = "lsoda",
                      rtol = 1e-9, atol = 1e-11)
  out <- as.data.frame(out)
  out$time_min <- out$time * 60
  out$Cplasma_ug_mL <- out$X1 / p$Vp
  out$Ce_ug_g <- out$Xe / p$Ve
  out$RR_absent_true <- out$Ce_ug_g >= p$C_RR_thr
  out$C_RR_thr_ug_g <- p$C_RR_thr
  out$treatment <- treatment
  out$dose_mgkg <- dose_mgkg
  idx <- vapply(times_min, function(tt) which.min(abs(out$time_min - tt)), integer(1))
  ans <- out[idx, c(
    "time_min", "Cplasma_ug_mL", "Ce_ug_g", "RR_absent_true",
    "C_RR_thr_ug_g", "treatment", "dose_mgkg"
  ), drop = FALSE]
  rownames(ans) <- NULL
  ans
}

true_state_at <- function(rat, time_min, dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
                          treatment = "Control", pars = MODEL_DEFAULTS) {
  simulate_rat(rat, dose_mgkg, treatment, time_min, pars)[1, ]
}

sample_blood <- function(rat, time_min, dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
                         treatment = "Control", assay_cv = MODEL_DEFAULTS$assay_cv,
                         add_assay_error = TRUE, pars = MODEL_DEFAULTS) {
  st <- true_state_at(rat, time_min, dose_mgkg, treatment, pars)
  measured <- st$Cplasma_ug_mL
  if (add_assay_error && assay_cv > 0) {
    s2 <- log(1 + assay_cv^2)
    measured <- measured * rlnorm(1, meanlog = -s2/2, sdlog = sqrt(s2))
  }
  data.frame(
    RatID = rat$RatID,
    treatment = treatment,
    dose_mgkg = dose_mgkg,
    sample_time_min = time_min,
    true_plasma_ug_mL = st$Cplasma_ug_mL,
    measured_plasma_ug_mL = measured,
    stringsAsFactors = FALSE
  )
}

check_righting_reflex_state <- function(rat, time_min, dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
                      treatment = "Control", pars = MODEL_DEFAULTS) {
  st <- true_state_at(rat, time_min, dose_mgkg, treatment, pars)
  data.frame(
    RatID = rat$RatID,
    treatment = treatment,
    dose_mgkg = dose_mgkg,
    check_time_min = time_min,
    righting_reflex = if (st$RR_absent_true) "absent" else "present",
    stringsAsFactors = FALSE
  )
}

terminal_brain_sample <- function(rat, time_min,
                                  dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
                                  treatment = "Control",
                                  assay_cv = MODEL_DEFAULTS$brain_assay_cv,
                                  add_assay_error = TRUE,
                                  pars = MODEL_DEFAULTS) {
  st <- true_state_at(rat, time_min, dose_mgkg, treatment, pars)
  measured <- st$Ce_ug_g
  if (isTRUE(add_assay_error) && assay_cv > 0) {
    s2 <- log(1 + assay_cv^2)
    measured <- measured * rlnorm(1, meanlog = -s2/2, sdlog = sqrt(s2))
  }
  data.frame(
    RatID = rat$RatID,
    treatment = treatment,
    dose_mgkg = dose_mgkg,
    terminal_time_min = time_min,
    brain_ug_g = measured,
    true_brain_ug_g = st$Ce_ug_g,
    plasma_ug_mL = st$Cplasma_ug_mL,
    stringsAsFactors = FALSE
  )
}

find_true_righting_reflex_transitions <- function(
    rat,
    dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
    treatment = "Control",
    initial_max_time_min = 600,
    resolution_min = 0.10,
    extend_factor = 2,
    max_horizon_min = 1440,
    pars = MODEL_DEFAULTS) {

  # This function is used only for post-experiment model-internal analysis.
  # A 0.10-min grid is sufficient for the educational app; threshold-crossing
  # times are linearly interpolated between adjacent grid points.
  horizon <- min(initial_max_time_min, max_horizon_min)

  repeat {
    grid <- seq(0, horizon, by = resolution_min)
    sim <- simulate_rat(rat, dose_mgkg, treatment, grid, pars)
    thr <- unique(sim$C_RR_thr_ug_g)
    if (!length(thr) || !is.finite(thr[1])) {
      return(list(
        onset_min = NA_real_, recovery_min = NA_real_,
        sleep_duration_min = NA_real_, search_horizon_min = horizon,
        status = "threshold unavailable"
      ))
    }

    y <- sim$Ce_ug_g - thr[1]
    idx <- which(
      is.finite(head(y, -1)) & is.finite(tail(y, -1)) &
      head(y, -1) * tail(y, -1) <= 0 &
      head(y, -1) != tail(y, -1)
    )

    crossing_time <- function(j) {
      t0 <- sim$time_min[j]
      t1 <- sim$time_min[j + 1]
      y0 <- y[j]
      y1 <- y[j + 1]
      if (!is.finite(y0) || !is.finite(y1) || y1 == y0) return(t1)
      t0 - y0 * (t1 - t0) / (y1 - y0)
    }

    if (length(idx)) {
      cr <- vapply(idx, crossing_time, numeric(1))
      cr <- cr[is.finite(cr) & cr > 1e-8]
    } else {
      cr <- numeric()
    }

    onset_candidates <- cr[vapply(cr, function(tt) {
      j <- max(1L, min(nrow(sim) - 1L, findInterval(tt, sim$time_min)))
      sim$Ce_ug_g[j + 1] >= thr[1]
    }, logical(1))]

    if (!length(onset_candidates)) {
      return(list(
        onset_min = NA_real_, recovery_min = NA_real_,
        sleep_duration_min = NA_real_, search_horizon_min = horizon,
        status = "LRR not established"
      ))
    }

    onset_time <- onset_candidates[1]
    recovery_candidates <- cr[cr > onset_time + 1e-8]
    recovery_candidates <- recovery_candidates[vapply(recovery_candidates, function(tt) {
      j <- max(1L, min(nrow(sim) - 1L, findInterval(tt, sim$time_min)))
      sim$Ce_ug_g[j + 1] < thr[1]
    }, logical(1))]

    if (length(recovery_candidates)) {
      recovery_time <- recovery_candidates[1]
      return(list(
        onset_min = onset_time,
        recovery_min = recovery_time,
        sleep_duration_min = recovery_time - onset_time,
        search_horizon_min = horizon,
        status = "recovered"
      ))
    }

    if (horizon >= max_horizon_min) break
    horizon <- min(horizon * extend_factor, max_horizon_min)
  }

  list(
    onset_min = onset_time,
    recovery_min = NA_real_,
    sleep_duration_min = NA_real_,
    search_horizon_min = horizon,
    status = "recovery not found within technical search guard"
  )
}


load_embedded_rat_bank <- function(csv_text = RAT_BANK_CSV) {
  x <- read.csv(text = csv_text, stringsAsFactors = FALSE, check.names = FALSE)
  if ("ka_h-1" %in% names(x) && !"ka_h_1" %in% names(x)) {
    names(x)[names(x) == "ka_h-1"] <- "ka_h_1"
  }
  req <- c("RatID", "BW_kg", "S_IP", "ka_h_1", "Vmax_IIV_mult", "C_RR_thr")
  miss <- setdiff(req, names(x))
  if (length(miss)) stop("Embedded rat-bank data missing columns: ", paste(miss, collapse = ", "))
  if (nrow(x) != 30L) warning("Expected 30 embedded rats; found ", nrow(x), ".")
  x
}



HEPATIC_EXVIVO_DEFAULTS <- list(
  substrate_ug_mL = 56.6,
  substrate_uM = 250,
  model_KmC_ug_mL = 5.34234234234234,
  pentobarbital_mw_g_mol = 226.23,
  assay_cv = 0.10
)

hepatic_pentobarbital_exvivo_activity <- function(
    rat, treatment = "Control", add_assay_error = TRUE,
    assay_defaults = HEPATIC_EXVIVO_DEFAULTS,
    pars = MODEL_DEFAULTS) {
  if (!treatment %in% c("Control", "PHB", "OME")) stop("Unknown treatment: ", treatment)
  p <- scaled_rat_parameters(rat, treatment, pars)

  substrate_ug_mL <- assay_defaults$substrate_ug_mL
  tr <- pars$treatment[[treatment]]
  model_Km_ug_mL <- assay_defaults$model_KmC_ug_mL * tr$km_mult

  true_activity_mg_h <- p$Vmax * substrate_ug_mL /
    (model_Km_ug_mL + substrate_ug_mL)

  measured_activity_mg_h <- true_activity_mg_h
  if (isTRUE(add_assay_error) && assay_defaults$assay_cv > 0) {
    s2 <- log(1 + assay_defaults$assay_cv^2)
    measured_activity_mg_h <- measured_activity_mg_h *
      rlnorm(1, meanlog = -s2/2, sdlog = sqrt(s2))
  }

  data.frame(
    RatID = rat$RatID,
    treatment = treatment,
    assay = "Virtual ex vivo ペントバルビタール代謝assay",
    biological_target = "標準化基質条件におけるモデル由来の肝ペントバルビタール代謝活性",
    substrate_uM = assay_defaults$substrate_uM,
    substrate_ug_mL = substrate_ug_mL,
    model_Km_ug_mL = model_Km_ug_mL,
    operative_Vmax_mg_h = p$Vmax,
    saturation_fraction = substrate_ug_mL / (model_Km_ug_mL + substrate_ug_mL),
    true_activity_mg_h = true_activity_mg_h,
    measured_activity_mg_h = measured_activity_mg_h,
    assay_cv = assay_defaults$assay_cv,
    unit = "mg/h (model-derived virtual ex vivo rate)",
    literature_basis = "Kuntzman et al. 1967: standardized PTB substrate 56.6 ug/mL (=250 uM); Holtzman & Thompson 1975: rat hepatic microsomal Michaelis-Menten hydroxylation",
    stringsAsFactors = FALSE
  )
}

.empty_liver_log <- function() {
  data.frame(
    RatID = character(), treatment = character(), virtual_clock_min = numeric(),
    elapsed_postdose_min = numeric(), assay = character(), biological_target = character(),
    substrate_uM = numeric(), substrate_ug_mL = numeric(), model_Km_ug_mL = numeric(),
    operative_Vmax_mg_h = numeric(), saturation_fraction = numeric(),
    true_activity_mg_h = numeric(), measured_activity_mg_h = numeric(),
    assay_cv = numeric(), unit = character(), literature_basis = character(),
    legacy_relative_activity = numeric(), stringsAsFactors = FALSE
  )
}

.normalize_liver_log <- function(x) {
  if (is.null(x)) return(.empty_liver_log())
  n <- nrow(x)
  for (nm in c("RatID", "treatment", "assay", "biological_target", "unit", "literature_basis")) {
    if (!nm %in% names(x)) x[[nm]] <- rep(NA_character_, n)
  }
  for (nm in c("virtual_clock_min", "elapsed_postdose_min", "substrate_uM", "substrate_ug_mL",
               "model_Km_ug_mL", "operative_Vmax_mg_h", "saturation_fraction",
               "true_activity_mg_h", "measured_activity_mg_h", "assay_cv",
               "legacy_relative_activity")) {
    if (!nm %in% names(x)) x[[nm]] <- rep(NA_real_, n)
  }
  if ("measured_relative_activity" %in% names(x)) {
    x$legacy_relative_activity <- x$measured_relative_activity
  }
  x[, names(.empty_liver_log()), drop = FALSE]
}



if (!exists("MODEL_DEFAULTS")) {
  stop("Source Virtual_CYP_PK_simulation_engine_v0.3.R before this file.")
}

EXPERIMENT_ACTION_MINUTES <- list(
  dose = 0.50,
  rr_check = 0.50,
  blood_sample = 1.00,
  rr_blood = 1.00,
  terminal_tissues = 0.00
)

new_experiment <- function(rat_bank, active_rat_ids,
                           treatment_by_rat,
                           dose_mgkg = MODEL_DEFAULTS$default_dose_mgkg,
                           action_minutes = EXPERIMENT_ACTION_MINUTES,
                           start_time_min = 0) {
  if (!all(active_rat_ids %in% rat_bank$RatID)) stop("Unknown active rat ID.")
  if (is.null(names(treatment_by_rat))) stop("treatment_by_rat must be named by RatID.")
  if (!all(active_rat_ids %in% names(treatment_by_rat))) stop("Each active rat needs a treatment assignment.")
  if (!all(unlist(treatment_by_rat[active_rat_ids]) %in% names(MODEL_DEFAULTS$treatment))) {
    stop("Treatment must be Control, PHB, or OME.")
  }

  e <- new.env(parent = emptyenv())
  e$clock_min <- as.numeric(start_time_min)
  e$rat_bank <- rat_bank
  e$active_rat_ids <- active_rat_ids
  e$treatment <- as.list(treatment_by_rat)
  e$dose_mgkg <- as.numeric(dose_mgkg)
  e$action_minutes <- action_minutes
  e$dose_time <- setNames(rep(NA_real_, length(active_rat_ids)), active_rat_ids)
  e$terminal <- setNames(rep(FALSE, length(active_rat_ids)), active_rat_ids)
  e$blood_sample_count <- setNames(rep(0L, length(active_rat_ids)), active_rat_ids)
  e$latest_rr <- setNames(rep(NA_character_, length(active_rat_ids)), active_rat_ids)
  e$latest_rr_time <- setNames(rep(NA_real_, length(active_rat_ids)), active_rat_ids)

  e$event_log <- data.frame(
    event_id = integer(), clock_start_min = numeric(), clock_end_min = numeric(),
    RatID = character(), event = character(), treatment = character(),
    elapsed_postdose_min = numeric(), result = character(),
    stringsAsFactors = FALSE
  )
  e$blood_log <- data.frame(
    RatID = character(), treatment = character(), dose_mgkg = numeric(),
    virtual_clock_min = numeric(), elapsed_postdose_min = numeric(), sample_number = integer(),
    measured_plasma_ug_mL = numeric(), true_plasma_ug_mL = numeric(),
    stringsAsFactors = FALSE
  )
  e$rr_log <- data.frame(
    RatID = character(), treatment = character(), dose_mgkg = numeric(),
    virtual_clock_min = numeric(), elapsed_postdose_min = numeric(), righting_reflex = character(),
    stringsAsFactors = FALSE
  )
  e$brain_log <- data.frame(
    RatID = character(), treatment = character(), dose_mgkg = numeric(),
    virtual_clock_min = numeric(), elapsed_postdose_min = numeric(),
    brain_ug_g = numeric(), true_brain_ug_g = numeric(), plasma_ug_mL = numeric(),
    stringsAsFactors = FALSE
  )
  e$liver_log <- .empty_liver_log()
  class(e) <- c("virtual_cyp_pk_experiment", "environment")
  e
}

.experiment_rat <- function(e, rat_id, allow_terminal = FALSE) {
  if (!rat_id %in% e$active_rat_ids) stop("Rat is not active in this experiment: ", rat_id)
  if (!allow_terminal && isTRUE(e$terminal[[rat_id]])) stop("Rat has already undergone terminal collection: ", rat_id)
  as.list(e$rat_bank[e$rat_bank$RatID == rat_id, , drop = FALSE][1, ])
}

.require_dosed <- function(e, rat_id) {
  if (is.na(e$dose_time[[rat_id]])) stop("ペントバルビタールはまだ投与されていません: ", rat_id)
}

.postdose_time <- function(e, rat_id, at_clock = e$clock_min) {
  if (is.na(e$dose_time[[rat_id]])) return(NA_real_)
  at_clock - e$dose_time[[rat_id]]
}

.log_event <- function(e, start, end, rat_id, event, result = "") {
  tr <- if (nzchar(rat_id)) as.character(e$treatment[[rat_id]]) else ""
  elapsed <- if (nzchar(rat_id)) .postdose_time(e, rat_id, end) else NA_real_
  e$event_log <- rbind(e$event_log, data.frame(
    event_id = nrow(e$event_log) + 1L,
    clock_start_min = start,
    clock_end_min = end,
    RatID = rat_id,
    event = event,
    treatment = tr,
    elapsed_postdose_min = elapsed,
    result = as.character(result),
    stringsAsFactors = FALSE
  ))
  invisible(e)
}

wait_minutes <- function(e, minutes) {
  if (!is.finite(minutes) || minutes < 0) stop("Wait time must be non-negative.")
  start <- e$clock_min
  e$clock_min <- e$clock_min + minutes
  .log_event(e, start, e$clock_min, "", "wait", sprintf("%.2f min", minutes))
  invisible(e)
}

administer_pentobarbital <- function(e, rat_id) {
  .experiment_rat(e, rat_id)
  if (!is.na(e$dose_time[[rat_id]])) stop("ペントバルビタールはすでに投与されています: ", rat_id)
  start <- e$clock_min
  e$clock_min <- e$clock_min + e$action_minutes$dose
  e$dose_time[[rat_id]] <- e$clock_min
  .log_event(e, start, e$clock_min, rat_id, "pentobarbital_dose",
             sprintf("%.1f mg/kg i.p.", e$dose_mgkg))
  invisible(e)
}

check_righting_reflex <- function(e, rat_id) {
  rat <- .experiment_rat(e, rat_id)
  .require_dosed(e, rat_id)
  start <- e$clock_min
  e$clock_min <- e$clock_min + e$action_minutes$rr_check
  tpost <- .postdose_time(e, rat_id, e$clock_min)
  obs <- check_righting_reflex_state(rat, time_min = tpost, dose_mgkg = e$dose_mgkg,
                   treatment = as.character(e$treatment[[rat_id]]))
  e$latest_rr[[rat_id]] <- obs$righting_reflex
  e$latest_rr_time[[rat_id]] <- e$clock_min
  .log_event(e, start, e$clock_min, rat_id, "righting_reflex_check", obs$righting_reflex)

  row <- data.frame(
    RatID = rat_id,
    treatment = as.character(e$treatment[[rat_id]]),
    dose_mgkg = e$dose_mgkg,
    virtual_clock_min = e$clock_min,
    elapsed_postdose_min = tpost,
    righting_reflex = obs$righting_reflex,
    stringsAsFactors = FALSE
  )
  e$rr_log <- rbind(e$rr_log, row)
  row
}

collect_blood <- function(e, rat_id, add_assay_error = TRUE) {
  rat <- .experiment_rat(e, rat_id)
  .require_dosed(e, rat_id)
  start <- e$clock_min
  e$clock_min <- e$clock_min + e$action_minutes$blood_sample
  tpost <- .postdose_time(e, rat_id, e$clock_min)
  ans <- sample_blood(rat, time_min = tpost, dose_mgkg = e$dose_mgkg,
                      treatment = as.character(e$treatment[[rat_id]]),
                      add_assay_error = add_assay_error)
  e$blood_sample_count[[rat_id]] <- e$blood_sample_count[[rat_id]] + 1L
  n <- e$blood_sample_count[[rat_id]]
  .log_event(e, start, e$clock_min, rat_id, "blood_sample",
             sprintf("measured %.3f ug/mL", ans$measured_plasma_ug_mL))

  row <- data.frame(
    RatID = rat_id,
    treatment = as.character(e$treatment[[rat_id]]),
    dose_mgkg = e$dose_mgkg,
    virtual_clock_min = e$clock_min,
    elapsed_postdose_min = tpost,
    sample_number = n,
    measured_plasma_ug_mL = ans$measured_plasma_ug_mL,
    true_plasma_ug_mL = ans$true_plasma_ug_mL,
    stringsAsFactors = FALSE
  )
  e$blood_log <- rbind(e$blood_log, row)
  row
}

collect_lrr_and_blood <- function(e, rat_id, add_assay_error = TRUE) {
  rat <- .experiment_rat(e, rat_id)
  .require_dosed(e, rat_id)
  start <- e$clock_min
  e$clock_min <- e$clock_min + e$action_minutes$rr_blood
  tpost <- .postdose_time(e, rat_id, e$clock_min)
  tr <- as.character(e$treatment[[rat_id]])

  obs <- check_righting_reflex_state(rat, time_min = tpost, dose_mgkg = e$dose_mgkg, treatment = tr)
  ans <- sample_blood(rat, time_min = tpost, dose_mgkg = e$dose_mgkg,
                      treatment = tr, add_assay_error = add_assay_error)

  e$latest_rr[[rat_id]] <- obs$righting_reflex
  e$latest_rr_time[[rat_id]] <- e$clock_min
  e$blood_sample_count[[rat_id]] <- e$blood_sample_count[[rat_id]] + 1L
  n <- e$blood_sample_count[[rat_id]]

  .log_event(e, start, e$clock_min, rat_id, "righting_reflex_and_blood_sample",
             sprintf("righting reflex %s; measured %.3f ug/mL", obs$righting_reflex, ans$measured_plasma_ug_mL))

  rr_row <- data.frame(
    RatID = rat_id,
    treatment = tr,
    dose_mgkg = e$dose_mgkg,
    virtual_clock_min = e$clock_min,
    elapsed_postdose_min = tpost,
    righting_reflex = obs$righting_reflex,
    stringsAsFactors = FALSE
  )
  e$rr_log <- rbind(e$rr_log, rr_row)

  blood_row <- data.frame(
    RatID = rat_id,
    treatment = tr,
    dose_mgkg = e$dose_mgkg,
    virtual_clock_min = e$clock_min,
    elapsed_postdose_min = tpost,
    sample_number = n,
    measured_plasma_ug_mL = ans$measured_plasma_ug_mL,
    true_plasma_ug_mL = ans$true_plasma_ug_mL,
    stringsAsFactors = FALSE
  )
  e$blood_log <- rbind(e$blood_log, blood_row)

  list(lrr = rr_row, blood = blood_row)
}

collect_terminal_tissues <- function(e, rat_id) {
  rat <- .experiment_rat(e, rat_id)
  .require_dosed(e, rat_id)
  start <- e$clock_min
  e$clock_min <- e$clock_min + e$action_minutes$terminal_tissues
  tpost <- .postdose_time(e, rat_id, e$clock_min)
  tr <- as.character(e$treatment[[rat_id]])

  brain <- terminal_brain_sample(
    rat, time_min = tpost, dose_mgkg = e$dose_mgkg, treatment = tr,
    add_assay_error = TRUE
  )
  liver <- hepatic_pentobarbital_exvivo_activity(
    rat, treatment = tr, add_assay_error = TRUE
  )

  .log_event(
    e, start, e$clock_min, rat_id, "terminal_tissue_collection",
    sprintf("brain %.3f ug/g; virtual ex vivo hepatic PTB activity %.3f mg/h at %.0f uM substrate",
            brain$brain_ug_g, liver$measured_activity_mg_h, liver$substrate_uM)
  )
  e$terminal[[rat_id]] <- TRUE

  brain_row <- data.frame(
    RatID = rat_id, treatment = tr, dose_mgkg = e$dose_mgkg,
    virtual_clock_min = e$clock_min, elapsed_postdose_min = tpost,
    brain_ug_g = brain$brain_ug_g, true_brain_ug_g = brain$true_brain_ug_g,
    plasma_ug_mL = brain$plasma_ug_mL,
    stringsAsFactors = FALSE
  )
  e$brain_log <- rbind(e$brain_log, brain_row)

  liver_row <- data.frame(
    RatID = rat_id, treatment = tr, virtual_clock_min = e$clock_min,
    elapsed_postdose_min = tpost,
    assay = liver$assay, biological_target = liver$biological_target,
    substrate_uM = liver$substrate_uM,
    substrate_ug_mL = liver$substrate_ug_mL,
    model_Km_ug_mL = liver$model_Km_ug_mL,
    operative_Vmax_mg_h = liver$operative_Vmax_mg_h,
    saturation_fraction = liver$saturation_fraction,
    true_activity_mg_h = liver$true_activity_mg_h,
    measured_activity_mg_h = liver$measured_activity_mg_h,
    assay_cv = liver$assay_cv,
    unit = liver$unit,
    literature_basis = liver$literature_basis,
    legacy_relative_activity = NA_real_,
    stringsAsFactors = FALSE
  )
  e$liver_log <- rbind(e$liver_log, liver_row)

  list(brain = brain_row, liver = liver_row)
}

experiment_status <- function(e) {
  data.frame(
    RatID = e$active_rat_ids,
    treatment = vapply(e$active_rat_ids, function(x) as.character(e$treatment[[x]]), character(1)),
    dose_time_min = unname(e$dose_time[e$active_rat_ids]),
    elapsed_postdose_min = vapply(e$active_rat_ids, function(x) .postdose_time(e, x), numeric(1)),
    latest_rr = unname(e$latest_rr[e$active_rat_ids]),
    latest_lrr_clock_min = unname(e$latest_rr_time[e$active_rat_ids]),
    blood_samples = unname(e$blood_sample_count[e$active_rat_ids]),
    terminal = unname(e$terminal[e$active_rat_ids]),
    stringsAsFactors = FALSE
  )
}


# Student-facing log exports -------------------------------------------------
# Tab 3 is intended to contain only information actually observed/recorded
# during the virtual experiment. Model-internal "true" values and operative
# parameters remain available separately in Tab 4 after the experiment ends.

.event_label_ja <- function(x) {
  map <- c(
    pentobarbital_dose = "ペントバルビタール投与",
    righting_reflex_check = "正向反射確認",
    blood_sample = "採血",
    righting_reflex_and_blood_sample = "正向反射確認＋採血",
    terminal_tissue_collection = "組織採取（肝臓＋脳）",
    wait = "待機"
  )
  ans <- unname(map[as.character(x)])
  ans[is.na(ans)] <- as.character(x)[is.na(ans)]
  ans
}

.event_result_ja <- function(x) {
  x <- as.character(x)

  x <- sub(
    "^righting reflex present; measured ([0-9.]+) ug/mL$",
    "正向反射あり（覚醒）；血中PTB濃度 \\1 µg/mL",
    x
  )
  x <- sub(
    "^righting reflex absent; measured ([0-9.]+) ug/mL$",
    "正向反射なし（麻酔）；血中PTB濃度 \\1 µg/mL",
    x
  )
  x <- sub(
    "^measured ([0-9.]+) ug/mL$",
    "血中PTB濃度 \\1 µg/mL",
    x
  )
  x <- sub(
    "^brain ([0-9.]+) ug/g; virtual ex vivo hepatic PTB activity ([0-9.]+) mg/h at ([0-9.]+) uM substrate$",
    "脳内PTB濃度 \\1 µg/g；肝virtual ex vivo PTB代謝活性 \\2 mg/h（基質 \\3 µM）",
    x
  )
  x
}

student_event_log <- function(e) {
  x <- e$event_log
  if (!nrow(x)) {
    return(data.frame(
      `操作番号` = integer(),
      `開始時刻（min）` = numeric(),
      `終了時刻（min）` = numeric(),
      Rat = character(),
      `操作` = character(),
      `群` = character(),
      `投与後時間（min）` = numeric(),
      `結果` = character(),
      check.names = FALSE
    ))
  }

  data.frame(
    `操作番号` = x$event_id,
    `開始時刻（min）` = round(x$clock_start_min, 2),
    `終了時刻（min）` = round(x$clock_end_min, 2),
    Rat = ifelse(is.na(x$RatID), "", x$RatID),
    `操作` = .event_label_ja(x$event),
    `群` = ifelse(is.na(x$treatment), "", x$treatment),
    `投与後時間（min）` = round(x$elapsed_postdose_min, 2),
    `結果` = .event_result_ja(x$result),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_blood_log <- function(e) {
  x <- e$blood_log
  data.frame(
    Rat = x$RatID,
    `群` = x$treatment,
    `PTB投与量（mg/kg）` = x$dose_mgkg,
    `仮想実験時刻（min）` = round(x$virtual_clock_min, 2),
    `PTB投与後時間（min）` = round(x$elapsed_postdose_min, 2),
    `採血回数` = x$sample_number,
    `測定血中PTB濃度（µg/mL）` = round(x$measured_plasma_ug_mL, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_rr_log <- function(e) {
  x <- e$rr_log
  rr <- ifelse(
    x$righting_reflex == "absent",
    "なし（麻酔）",
    ifelse(x$righting_reflex == "present", "あり（覚醒）", x$righting_reflex)
  )
  data.frame(
    Rat = x$RatID,
    `群` = x$treatment,
    `PTB投与量（mg/kg）` = x$dose_mgkg,
    `仮想実験時刻（min）` = round(x$virtual_clock_min, 2),
    `PTB投与後時間（min）` = round(x$elapsed_postdose_min, 2),
    `正向反射` = rr,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_brain_log <- function(e) {
  x <- e$brain_log
  data.frame(
    Rat = x$RatID,
    `群` = x$treatment,
    `PTB投与量（mg/kg）` = x$dose_mgkg,
    `組織採取時刻（min）` = round(x$virtual_clock_min, 2),
    `PTB投与後時間（min）` = round(x$elapsed_postdose_min, 2),
    `測定脳内PTB濃度（µg/g）` = round(x$brain_ug_g, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_liver_log <- function(e) {
  x <- .normalize_liver_log(e$liver_log)
  data.frame(
    Rat = x$RatID,
    `群` = x$treatment,
    `組織採取時刻（min）` = round(x$virtual_clock_min, 2),
    `PTB投与後時間（min）` = round(x$elapsed_postdose_min, 2),
    `共通基質濃度（µM）` = x$substrate_uM,
    `共通基質濃度（µg/mL）` = x$substrate_ug_mL,
    `測定肝virtual_ex_vivo_PTB代謝活性（mg/h）` =
      round(x$measured_activity_mg_h, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Excel on Windows may misinterpret a plain UTF-8 CSV that contains Japanese.
# Prepending the UTF-8 BOM makes the exported files open correctly in Excel
# while remaining valid UTF-8 CSV files.
write_csv_utf8_bom <- function(x, file, row.names = FALSE, na = "") {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  utils::write.csv(
    x, tmp,
    row.names = row.names,
    na = na,
    fileEncoding = "UTF-8"
  )

  raw_data <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
  con <- base::file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(raw_data, con)
  invisible(file)
}

export_experiment_csv <- function(e, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  write_csv_utf8_bom(
    student_event_log(e),
    file.path(directory, "experiment_log.csv")
  )
  write_csv_utf8_bom(
    student_blood_log(e),
    file.path(directory, "blood_samples.csv")
  )
  write_csv_utf8_bom(
    student_rr_log(e),
    file.path(directory, "righting_reflex_checks.csv")
  )
  write_csv_utf8_bom(
    student_brain_log(e),
    file.path(directory, "terminal_brain_samples.csv")
  )
  write_csv_utf8_bom(
    student_liver_log(e),
    file.path(directory, "terminal_liver_virtual_exvivo_PTB_activity.csv")
  )

  invisible(directory)
}

experiment_snapshot <- function(e, model_version = "Code03-JA-CLprimary-MRR-v1.4") {
  list(
    format = "Virtual_CYP_PK_session",
    format_version = 1L,
    model_version = model_version,
    saved_at = as.character(Sys.time()),
    clock_min = e$clock_min,
    rat_bank = e$rat_bank,
    active_rat_ids = e$active_rat_ids,
    treatment = e$treatment,
    dose_mgkg = e$dose_mgkg,
    action_minutes = e$action_minutes,
    dose_time = e$dose_time,
    terminal = e$terminal,
    blood_sample_count = e$blood_sample_count,
    latest_rr = e$latest_rr,
    latest_rr_time = e$latest_rr_time,
    event_log = e$event_log,
    blood_log = e$blood_log,
    rr_log = e$rr_log,
    brain_log = e$brain_log,
    liver_log = e$liver_log,
    random_seed = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      get(".Random.seed", envir = .GlobalEnv) else NULL
  )
}

experiment_from_snapshot <- function(x) {
  if (!is.list(x) || !(x$format %in% c("Virtual_CYP_PK_session", "Virtual_CYP_PKPD_session"))) {
    stop("This is not a Virtual CYP-PK/PD Lab session file.")
  }
  e <- new.env(parent = emptyenv())
  fields <- c("clock_min","rat_bank","active_rat_ids","treatment","dose_mgkg",
              "action_minutes","dose_time","terminal","blood_sample_count",
              "latest_rr","latest_rr_time","event_log","blood_log","rr_log",
              "brain_log","liver_log")
  for (nm in fields) e[[nm]] <- x[[nm]]
  if (!is.null(e$brain_log) && !("true_brain_ug_g" %in% names(e$brain_log))) {
    e$brain_log$true_brain_ug_g <- e$brain_log$brain_ug_g
  }
  if (is.null(e$brain_log)) {
    e$brain_log <- data.frame(
      RatID=character(), treatment=character(), dose_mgkg=numeric(),
      virtual_clock_min=numeric(), elapsed_postdose_min=numeric(),
      brain_ug_g=numeric(), true_brain_ug_g=numeric(), plasma_ug_mL=numeric(),
      stringsAsFactors=FALSE
    )
  }
  e$liver_log <- .normalize_liver_log(e$liver_log)
  class(e) <- c("virtual_cyp_pk_experiment", "environment")
  if (!is.null(x$random_seed)) assign(".Random.seed", x$random_seed, envir = .GlobalEnv)
  e
}

save_experiment_session <- function(e, file, model_version = "Code03-JA-CLprimary-MRR-v1.4") {
  saveRDS(experiment_snapshot(e, model_version), file = file, compress = "xz")
}

load_experiment_session <- function(file) {
  experiment_from_snapshot(readRDS(file))
}


rat_bank <- load_embedded_rat_bank()

stopifnot(
  nrow(rat_bank) == 30L,
  length(unique(rat_bank$RatID)) == 30L,
  all(is.finite(rat_bank$BW_kg)),
  all(is.finite(rat_bank$S_IP)),
  all(is.finite(rat_bank$ka_h_1)),
  all(is.finite(rat_bank$Vmax_IIV_mult)),
  max(abs(rat_bank$C_RR_thr - MODEL_DEFAULTS$C_RR_thr)) < 1e-10
)



stopifnot(
  isTRUE(all.equal(as.numeric(formals(make_rat_bank)$bw_cv), CODE03_VARIABILITY_POLICY$IIV_BW_cv)),
  isTRUE(all.equal(as.numeric(formals(make_rat_bank)$S_IP_cv), CODE03_VARIABILITY_POLICY$IIV_S_IP_cv)),
  isTRUE(all.equal(as.numeric(formals(make_rat_bank)$ka_cv), CODE03_VARIABILITY_POLICY$IIV_ka_cv)),
  isTRUE(all.equal(as.numeric(formals(make_rat_bank)$vmax_cv), CODE03_VARIABILITY_POLICY$IIV_Vmax_cv)),
  isTRUE(all.equal(MODEL_DEFAULTS$assay_cv, CODE03_VARIABILITY_POLICY$blood_assay_cv)),
  isTRUE(all.equal(MODEL_DEFAULTS$brain_assay_cv, CODE03_VARIABILITY_POLICY$brain_assay_cv)),
  isTRUE(all.equal(HEPATIC_EXVIVO_DEFAULTS$assay_cv, CODE03_VARIABILITY_POLICY$hepatic_assay_cv)),
  nrow(rat_bank) == 30L,
  length(unique(rat_bank$RatID)) == 30L
)



library(shiny)




fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits))
}

treatment_label <- function(x) {
  c(Control = "Control（無処置）",
    PHB = "フェノバルビタール（PHB：酸化的薬物代謝誘導）",
    OME = "オメプラゾール（OME：酸化的薬物代謝阻害）")[x]
}

experiment_visual_guide_ja <- function() {
  div(class = "reference-box app-lead",
      div(class = "panel-title-strong", "実験の流れ"),
      tags$div(
        style = "overflow-x:auto;",
        HTML('
        <svg viewBox="0 0 1120 280" width="100%" style="min-width:820px; max-width:1120px;">
          <defs>
            <marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto">
              <path d="M0,0 L0,6 L9,3 z" fill="#4b5563"></path>
            </marker>
          </defs>

          <rect x="20" y="75" width="180" height="120" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="110" y="102" text-anchor="middle" font-size="18" font-weight="700">群分け</text>
          <text x="110" y="132" text-anchor="middle" font-size="16">Control 3匹</text>
          <text x="110" y="157" text-anchor="middle" font-size="16">PHB 3匹</text>
          <text x="110" y="182" text-anchor="middle" font-size="16">OME 3匹</text>

          <line x1="200" y1="135" x2="280" y2="135" stroke="#4b5563" stroke-width="2.5" marker-end="url(#arrow)"/>

          <rect x="285" y="55" width="235" height="160" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="402" y="83" text-anchor="middle" font-size="18" font-weight="700">ペントバルビタール投与</text>
          <text x="402" y="114" text-anchor="middle" font-size="15">経時的に正向反射を確認</text>
          <text x="402" y="141" text-anchor="middle" font-size="15">正向反射確認＋採血</text>
          <text x="402" y="168" text-anchor="middle" font-size="15">採血目安：15・30・60・120分</text>
          <text x="402" y="195" text-anchor="middle" font-size="15">必要に応じて追加測定</text>

          <line x1="520" y1="135" x2="595" y2="135" stroke="#4b5563" stroke-width="2.5" marker-end="url(#arrow)"/>

          <rect x="600" y="42" width="225" height="190" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="712" y="70" text-anchor="middle" font-size="18" font-weight="700">組織採取</text>
          <text x="712" y="101" text-anchor="middle" font-size="16">脳</text>
          <text x="712" y="125" text-anchor="middle" font-size="14">仮想「脳内濃度」</text>
          <text x="712" y="154" text-anchor="middle" font-size="16">肝臓</text>
          <text x="712" y="178" text-anchor="middle" font-size="14">virtual ex vivo代謝assay</text>
          <text x="712" y="207" text-anchor="middle" font-size="12.5">採取後はそのラットに</text>
          <text x="712" y="224" text-anchor="middle" font-size="12.5">追加操作できません</text>

          <line x1="825" y1="135" x2="895" y2="135" stroke="#4b5563" stroke-width="2.5" marker-end="url(#arrow)"/>

          <rect x="900" y="75" width="195" height="120" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="997" y="104" text-anchor="middle" font-size="18" font-weight="700">結果を比較</text>
          <text x="997" y="137" text-anchor="middle" font-size="15">血中PTB濃度</text>
          <text x="997" y="162" text-anchor="middle" font-size="15">麻酔持続時間</text>
          <text x="997" y="187" text-anchor="middle" font-size="15">脳・肝の終末測定</text>
        </svg>')
      ),

  )
}

build_debrief_summary <- function(e, transitions = NULL) {
  rows <- lapply(e$active_rat_ids, function(rat_id) {
    r <- as.list(e$rat_bank[e$rat_bank$RatID == rat_id, , drop = FALSE][1, ])
    tr <- as.character(e$treatment[[rat_id]])
    p <- scaled_rat_parameters(r, tr)

    z <- if (!is.na(e$dose_time[[rat_id]])) {
      if (!is.null(transitions) && !is.null(transitions[[rat_id]])) {
        transitions[[rat_id]]
      } else {
        find_true_righting_reflex_transitions(
          r, dose_mgkg = e$dose_mgkg, treatment = tr
        )
      }
    } else {
      list(onset_min = NA_real_, recovery_min = NA_real_,
           sleep_duration_min = NA_real_, search_horizon_min = NA_real_,
           status = "not dosed")
    }

    liver <- e$liver_log[e$liver_log$RatID == rat_id, , drop = FALSE]
    brain <- e$brain_log[e$brain_log$RatID == rat_id, , drop = FALSE]

    cp_times <- c(15, 30, 60, 120)
    cp_vals <- setNames(rep(NA_real_, length(cp_times)), cp_times)
    if (!is.na(e$dose_time[[rat_id]])) {
      cp_sim <- simulate_rat(
        r, dose_mgkg = e$dose_mgkg, treatment = tr, times_min = cp_times
      )
      cp_vals[] <- cp_sim$Cplasma_ug_mL
    }

    data.frame(
      RatID = rat_id,
      treatment = tr,
      BW_kg = r$BW_kg,
      F_eff = r$S_IP,
      ka_h_1 = r$ka_h_1,
      Vmax_IIV_mult = r$Vmax_IIV_mult,
      treatment_Vmax_relative_change = MODEL_DEFAULTS$treatment[[tr]]$vmax_mult,
      operative_Vmax_mg_h = p$Vmax,
      treatment_Km_relative_change = MODEL_DEFAULTS$treatment[[tr]]$km_mult,
      operative_Km_mg = p$Km,
      M_RR = MODEL_DEFAULTS$treatment[[tr]]$M_RR,
      baseline_C_RR_thr_ug_g = MODEL_DEFAULTS$C_RR_thr,
      effective_C_RR_thr_ug_g = p$C_RR_thr,
      true_Cp_15_ug_mL = unname(cp_vals["15"]),
      true_Cp_30_ug_mL = unname(cp_vals["30"]),
      true_Cp_60_ug_mL = unname(cp_vals["60"]),
      true_Cp_120_ug_mL = unname(cp_vals["120"]),
      true_LRR_onset_min = z$onset_min,
      true_LRR_recovery_min = z$recovery_min,
      true_LRR_duration_min = z$sleep_duration_min,
      blood_samples_n = sum(e$blood_log$RatID == rat_id),
      terminal_brain_ug_g = if (nrow(brain)) tail(brain$brain_ug_g, 1) else NA_real_,
      terminal_brain_true_ug_g = if (nrow(brain)) tail(brain$true_brain_ug_g, 1) else NA_real_,
      hepatic_exvivo_activity_mg_h = if (nrow(liver)) tail(liver$measured_activity_mg_h, 1) else NA_real_,
      hepatic_exvivo_true_activity_mg_h = if (nrow(liver)) tail(liver$true_activity_mg_h, 1) else NA_real_,
      hepatic_exvivo_substrate_uM = if (nrow(liver)) tail(liver$substrate_uM, 1) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

plot_debrief_rat <- function(e, rat_id, transition = NULL, trajectory = NULL) {
  if (is.na(e$dose_time[[rat_id]])) {
    plot.new(); text(0.5, 0.5, "ペントバルビタールは未投与です。")
    return(invisible(NULL))
  }
  r <- as.list(e$rat_bank[e$rat_bank$RatID == rat_id, , drop = FALSE][1, ])
  tr <- as.character(e$treatment[[rat_id]])
  if (is.null(trajectory)) {
    max_obs <- suppressWarnings(max(c(
      120,
      e$blood_log$elapsed_postdose_min[e$blood_log$RatID == rat_id],
      e$rr_log$elapsed_postdose_min[e$rr_log$RatID == rat_id]
    ), na.rm = TRUE))
    times <- seq(0, max_obs + 20, by = 0.25)
    sim <- simulate_rat(r, e$dose_mgkg, tr, times)
  } else {
    sim <- trajectory
  }
  p <- scaled_rat_parameters(r, tr)

  ymax <- max(sim$Cplasma_ug_mL, sim$Ce_ug_g, p$C_RR_thr, na.rm = TRUE) * 1.08
  plot(sim$time_min, sim$Cplasma_ug_mL, type = "l", lwd = 2,
       xlab = "Time after pentobarbital administration (min)",
       ylab = "Pentobarbital concentration (plasma: µg/mL; effect site: µg/g)",
       ylim = c(0, ymax), main = paste(rat_id, tr, sep = " / "))
  lines(sim$time_min, sim$Ce_ug_g, lwd = 2, lty = 2)
  abline(h = p$C_RR_thr, lty = 3)

  b <- e$blood_log[e$blood_log$RatID == rat_id, , drop = FALSE]
  if (nrow(b)) points(b$elapsed_postdose_min, b$measured_plasma_ug_mL, pch = 19)

  z <- if (!is.null(transition)) {
    transition
  } else {
    find_true_righting_reflex_transitions(
      r, dose_mgkg = e$dose_mgkg, treatment = tr
    )
  }
  if (is.finite(z$onset_min)) abline(v = z$onset_min, lty = 4)
  if (is.finite(z$recovery_min)) abline(v = z$recovery_min, lty = 4)

  lrr <- e$rr_log[e$rr_log$RatID == rat_id, , drop = FALSE]
  if (nrow(lrr)) {
    lrr <- lrr[order(lrr$elapsed_postdose_min), , drop = FALSE]
    first_abs_i <- which(lrr$righting_reflex == "absent")[1]
    observed_points <- data.frame()
    if (is.finite(first_abs_i)) {
      first_abs <- lrr[first_abs_i, , drop = FALSE]
      later_present <- which(
        seq_len(nrow(lrr)) > first_abs_i & lrr$righting_reflex == "present"
      )
      observed_points <- first_abs
      observed_points$plot_type <- "first_absent"
      if (length(later_present)) {
        rec <- lrr[later_present[1], , drop = FALSE]
        rec$plot_type <- "first_recovery"
        observed_points <- rbind(observed_points, rec)
      }
    } else {
      present_i <- which(lrr$righting_reflex == "present")[1]
      if (is.finite(present_i)) {
        observed_points <- lrr[present_i, , drop = FALSE]
        observed_points$plot_type <- "present_only"
      }
    }
    if (nrow(observed_points)) {
      oy <- approx(sim$time_min, sim$Ce_ug_g,
                   xout = observed_points$elapsed_postdose_min, rule = 2)$y
      opch <- ifelse(observed_points$plot_type == "first_absent", 4, 1)
      points(observed_points$elapsed_postdose_min, oy, pch = opch, cex = 1.15)
    }
  }

  legend("topright",
         legend = c(
           "True plasma concentration",
           "Effect-site concentration (Ce)",
           "C_RR,thr × M_RR: effective righting-reflex threshold",
           "Measured blood concentration",
           "True righting-reflex transition",
           "First observed righting reflex absent",
           "First observed recovery"
         ),
         lty = c(1, 2, 3, NA, 4, NA, NA),
         pch = c(NA, NA, NA, 19, NA, 4, 1),
         lwd = c(2, 2, 1, NA, 1, NA, NA), bty = "n", cex = 0.82)
}

nearest_sample_to <- function(e, target_min = 30) {
  if (!nrow(e$blood_log)) return(data.frame())
  spl <- split(e$blood_log, e$blood_log$RatID)
  x <- lapply(spl, function(d) d[which.min(abs(d$elapsed_postdose_min - target_min)), , drop=FALSE])
  do.call(rbind, x)
}


MODEL_SCHEMATIC_SVG <- paste(c(
  "<svg",
  "   viewBox=\"0 0 1800 1280\"",
  "   preserveAspectRatio=\"xMidYMid meet\"",
  "   style=\"width:100%;height:auto;max-width:100%;display:block;\"",
  "   version=\"1.1\"",
  "   id=\"svg75\"",
  "   xmlns:xlink=\"http://www.w3.org/1999/xlink\"",
  "   xmlns=\"http://www.w3.org/2000/svg\"",
  "  <defs",
  "     id=\"defs9\">",
  "    <linearGradient",
  "       id=\"blueFill\"",
  "       x1=\"0\"",
  "       y1=\"0\"",
  "       x2=\"0\"",
  "       y2=\"1\">",
  "      <stop",
  "         offset=\"0%\"",
  "         stop-color=\"#eef6ff\"",
  "         id=\"stop1\" />",
  "      <stop",
  "         offset=\"100%\"",
  "         stop-color=\"#dbeeff\"",
  "         id=\"stop2\" />",
  "    </linearGradient>",
  "    <linearGradient",
  "       id=\"purpleFill\"",
  "       x1=\"0\"",
  "       y1=\"0\"",
  "       x2=\"0\"",
  "       y2=\"1\">",
  "      <stop",
  "         offset=\"0%\"",
  "         stop-color=\"#faf5ff\"",
  "         id=\"stop3\" />",
  "      <stop",
  "         offset=\"100%\"",
  "         stop-color=\"#f2e8ff\"",
  "         id=\"stop4\" />",
  "    </linearGradient>",
  "    <linearGradient",
  "       id=\"amberFill\"",
  "       x1=\"0\"",
  "       y1=\"0\"",
  "       x2=\"0\"",
  "       y2=\"1\">",
  "      <stop",
  "         offset=\"0%\"",
  "         stop-color=\"#fff9e8\"",
  "         id=\"stop5\" />",
  "      <stop",
  "         offset=\"100%\"",
  "         stop-color=\"#fff0c9\"",
  "         id=\"stop6\" />",
  "    </linearGradient>",
  "    <linearGradient",
  "       id=\"greenFill\"",
  "       x1=\"0\"",
  "       y1=\"0\"",
  "       x2=\"0\"",
  "       y2=\"1\">",
  "      <stop",
  "         offset=\"0%\"",
  "         stop-color=\"#f1fbf2\"",
  "         id=\"stop7\" />",
  "      <stop",
  "         offset=\"100%\"",
  "         stop-color=\"#def3e1\"",
  "         id=\"stop8\" />",
  "    </linearGradient>",
  "    <marker",
  "       id=\"arrowSolid\"",
  "       markerWidth=\"12\"",
  "       markerHeight=\"12\"",
  "       refX=\"10\"",
  "       refY=\"4\"",
  "       orient=\"auto\">",
  "      <path",
  "         d=\"M0,0 L0,8 L11,4 z\"",
  "         fill=\"#374151\"",
  "         id=\"path8\" />",
  "    </marker>",
  "    <marker",
  "       id=\"arrowDash\"",
  "       markerWidth=\"12\"",
  "       markerHeight=\"12\"",
  "       refX=\"10\"",
  "       refY=\"4\"",
  "       orient=\"auto\">",
  "      <path",
  "         d=\"M0,0 L0,8 L11,4 z\"",
  "         fill=\"#6b7280\"",
  "         id=\"path9\" />",
  "    </marker>",
  "    <style",
  "       id=\"style9\">",
  "      .t { font-family: Arial, &quot;Yu Gothic&quot;, &quot;Noto Sans JP&quot;, sans-serif; fill:#111827; }",
  "      .title { font-size:44px; font-weight:700; }",
  "      .h { font-size:28px; font-weight:700; }",
  "      .m { font-size:24px; }",
  "      .s { font-size:21px; }",
  "      .xs { font-size:18px; }",
  "      .comp { fill:url(#blueFill); stroke:#2684ff; stroke-width:2.8; rx:14; }",
  "      .concept { fill:url(#purpleFill); stroke:#8b5cf6; stroke-width:2.8; stroke-dasharray:10 8; rx:14; }",
  "      .process { fill:url(#amberFill); stroke:#d18a00; stroke-width:2.8; }",
  "      .out { fill:url(#greenFill); stroke:#159447; stroke-width:2.8; }",
  "      .note { fill:#ffffff; stroke:#4b5563; stroke-width:2.2; rx:12; }",
  "      .solid { stroke:#374151; stroke-width:2.8; fill:none; marker-end:url(#arrowSolid); }",
  "      .dash { stroke:#6b7280; stroke-width:2.8; fill:none; stroke-dasharray:10 7; marker-end:url(#arrowDash); }",
  "    </style>",
  "    <linearGradient",
  "       xlink:href=\"#purpleFill\"",
  "       id=\"linearGradient75\"",
  "       x1=\"25.132067\"",
  "       y1=\"165.24546\"",
  "       x2=\"25.132067\"",
  "       y2=\"402.95293\"",
  "       gradientTransform=\"matrix(1.0815141,0,0,0.74917333,11.75429,-0.33954377)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#amberFill\"",
  "       id=\"linearGradient76\"",
  "       x1=\"2.2412561\"",
  "       y1=\"721.06479\"",
  "       x2=\"2.2412561\"",
  "       y2=\"989.83095\"",
  "       gradientTransform=\"scale(1.5553598,0.64293807)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#amberFill\"",
  "       id=\"linearGradient77\"",
  "       x1=\"793.48136\"",
  "       y1=\"655.49613\"",
  "       x2=\"793.48136\"",
  "       y2=\"1026.8318\"",
  "       gradientTransform=\"scale(1.529389,0.65385588)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#greenFill\"",
  "       id=\"linearGradient79\"",
  "       x1=\"250.01313\"",
  "       y1=\"1245.0477\"",
  "       x2=\"250.01313\"",
  "       y2=\"1475.1372\"",
  "       gradientTransform=\"scale(1.7326019,0.57716664)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#greenFill\"",
  "       id=\"linearGradient80\"",
  "       x1=\"1.8430741\"",
  "       y1=\"1237.2152\"",
  "       x2=\"1.8430741\"",
  "       y2=\"1465.8573\"",
  "       gradientTransform=\"scale(1.7217023,0.58082052)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#blueFill\"",
  "       id=\"linearGradient81\"",
  "       x1=\"598.26299\"",
  "       y1=\"478.84788\"",
  "       x2=\"598.26299\"",
  "       y2=\"696.19055\"",
  "       gradientTransform=\"matrix(1.5772328,0,0,0.63402182,0,12)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#purpleFill\"",
  "       id=\"linearGradient82\"",
  "       x1=\"402.11637\"",
  "       y1=\"1449.0804\"",
  "       x2=\"402.11637\"",
  "       y2=\"1524.9134\"",
  "       gradientTransform=\"matrix(1.2896764,0,0,0.77538829,-28,0)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#amberFill\"",
  "       id=\"linearGradient83\"",
  "       x1=\"713.48141\"",
  "       y1=\"1658.9268\"",
  "       x2=\"713.48141\"",
  "       y2=\"1745.7414\"",
  "       gradientTransform=\"scale(1.476439,0.67730534)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#greenFill\"",
  "       id=\"linearGradient84\"",
  "       x1=\"1042.1433\"",
  "       y1=\"1547.4197\"",
  "       x2=\"1042.1433\"",
  "       y2=\"1628.399\"",
  "       gradientTransform=\"scale(1.377198,0.72611199)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#blueFill\"",
  "       id=\"linearGradient85\"",
  "       x1=\"49.314695\"",
  "       y1=\"1449.0804\"",
  "       x2=\"49.314695\"",
  "       y2=\"1524.9134\"",
  "       gradientTransform=\"matrix(1.2896764,0,0,0.77538829,22,0)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#amberFill\"",
  "       id=\"linearGradient86\"",
  "       x1=\"323.24175\"",
  "       y1=\"636.30264\"",
  "       x2=\"323.24175\"",
  "       y2=\"923.95796\"",
  "       gradientTransform=\"scale(1.4184187,0.70501043)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#greenFill\"",
  "       id=\"linearGradient87\"",
  "       x1=\"618.93968\"",
  "       y1=\"1420.1259\"",
  "       x2=\"618.93968\"",
  "       y2=\"1682.5706\"",
  "       gradientTransform=\"scale(1.9762398,0.50601147)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#greenFill\"",
  "       id=\"linearGradient2\"",
  "       gradientUnits=\"userSpaceOnUse\"",
  "       gradientTransform=\"scale(1.9762398,0.50601147)\"",
  "       x1=\"618.93968\"",
  "       y1=\"1420.1259\"",
  "       x2=\"618.93968\"",
  "       y2=\"1682.5706\" />",
  "    <linearGradient",
  "       xlink:href=\"#purpleFill\"",
  "       id=\"linearGradient13\"",
  "       x1=\"1077.5879\"",
  "       y1=\"195.62577\"",
  "       x2=\"1077.5879\"",
  "       y2=\"456.02132\"",
  "       gradientTransform=\"matrix(1.3156654,0,0,0.87539278,0.95828005,-52.686832)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "  </defs>",
  "  <text",
  "     class=\"t title\"",
  "     x=\"900\"",
  "     y=\"62\"",
  "     text-anchor=\"middle\"",
  "     id=\"text9\">Virtual CYP-PK Lab：シミュレーションモデルの模式図</text>",
  "  <!-- absorption depot -->",
  "  <rect",
  "     class=\"concept\"",
  "     x=\"40.067501\"",
  "     y=\"124.86019\"",
  "     width=\"254.81891\"",
  "     height=\"175.27962\"",
  "     id=\"rect10\"",
  "     style=\"fill:url(#linearGradient75)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"170\"",
  "     y=\"168\"",
  "     text-anchor=\"middle\"",
  "     id=\"text10\">腹腔内投与後の</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"170\"",
  "     y=\"205\"",
  "     text-anchor=\"middle\"",
  "     id=\"text11\">吸収デポ</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"170\"",
  "     y=\"246\"",
  "     text-anchor=\"middle\"",
  "     id=\"text12\">Adep</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"170\"",
  "     y=\"282\"",
  "     text-anchor=\"middle\"",
  "     id=\"text13\">一次吸収 k<tspan",
  "   baseline-shift=\"sub\"",
  "   font-size=\"16px\"",
  "   id=\"tspan12\">a</tspan></text>",
  "  <!-- central -->",
  "  <rect",
  "     class=\"comp\"",
  "     x=\"470\"",
  "     y=\"125\"",
  "     width=\"345\"",
  "     height=\"185\"",
  "     id=\"rect13\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"642\"",
  "     y=\"166\"",
  "     text-anchor=\"middle\"",
  "     id=\"text14\">中心（血漿）</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"642\"",
  "     y=\"203\"",
  "     text-anchor=\"middle\"",
  "     id=\"text15\">コンパートメント</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"642\"",
  "     y=\"248\"",
  "     text-anchor=\"middle\"",
  "     id=\"text16\">X1</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"642\"",
  "     y=\"286\"",
  "     text-anchor=\"middle\"",
  "     id=\"text17\">Cplasma = X1 / Vp</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"297.30515\"",
  "     y1=\"215\"",
  "     x2=\"463.63846\"",
  "     y2=\"215\"",
  "     id=\"line17\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"382.09402\"",
  "     y=\"186.20921\"",
  "     text-anchor=\"middle\"",
  "     id=\"text18\">F<tspan",
  "   baseline-shift=\"sub\"",
  "   font-size=\"16px\"",
  "   id=\"tspan17\">eff</tspan> · k<tspan",
  "   baseline-shift=\"sub\"",
  "   font-size=\"16px\"",
  "   id=\"tspan18\">a</tspan> · Adep</text>",
  "  <!-- peripheral -->",
  "  <rect",
  "     class=\"comp\"",
  "     x=\"945\"",
  "     y=\"90\"",
  "     width=\"340\"",
  "     height=\"135\"",
  "     id=\"rect18\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"140\"",
  "     text-anchor=\"middle\"",
  "     id=\"text19\">浅い末梢</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"177\"",
  "     text-anchor=\"middle\"",
  "     id=\"text20\">コンパートメント</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1115\"",
  "     y=\"210\"",
  "     text-anchor=\"middle\"",
  "     id=\"text21\">X2</text>",
  "  <rect",
  "     class=\"comp\"",
  "     x=\"945\"",
  "     y=\"317\"",
  "     width=\"340\"",
  "     height=\"135\"",
  "     id=\"rect21\"",
  "     style=\"fill:url(#linearGradient81)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"367\"",
  "     text-anchor=\"middle\"",
  "     id=\"text22\">深い末梢</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"404\"",
  "     text-anchor=\"middle\"",
  "     id=\"text23\">コンパートメント</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1115\"",
  "     y=\"437\"",
  "     text-anchor=\"middle\"",
  "     id=\"text24\">X3</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"815\"",
  "     y1=\"170\"",
  "     x2=\"942\"",
  "     y2=\"142\"",
  "     id=\"line24\" />",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"942\"",
  "     y1=\"178\"",
  "     x2=\"817\"",
  "     y2=\"208\"",
  "     id=\"line25\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"849\"",
  "     y=\"142\"",
  "     id=\"text25\">k12</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"878\"",
  "     y=\"221\"",
  "     id=\"text26\">k21</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"815\"",
  "     y1=\"288\"",
  "     x2=\"942\"",
  "     y2=\"362\"",
  "     id=\"line26\" />",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"931.40472\"",
  "     y1=\"390.09753\"",
  "     x2=\"816.61407\"",
  "     y2=\"319.54483\"",
  "     id=\"line27\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"875\"",
  "     y=\"309\"",
  "     id=\"text27\">k13</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"847.39539\"",
  "     y=\"384\"",
  "     id=\"text28\">k31</text>",
  "  <!-- brain/effect-site -->",
  "  <rect",
  "     class=\"concept\"",
  "     x=\"1420.1024\"",
  "     y=\"120.17594\"",
  "     width=\"339.79507\"",
  "     height=\"224.7216\"",
  "     id=\"rect28\"",
  "     style=\"fill:url(#linearGradient13)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1590\"",
  "     y=\"157\"",
  "     text-anchor=\"middle\"",
  "     id=\"text29\">脳 effect-site</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1590\"",
  "     y=\"194\"",
  "     text-anchor=\"middle\"",
  "     id=\"text30\">コンパートメント</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1590\"",
  "     y=\"225\"",
  "     text-anchor=\"middle\"",
  "     id=\"text31\"><tspan",
  "       id=\"tspan11\"",
  "       x=\"1590\"",
  "       y=\"225\">Xe</tspan><tspan",
  "       id=\"tspan13\"",
  "       x=\"1590\"",
  "       y=\"255\">Ce = Xe / Ve</tspan></text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1590\"",
  "     y=\"295\"",
  "     text-anchor=\"middle\"",
  "     id=\"text32\"><tspan",
  "       id=\"tspan1\"",
  "       x=\"1590\"",
  "       y=\"295\">血中濃度の変化に応じて遅れて</tspan><tspan",
  "       id=\"tspan4\"",
  "       x=\"1590\"",
  "       y=\"323.43671\">変化する仮想的な脳内作用部位</tspan></text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"1590\"",
  "     y=\"322\"",
  "     text-anchor=\"middle\"",
  "     id=\"text33\"",
  "     style=\"font-size:21px\" />",
  "  <!-- horizontal conceptual arrow to brain -->",
  "  <line",
  "     class=\"dash\"",
  "     x1=\"815\"",
  "     y1=\"272\"",
  "     x2=\"1417\"",
  "     y2=\"272\"",
  "     id=\"line33\" />",
  "  <rect",
  "     x=\"890\"",
  "     y=\"244\"",
  "     width=\"452\"",
  "     height=\"62\"",
  "     rx=\"8\"",
  "     fill=\"#ffffff\"",
  "     opacity=\"0.96\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1116\"",
  "     y=\"266\"",
  "     text-anchor=\"middle\"",
  "     style=\"fill:#111827;font-size:21px\">血中濃度に応じた脳 / effect-site 濃度変化</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1116\"",
  "     y=\"292\"",
  "     text-anchor=\"middle\"",
  "     style=\"fill:#111827;font-size:21px\">（Qe, Kp,e）</text>",
  "  <!-- intervention specific PK changes -->",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"420,550 385,635 40,635 5,550 40,465 385,465 \"",
  "     id=\"polygon35\"",
  "     style=\"fill:url(#linearGradient76)\"",
  "     transform=\"matrix(0.95891021,0,0,1.1309677,17.196078,-71.217479)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"212\"",
  "     y=\"503\"",
  "     text-anchor=\"middle\"",
  "     id=\"text36\">処置特異的な代謝PK変化</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"540\"",
  "     text-anchor=\"middle\"",
  "     id=\"text37\">Control：Vmax ×1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"568\"",
  "     text-anchor=\"middle\"",
  "     id=\"text38\">　　　　Km ×1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"596\"",
  "     text-anchor=\"middle\"",
  "     id=\"text39\">PHB：Vmax ×2.011 / Km ×1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"624\"",
  "     text-anchor=\"middle\"",
  "     id=\"text40\">OME：Vmax ×1.000 / Km ×3.456</text>",
  "  <!-- MM elimination -->",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"500,650 460,550 500,450 825,450 865,550 825,650 \"",
  "     id=\"polygon40\"",
  "     style=\"fill:url(#linearGradient86)\"",
  "     transform=\"translate(32)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"694\"",
  "     y=\"500\"",
  "     text-anchor=\"middle\"",
  "     id=\"text41\">Michaelis–Menten 消失</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"694\"",
  "     y=\"548\"",
  "     text-anchor=\"middle\"",
  "     id=\"text42\">rate = Vmax · X1 / (Km + X1)</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"694\"",
  "     y=\"594\"",
  "     text-anchor=\"middle\"",
  "     id=\"text43\">中央compartmentからの</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"694\"",
  "     y=\"624\"",
  "     text-anchor=\"middle\"",
  "     id=\"text44\">代謝消失</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"642\"",
  "     y1=\"310\"",
  "     x2=\"642\"",
  "     y2=\"447\"",
  "     id=\"line44\" />",
  "  <line",
  "     class=\"dash\"",
  "     x1=\"420.53787\"",
  "     y1=\"550\"",
  "     x2=\"486.89807\"",
  "     y2=\"550\"",
  "     id=\"line45\" />",
  "  <!-- LRR decision -->",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"1250,670 1215,550 1250,430 1745,430 1780,550 1745,670 \"",
  "     id=\"polygon45\"",
  "     style=\"fill:url(#linearGradient77)\"",
  "     transform=\"matrix(0.78856273,0,0,1.0013262,347.8453,-1.5844639)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1528\"",
  "     y=\"468\"",
  "     text-anchor=\"middle\"",
  "     id=\"text45\">正向反射消失（LRR）の判定</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1528\"",
  "     y=\"507\"",
  "     text-anchor=\"middle\"",
  "     id=\"text46\">Ce ≥ C<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan6\">RR,thr</tspan> × M<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan8\">RR</tspan></text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1528\"",
  "     y=\"540\"",
  "     text-anchor=\"middle\"",
  "     id=\"text47\">Control：M<tspan",
  "   style=\"font-size:16px\"",
  "   id=\"tspan9\">RR</tspan> = 1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1528\"",
  "     y=\"572\"",
  "     text-anchor=\"middle\"",
  "     id=\"text48\">PHB：1.070</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1528\"",
  "     y=\"604\"",
  "     text-anchor=\"middle\"",
  "     id=\"text49\">OME：1.036</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"1528\"",
  "     y=\"630\"",
  "     text-anchor=\"middle\"",
  "     id=\"text50\"",
  "     style=\"font-size:21px\">PK fit後に行動差を再現するための</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"1528\"",
  "     y=\"654\"",
  "     text-anchor=\"middle\"",
  "     id=\"text51\"",
  "     style=\"font-size:21px\">残差的な閾値調整</text>",
  "  <line",
  "     class=\"dash\"",
  "     x1=\"1590\"",
  "     y1=\"345\"",
  "     x2=\"1590\"",
  "     y2=\"427\"",
  "     id=\"line51\" />",
  "  <!-- outputs -->",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"435,850 470,720 830,720 795,850 \"",
  "     id=\"polygon51\"",
  "     style=\"fill:url(#linearGradient79)\"",
  "     transform=\"translate(62)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"694\"",
  "     y=\"770\"",
  "     text-anchor=\"middle\"",
  "     id=\"text52\">血中 PTB 濃度</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"694\"",
  "     y=\"815\"",
  "     text-anchor=\"middle\"",
  "     id=\"text53\">採血時点のCplasma</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"662\"",
  "     y=\"845\"",
  "     text-anchor=\"middle\"",
  "     id=\"text54\" />",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"694\"",
  "     y1=\"650\"",
  "     x2=\"694\"",
  "     y2=\"717\"",
  "     id=\"line54\" />",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"5,850 40,720 395,720 360,850 \"",
  "     id=\"polygon54\"",
  "     style=\"fill:url(#linearGradient80)\"",
  "     transform=\"matrix(0.99915962,0,0,1.2167289,24.168075,-155.92388)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"224\"",
  "     y=\"763\"",
  "     text-anchor=\"middle\"",
  "     id=\"text55\">終末肝 virtual ex vivo</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"224\"",
  "     y=\"800\"",
  "     text-anchor=\"middle\"",
  "     id=\"text56\">PTB 代謝活性</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"224\"",
  "     y=\"834\"",
  "     text-anchor=\"middle\"",
  "     id=\"text57\"",
  "     style=\"font-size:21px\">処置別の Vmax と Km を</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"224\"",
  "     y=\"858\"",
  "     text-anchor=\"middle\"",
  "     id=\"text58\"",
  "     style=\"font-size:21px\">そのまま反映して算出</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"224\"",
  "     y1=\"646.54309\"",
  "     x2=\"224\"",
  "     y2=\"717.38116\"",
  "     id=\"line58\" />",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"1740,720 1705,850 1225,850 1260,720 \"",
  "     id=\"polygon58\"",
  "     style=\"fill:url(#linearGradient87)\"",
  "     transform=\"matrix(0.92750309,0,0,1.0009007,126.21948,-0.70705017)\" />",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"1225,850 1260,720 1740,720 1705,850 \"",
  "     id=\"polygon58-7\"",
  "     style=\"fill:url(#linearGradient2);stroke:#159447;stroke-width:2.8\"",
  "     transform=\"matrix(1.0881286,0,0,1.2568222,-122.11762,-3.0610802)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1500\"",
  "     y=\"764\"",
  "     text-anchor=\"middle\"",
  "     id=\"text59\">脳関連アウトプット</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1500\"",
  "     y=\"798\"",
  "     text-anchor=\"middle\"",
  "     id=\"text60\">LRR持続時間</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1500\"",
  "     y=\"828\"",
  "     text-anchor=\"middle\"",
  "     id=\"text61\">＋ 終末脳内 PTB 濃度</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"1498\"",
  "     y1=\"670\"",
  "     x2=\"1498\"",
  "     y2=\"717\"",
  "     id=\"line61\" />",
  "  <!-- notes row -->",
  "  <rect",
  "     class=\"note\"",
  "     x=\"34.720989\"",
  "     y=\"899.72095\"",
  "     width=\"522.92725\"",
  "     height=\"168.51393\"",
  "     id=\"rect61\" />",
  "  <rect",
  "     class=\"note\"",
  "     x=\"594.84137\"",
  "     y=\"900.37286\"",
  "     width=\"598.32819\"",
  "     height=\"168.3967\"",
  "     id=\"rect61-9\"",
  "     style=\"fill:#ffffff;stroke:#4b5563;stroke-width:1.80899\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"639.78076\"",
  "     y=\"965.98291\"",
  "     id=\"text63-2\"",
  "     style=\"font-size:21px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\"><tspan",
  "       id=\"tspan2\"",
  "       x=\"639.78076\"",
  "       y=\"965.98291\">個体間変動（log-normal）：</tspan><tspan",
  "       x=\"639.78076\"",
  "       y=\"994.41962\"",
  "       id=\"tspan3\">BW 5%、F<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan14\">eff</tspan> 10%、ka 10%、Vmax 15% CV</tspan><tspan",
  "       x=\"639.78076\"",
  "       y=\"1022.8563\"",
  "       id=\"tspan5\">測定誤差（log-normal）：</tspan><tspan",
  "       x=\"639.78076\"",
  "       y=\"1051.293\"",
  "       id=\"tspan7\">血中濃度 6%、脳内濃度 6%、肝virtual assay 10% CV</tspan></text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"638.1463\"",
  "     y=\"933.70123\"",
  "     id=\"text62-0\"",
  "     style=\"font-weight:700;font-size:28px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\">個体差・測定誤差</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"62\"",
  "     y=\"943\"",
  "     id=\"text62\">群間で共通の基本構造・parameter</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"62\"",
  "     y=\"986\"",
  "     id=\"text63\">F_eff, ka, k12, k21, k13, k31, Vp, Qe, Kp,e, Ve と</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"62\"",
  "     y=\"1018\"",
  "     id=\"text64\">基準 Vmax / Km / C<tspan",
  "   style=\"font-size:16px\"",
  "   id=\"tspan10\">RR,thr</tspan>。</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"62\"",
  "     y=\"1050\"",
  "     id=\"text65\"",
  "     style=\"font-size:21px\">処置差は上記の相対変化としてモデルに反映。</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1273.1837\"",
  "     y=\"942.94476\"",
  "     id=\"text66-3\"",
  "     style=\"font-weight:700;font-size:28px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\">肝 virtual ex vivo assay 条件</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1253.1837\"",
  "     y=\"984.9447\"",
  "     id=\"text67-4\"",
  "     style=\"font-size:21px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\">一定基質濃度 Cstd = 56.6 µg/mL（250 µM）を使用</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1253.1837\"",
  "     y=\"1017.9448\"",
  "     id=\"text68-5\"",
  "     style=\"font-size:21px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\">各群に設定された Vmax と Km を用いて</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1253.1837\"",
  "     y=\"1047.9447\"",
  "     id=\"text69-1\"",
  "     style=\"font-size:21px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\">同じ基質条件下の代謝速度を算出</text>",
  "  <!-- legend -->",
  "  <rect",
  "     class=\"note\"",
  "     x=\"34.911285\"",
  "     y=\"1089.9113\"",
  "     width=\"1725.1775\"",
  "     height=\"130.99315\"",
  "     id=\"rect69\" />",
  "  <rect",
  "     class=\"comp\"",
  "     x=\"87\"",
  "     y=\"1125\"",
  "     width=\"95\"",
  "     height=\"56\"",
  "     id=\"rect70\"",
  "     style=\"fill:url(#linearGradient85)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"207\"",
  "     y=\"1146\"",
  "     id=\"text70\">実コンパートメント</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"207\"",
  "     y=\"1175\"",
  "     id=\"text71\">全身の質量収支に含む</text>",
  "  <rect",
  "     class=\"concept\"",
  "     x=\"492\"",
  "     y=\"1125\"",
  "     width=\"95\"",
  "     height=\"56\"",
  "     id=\"rect71\"",
  "     style=\"fill:url(#linearGradient82)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"612\"",
  "     y=\"1146\"",
  "     id=\"text72\">概念的 / driven compartment</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"612\"",
  "     y=\"1175\"",
  "     id=\"text73\">全身の質量収支には含めない</text>",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"1055,1153 1070,1125 1165,1125 1180,1153 1165,1181 1070,1181 \"",
  "     id=\"polygon73\"",
  "     style=\"fill:url(#linearGradient83)\"",
  "     transform=\"translate(-62)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1143\"",
  "     y=\"1162\"",
  "     id=\"text74\">過程・判定ルール</text>",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"1545,1125 1532,1181 1437,1181 1450,1125 \"",
  "     id=\"polygon74\"",
  "     style=\"fill:url(#linearGradient84)\"",
  "     transform=\"translate(-62)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1503\"",
  "     y=\"1162\"",
  "     id=\"text75\">観測 / 導出アウトプット</text>",
  "</svg>"
), collapse = "\n")




ui <- navbarPage(
  title = "Virtual CYP–PK Lab",
  id = "main_tab",
  header = tags$head(
    tags$style(HTML("\n      body { background-color: #f7f9fb; }\n      .navbar-brand { font-weight: 700; }
      .app-subtitle { font-size: 18px; font-weight: 700; color: #17365d; margin-bottom: 6px; }
      .app-lead { font-size: 15px; line-height: 1.65; }
      .reference-box { background: #eef5ff; border-left: 5px solid #337ab7; padding: 12px 14px; margin-bottom: 12px; }\n      .clock-card {\n        background: #17365d; color: white; border-radius: 8px;\n        padding: 14px 18px; margin-bottom: 14px;\n      }\n      .clock-value { font-size: 30px; font-weight: 700; line-height: 1.1; }\n      .panel-title-strong { font-weight: 700; font-size: 18px; margin-bottom: 10px; }\n      .action-box { background: white; border: 1px solid #dde3ea; border-radius: 8px; padding: 14px; margin-bottom: 12px; }\n      .student-result { background: #eef5ff; border-left: 5px solid #337ab7; padding: 12px; min-height: 54px; }\n      .note-box { background: #fff8dc; border-left: 5px solid #d9a300; padding: 10px 12px; margin-bottom: 12px; }\n      .debrief-box { background: #eef8ef; border-left: 5px solid #5a9f59; padding: 10px 12px; margin-bottom: 12px; }\n      .rat-radio-grid .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 4px 10px;
        margin-bottom: 8px;
      }
      .rat-radio-grid .radio { margin-top: 0; margin-bottom: 0; }
      .rat-radio-grid label { white-space: nowrap; }
      table { background: white; }\n    "))
  ),

  tabPanel(
    "1. 実験設定",
    fluidPage(
      br(),
      div(class = "app-subtitle", "肝ミクロソーム酸化的薬物代謝の誘導・阻害がペントバルビタールPKと麻酔時間に及ぼす影響を学ぶ仮想薬理実習"),
      div(class = "reference-box app-lead",
          strong("この実習で行うこと"),
          p("この仮想実習では、ペントバルビタールをラットに腹腔内投与し、肝ミクロソーム酸化的薬物代謝の誘導・阻害がペントバルビタールPKと麻酔時間に及ぼす影響を、血中濃度と正向反射から比較します。"),
          p(HTML("<b>正向反射</b>とは、仰向けにした動物が自力で正常な姿勢に戻る反射です。本実習では、正向反射の消失を麻酔作用の指標として扱います。")),
          p("実験群は Control、フェノバルビタール前処置（PHB）、オメプラゾール前処置（OME）の3群です。"),
          p("組織採取を行った場合は、脳内ペントバルビタール濃度と、摘出肝を用いた実験を模した virtual ex vivo ペントバルビタール代謝assay を確認できます。")),
      experiment_visual_guide_ja(),
      div(class = "note-box app-lead",
          strong("実験の進め方の目安"),
          p("各群で3匹ずつラットを割り付け、ペントバルビタールを投与した後、正向反射を経時的に確認します。血中濃度は投与後15、30、60、120分を目安に「正向反射確認＋採血」で測定してください。必要に応じて追加測定しても構いません。"),
          p("実験ログには、各操作の実際の投与後時刻が記録されます。群間比較では、できるだけ近い投与後時刻のデータを見比べてください。"),
          p("組織採取後はそのラットに追加操作できません。脳と肝臓の情報が必要な場合は、採血や正向反射確認の計画を立てたうえで最後に実施してください。組織採取では仮想時間を進めません。"),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(tags$th("操作"), tags$th("進む仮想時間"))),
            tags$tbody(
              tags$tr(tags$td("ペントバルビタール投与"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$dose))),
              tags$tr(tags$td("正向反射の確認"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$rr_check))),
              tags$tr(tags$td("正向反射確認＋採血"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$rr_blood))),
              tags$tr(tags$td("組織採取（肝臓＋脳；肝virtual ex vivo assayを含む）"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$terminal_tissues))),
              tags$tr(tags$td("待機"), tags$td("入力した待機時間だけ進む"))
            )
          )),
      div(class = "reference-box app-lead",
          strong("参考となる前処置条件"),
          tags$ul(
            tags$li(HTML("<b>Control：</b> 酸化的薬物代謝を誘導・阻害する前処置なし。")),
            tags$li(HTML("<b>フェノバルビタール / PHB：</b> Means et al. (1978)。PHB 10 mg/kg i.p.を投与し、24時間後にペントバルビタール 50 mg/kg i.p.を投与した条件を主要な根拠としています。PMID: 634995")),
            tags$li(HTML("<b>オメプラゾール / OME：</b> Henry et al. (1986)。omeprazole 40 mg/kg i.v.をペントバルビタール投与30分前に投与し、ペントバルビタール 45 mg/kg i.p.を投与した条件を主要な根拠としています。PMID: 3742882"))
          ),
          p(HTML("<b>注意：</b> OME 40 mg/kg i.v.は、薬物相互作用を評価するために用いられた高用量のラット実験条件です。臨床用量を再現したものではありません。")),
          p("上記はモデル化の根拠となった文献条件です。実際の模擬実験では、画面で設定したペントバルビタール投与量を全3群に共通して使用します。")),
      fluidRow(
        column(
          8,
          div(class = "note-box",
              strong("ラットの割付方法："),
              tags$ol(
                tags$li("Control、フェノバルビタール（PHB）、オメプラゾール（OME）へ各3匹を選びます。"),
                tags$li("同じRatを2群以上に入れることはできません。"),
                tags$li("迷う場合は「9匹をランダム割付」を押しても構いません。"),
                tags$li("割付チェックが緑色になったら「この割付で実験を開始」を押します。")
              )),
          actionButton("random_allocate", "9匹をランダム割付", class = "btn-default"),
          br(), br(),
          fluidRow(
            column(4, selectizeInput("control_rats", "Control（3匹）",
                                     choices = rat_bank$RatID, selected = character(0),
                                     multiple = TRUE, options = list(maxItems = 3))),
            column(4, selectizeInput("phb_rats", "フェノバルビタール / PHB（3匹）",
                                     choices = rat_bank$RatID, selected = character(0),
                                     multiple = TRUE, options = list(maxItems = 3))),
            column(4, selectizeInput("ome_rats", "オメプラゾール / OME（3匹）",
                                     choices = rat_bank$RatID, selected = character(0),
                                     multiple = TRUE, options = list(maxItems = 3)))
          ),
          fluidRow(
            column(6, numericInput("dose_mgkg", "ペントバルビタール投与量 (mg/kg, i.p.)", value = 50, min = 20, max = 60, step = 1)),
            column(6, br(), actionButton("start_experiment", "この割付で実験を開始", class = "btn-primary", width = "100%"))
          ),
          br(),
          uiOutput("setup_validation"),
          div(class = "panel-title-strong", "30匹の一覧（実験前に見える情報）"),
          tableOutput("allocation_table")
        ),
        column(
          4,
          div(class = "action-box",
              div(class = "panel-title-strong", "保存した実験を再現"),
              p("以前保存した1個のsessionファイル（.rds）をアップロードすると、同じラット・割付・操作ログ・測定結果を復元し、そのままモデル内部情報の開示画面を開きます。"),
              fileInput("session_file", "Session replay file (.rds)", accept = c(".rds"))),
          div(class = "action-box",
              div(class = "panel-title-strong", "仮想ラット集団"),
              p("30匹は固定された個体で、各個体は異なる薬物動態を持ちます。"),
              p("実験中は体重以外の個体パラメータを隠します。"),
              p("実験中に隠されている個体パラメータは、実験終了後のモデル内部情報の開示画面で意味とともに説明します。")),
        )
      )
    )
  ),

  tabPanel(
    "2. 実験操作",
    fluidPage(
      br(),
      uiOutput("experiment_gate"),
      conditionalPanel(
        condition = "output.experimentStarted == true",
        fluidRow(
          column(
            3,
            div(class = "clock-card",
                div("Virtual experimental time"),
                div(class = "clock-value", textOutput("clock_text", inline = TRUE)),
                div("min")),
            div(class = "action-box",
                div(class = "rat-radio-grid",
                    radioButtons("active_rat", "操作するラット（9匹を一覧表示）",
                                 choices = character(0), selected = character(0))),
                actionButton("dose_btn", sprintf("ペントバルビタール投与（%.1f分）", EXPERIMENT_ACTION_MINUTES$dose), width = "100%"), br(), br(),
                actionButton("lrr_btn", sprintf("正向反射を確認（%.1f分）", EXPERIMENT_ACTION_MINUTES$rr_check), width = "100%"), br(), br(),
                actionButton("lrr_blood_btn", sprintf("正向反射確認＋採血（%.1f分）", EXPERIMENT_ACTION_MINUTES$rr_blood), width = "100%"),
                tags$small("※ 採血時点では正向反射も同じ操作時点で記録します。"), br(), br(),
                div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:8px;",
                    actionButton("wait_05_btn", "+0.5分", class = "btn-default"),
                    actionButton("wait_1_btn", "+1分", class = "btn-default"),
                    actionButton("wait_5_btn", "+5分", class = "btn-default"),
                    actionButton("wait_10_btn", "+10分", class = "btn-default")),
                numericInput("wait_min", "任意の待機時間 (min)", value = 5, min = 0, step = 0.5),
                actionButton("wait_btn", "指定時間だけ待機", width = "100%"),
                tags$hr(),
                actionButton(
                  "tissue_btn",
                  HTML(sprintf("組織採取（肝臓＋脳）<br>（%.1f分）", EXPERIMENT_ACTION_MINUTES$terminal_tissues)),
                  width = "100%", class = "btn-warning",
                  style = "white-space:normal;height:auto;min-height:48px;line-height:1.25;padding:7px 10px;"
                ),
                br(), br(),
                actionButton(
                  "finish_experiment_btn",
                  HTML("実験を終了して<br>モデル内部情報を開示"),
                  width = "100%", class = "btn-success",
                  style = "white-space:normal;height:auto;min-height:48px;line-height:1.25;padding:7px 10px;"
                ),
                tags$small("※ 押すとデータ取得を終了し、以後の実験操作はできません。モデル内部情報の開示画面へ移動します。")),
            div(class = "student-result", uiOutput("action_result"))
          ),
          column(
            9,
            div(class = "panel-title-strong", "現在のラット状態"),
            tableOutput("status_table"),
            tags$hr(),
            div(class = "panel-title-strong", "直近の採血結果"),
            tableOutput("blood_table"),
            tags$hr(),
            div(class = "panel-title-strong", "正向反射確認履歴"),
            tableOutput("lrr_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "3. 実験ログ",
    fluidPage(
      br(),
      uiOutput("log_gate"),
      tableOutput("event_log_table"),
      br(),
      downloadButton("download_logs", "測定データ・実験ログをCSV ZIPで保存"),
      tags$span("　"),
      downloadButton("download_session", "再現用Sessionファイルを保存")
    )
  ),

  tabPanel(
    "4. モデル内部情報の開示",
    fluidPage(
      br(),
      uiOutput("debrief_gate"),
      conditionalPanel(
        condition = "output.experimentStarted == true",
        conditionalPanel(
          condition = "output.debriefRevealed == false",
          div(class = "note-box",
              "実験操作タブの「実験を終了してモデル内部情報を開示」を押すと、実験中には見えなかったモデル内部情報が表示されます。")
        ),
        conditionalPanel(
          condition = "output.debriefRevealed == true",
          div(class = "debrief-box",
              strong("モデル内部情報の開示（実験結果ではありません）："),
              "ここに表示するのは、模擬実験実施者が取得した測定結果そのものではなく、シミュレーション内部で設定・計算されていた個体パラメータ、真の濃度推移、真の正向反射変化時刻です。自分たちの実験ログ・測定値と照合して解釈してください。"),
          div(class = "reference-box",
              strong("処置群で用いたモデル上の相対変化："),
              p(sprintf("PHB：Vmax ×%.3f、Km ×1.000、M_RR ×%.3f。OME：Vmax ×1.000、Km ×%.3f、M_RR ×%.3f。",
                        MODEL_DEFAULTS$treatment$PHB$vmax_mult,
                        MODEL_DEFAULTS$treatment$PHB$M_RR,
                        MODEL_DEFAULTS$treatment$OME$km_mult,
                        MODEL_DEFAULTS$treatment$OME$M_RR)),
              p("Vmax/Kmの相対変化は、文献で観察されたペントバルビタールPK変化を固定構造モデル上で再現するためのoperational translationです。M_RRはPK fitの後に行動差を再現するために求めた残差的な閾値倍率であり、単独では純粋なPD parameterとして解釈できません。")),
          div(class = "note-box",
              strong("用語："),
              "PK = Pharmacokinetics（薬物動態）、PD = Pharmacodynamics（薬力学）、",
              "正向反射消失・回復時刻、",
              "CYP = Cytochrome P450（シトクロムP450、薬物代謝酵素群）、",
              "IIV = Interindividual Variability（個体間変動）。"),
          fluidRow(
            column(5,
                   selectInput("debrief_rat", "確認するラット", choices = NULL),
                   tableOutput("latent_table"),
                   tableOutput("true_transition_table"),
                   downloadButton("download_debrief_plot", "このラットのモデル内部グラフをPNG保存")),
            column(7, plotOutput("trajectory_plot", height = "500px"))
          ),
          tags$hr(),
          div(class = "panel-title-strong", "群比較（モデル内部の真値；実測結果ではありません）"),
          plotOutput("group_comparison_plot", height = "720px"),
          tags$hr(),
          div(class = "panel-title-strong", "全使用個体のモデル内部情報"),
          tableOutput("all_latent_table"),
          downloadButton("download_debrief_summary", "全ラットのモデル内部情報をCSV保存"),
          tags$span("　"),
          downloadButton("download_debrief_all", "全モデル内部情報（表＋全グラフ）をZIP保存")
        )
      )
    )
  ),

  tabPanel(
    "5. 参考情報",
    fluidPage(
      br(),
      div(
        style = "width:100%; text-align:center; margin:0 auto 16px auto;",
        tags$div(
          HTML(MODEL_SCHEMATIC_SVG),
          style = "width:100%; max-width:1800px; margin:0 auto; overflow:hidden;"
        )
      ),
      div(class = "app-subtitle", "モデルと実験を理解するための参考情報"),

      div(class = "reference-box app-lead",
          strong("このアプリのモデル構造"),
          p("ペントバルビタールの全身薬物動態は、Hatanaka et al. (1988)を基礎とする3-compartment open modelに、腹腔内投与の一次吸収depotを追加して表現しています。中央compartmentからの消失にはMichaelis–Menten型の飽和性代謝を用います。"),
          div(style = "margin: 8px 0 10px 18px; padding: 8px 12px; background: #f8f9fa; border-left: 4px solid #6c757d; font-size: 15px;",
              HTML(
                "<b>腹腔内吸収：</b> dA<sub>dep</sub>/dt = −k<sub>a</sub>A<sub>dep</sub><br>",
                "<b>中央compartment：</b> dX<sub>1</sub>/dt = S<sub>IP</sub>k<sub>a</sub>A<sub>dep</sub> − (k<sub>12</sub>+k<sub>13</sub>)X<sub>1</sub> + k<sub>21</sub>X<sub>2</sub> + k<sub>31</sub>X<sub>3</sub> − V<sub>max</sub>X<sub>1</sub>/(K<sub>m</sub>+X<sub>1</sub>)<br>",
                "<b>末梢compartment：</b> dX<sub>2</sub>/dt = k<sub>12</sub>X<sub>1</sub> − k<sub>21</sub>X<sub>2</sub><br>",
                "<b>深部compartment：</b> dX<sub>3</sub>/dt = k<sub>13</sub>X<sub>1</sub> − k<sub>31</sub>X<sub>3</sub><br>",
                "<b>血中濃度：</b> C<sub>plasma</sub> = X<sub>1</sub>/V<sub>p</sub>"
              )),
          p("Adep（absorption depot）は、腹腔内投与後、まだ全身循環へ移行していない薬物量を表す吸収デポです。ここからkaに従って中心compartmentへ薬物が移行します。Adepは全身分布compartmentには数えないため、全身分布部分はX1–X3の3-compartment modelです。"),
          p("Hatanaka et al. (1988)では、ラットのペントバルビタール血漿濃度推移がMichaelis–Menten型消失を持つ3-compartment open modelで記述されています。本アプリではその構造・source parameterを基礎として、腹腔内投与と教育用の個体差を追加しています。")),

      div(class = "reference-box app-lead",
          strong("仮想『脳内濃度』（Ce）と正向反射"),
          p("脳関連の挙動は、血漿濃度の変化に遅れて追従するlatent effect-site state Xe を用いて表現します。Xe は全身のmass balanceには含めないため、X1–X3からなるsystemic three-compartment modelとは別の概念的状態量として扱います。"),
          div(style = "margin: 8px 0 10px 18px; padding: 8px 12px; background: #f8f9fa; border-left: 4px solid #6c757d; font-size: 15px;",
              HTML(
                "<b>潜在効果部位：</b> X<sub>e</sub> は、血漿濃度の変化に遅れて追従する脳関連のlatent effect-site状態量<br>",
                "<b>効果部位濃度：</b> C<sub>e</sub> は X<sub>e</sub> / V<sub>e</sub> として算出するlatent effect-site concentrationで、アプリ内の仮想『脳内濃度』として表示します。<br>",
                "<b>正向反射：</b> C<sub>e</sub> ≥ C<sub>RR,thr</sub> × M<sub>RR</sub> のときloss of righting reflex（LRR）/hypnosis stateとして扱う"
              )),
          p("基準C_RR,thrはreference conditionで共通に用い、処置群ではPK fit後のresidual threshold analysisで求めたM_RRにより実効閾値をC_RR,thr × M_RRとして表します。このアプリでは独立した動的PDモデルは追加せず、Ceと処置別の実効閾値の関係で正向反射を判定します。M_RRは単独では純粋なPD parameterとして解釈できません。")),

      div(class = "reference-box app-lead",
          strong("略語・記号・モデル用語の意味"),
          p("主な略語・記号を以下にまとめます。数値そのものを覚える必要はありません。"),
          p(HTML("<b>重要：</b> UIで『脳内濃度』と表示する値は、latent effect-site concentration（C<sub>e</sub>）です。Q<sub>e</sub>、V<sub>e</sub>、K<sub>p,e</sub> はlatent effect-siteを規定するparameterです。")),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(tags$th("表記"), tags$th("正式名称 / Full spelling（標準略語でない記号は意味）"), tags$th("このアプリでの意味"))),
            tags$tbody(
              tags$tr(tags$td("PK"), tags$td("Pharmacokinetics（薬物動態）"), tags$td("薬物が吸収・分布・代謝・消失して濃度が時間とともに変化する過程。")),
              tags$tr(tags$td("PD"), tags$td("Pharmacodynamics（薬力学）"), tags$td("薬物濃度と作用の関係。本アプリでは独立した動的PDモデルは置かず、仮想「脳内濃度」（Ce）と正向反射閾値で作用を判定します。")),
              tags$tr(tags$td("i.p."), tags$td("intraperitoneal（腹腔内投与）"), tags$td("薬液を腹腔内へ投与する投与経路。")),
              tags$tr(tags$td("BW"), tags$td("Body Weight（体重）"), tags$td("ラットの体重。モデルparameterの体格補正にも使用します。")),
              tags$tr(tags$td("CYP / P450"), tags$td("Cytochrome P450（シトクロムP450）"), tags$td("肝臓などで多くの薬物代謝に関与する酵素群。")),
              tags$tr(tags$td("PHB"), tags$td("フェノバルビタール（Phenobarbital）"), tags$td("本アプリでは肝薬物代謝能を誘導する前処置として扱います。")),
              tags$tr(tags$td("OME"), tags$td("オメプラゾール（Omeprazole）"), tags$td("高用量ラット条件で酸化的薬物代謝の阻害が報告されており、本アプリではHenry et al. (1986)のclearance低下をKm増加としてoperationalに表現します。")),
              tags$tr(tags$td("LRR"), tags$td("Loss of Righting Reflex（正向反射消失）"), tags$td("仰向けにした動物が自力で正常姿勢へ戻れない状態。本アプリでは麻酔作用の開始側の行動指標です。")),
              tags$tr(tags$td("RORR"), tags$td("Return of Righting Reflex（正向反射回復）"), tags$td("正向反射が再び認められる状態。本アプリでは麻酔作用の終了側の行動指標です。")),
              tags$tr(tags$td("Adep"), tags$td("Absorption depot（吸収デポ）"), tags$td("腹腔内投与後、まだ全身循環へ吸収されていないペントバルビタール量を表す状態。全身分布compartmentには数えません。")),
              tags$tr(tags$td("X1"), tags$td("Central compartment amount（中央compartment内薬物量）"), tags$td("主に血漿側を表す中央compartmentの薬物量。")),
              tags$tr(tags$td("X2"), tags$td("Shallow peripheral compartment amount（浅い末梢compartment内薬物量）"), tags$td("比較的速く中央compartmentと交換する末梢側の薬物量。")),
              tags$tr(tags$td("X3"), tags$td("Deep peripheral compartment amount（深部末梢compartment内薬物量）"), tags$td("よりゆっくり中央compartmentと交換する薬物量。")),
              tags$tr(tags$td("Xe"), tags$td("Latent effect-site state（潜在効果部位状態量）"), tags$td("血漿濃度に遅れて追従する脳関連のlatent effect-site状態量。全身X1–X3のmass balanceには含めません。")),
              tags$tr(tags$td("Ve"), tags$td("Effect-site distribution volume（効果部位の見かけ容積）"), tags$td("XeからCeを算出するための見かけ容積です。")),
              tags$tr(tags$td("F_eff"), tags$td("Effective systemic input fraction（有効全身入力率）"), tags$td("腹腔内デポから全身循環へ到達する有効入力を表す無次元のモデル係数。標準的なbioavailabilityを直接測定した値ではありません。")),
              tags$tr(tags$td("ka"), tags$td("First-order absorption rate constant（一次吸収速度定数）"), tags$td("腹腔内デポから薬物が吸収される速さを表します。値が大きいほど吸収が速くなります。")),
              tags$tr(tags$td("k12 / k21"), tags$td("Inter-compartmental rate constants（compartment間移行速度定数）"), tags$td("X1↔X2の薬物移行速度。k12は中央→末梢、k21は末梢→中央。")),
              tags$tr(tags$td("k13 / k31"), tags$td("Inter-compartmental rate constants（compartment間移行速度定数）"), tags$td("X1↔X3の薬物移行速度。k13は中央→深部、k31は深部→中央。")),
              tags$tr(tags$td("Vmax"), tags$td("Maximum metabolic rate（最大代謝速度）"), tags$td("Michaelis–Menten型代謝が飽和したときの最大代謝速度。本アプリではPHB処置によるPK変化をVmaxの相対増加として表現します。")),
              tags$tr(tags$td("Km"), tags$td("Michaelis constant（Michaelis定数）"), tags$td("Michaelis–Menten型代謝の飽和しやすさを規定する定数。全身PKモデルでは量ベースのparameterで、本アプリではOME処置によるPK変化をKmの相対増加として表現します。")),
              tags$tr(tags$td("Vp"), tags$td("Central / plasma distribution volume（中央・血漿分布容積）"), tags$td("X1から血漿濃度Cplasmaを計算するための見かけの容積。")),
              tags$tr(tags$td("Qe"), tags$td("Effect-site equilibration parameter（効果部位平衡化parameter）"), tags$td("血漿濃度に追従するlatent effect-siteの平衡化速度を規定するモデルparameterです。")),
              tags$tr(tags$td("Kp,e"), tags$td("Effect-site-to-plasma equilibrium ratio（効果部位/血漿平衡比）"), tags$td("latent effect-siteと血漿の平衡時の濃度比を規定する係数です。")),
              tags$tr(tags$td("Cplasma"), tags$td("Plasma concentration（血漿ペントバルビタール濃度）"), tags$td("中央compartmentから計算される血中濃度。")),
              tags$tr(tags$td("Ce"), tags$td("Effect-site concentration（効果部位濃度）"), tags$td("XeをVeで除して算出するlatent effect-site濃度。アプリ内の仮想『脳内濃度』として表示します。")),
              tags$tr(tags$td("C_RR,thr"), tags$td("Reference RR threshold（基準正向反射効果部位閾値）"), tags$td("LRRとRORRの判定に共通して用いる基準Ce閾値。処置別の実効閾値はC_RR,thr × M_RRです。")),
              tags$tr(tags$td("M_RR"), tags$td("Residual threshold multiplier（残差閾値倍率）"), tags$td("PK fit後に文献の麻酔時間比を再現するために求めた残差的な閾値倍率。処置別実効閾値をC_RR,thr × M_RRとして与えます。単独では純粋なPD parameterとして解釈できません。")),
              tags$tr(tags$td("IIV"), tags$td("Interindividual Variability（個体間変動）"), tags$td("ラットごとの個体差。本アプリではBW、F_eff、ka、Vmaxにlog-normalな個体間変動を付与します。")),
              tags$tr(tags$td("CV"), tags$td("Coefficient of Variation（変動係数）"), tags$td("ばらつきの大きさを平均値に対する割合で表す指標。本アプリでは個体間変動および測定誤差の大きさの指定に用います。")),
              tags$tr(tags$td("Cstd"), tags$td("Standard substrate concentration（標準基質濃度）"), tags$td("肝virtual ex vivo assayで全ラットに共通して与えるペントバルビタール基質濃度。")),
              tags$tr(tags$td("Km,C"), tags$td("Concentration-form Michaelis constant（濃度表示のMichaelis定数）"), tags$td("肝virtual ex vivo assayの計算で用いる濃度単位のKm。全身PKモデルの量ベースKmとは別の基準値ですが、処置によるKmの相対変化は同じ係数を適用します。")),
              tags$tr(tags$td("v_assay"), tags$td("Virtual assay metabolic rate（仮想assay代謝速度）"), tags$td("標準化した基質条件で計算する肝ペントバルビタール代謝速度。実測microsomal specific activityそのものではありません。")),
              tags$tr(tags$td("ex vivo"), tags$td("ex vivo（摘出組織を用いる条件）"), tags$td("生体から取り出した組織を使って行う実験。本アプリの肝assayはその考え方を模したvirtual assayです。")),
              tags$tr(tags$td("Michaelis–Menten"), tags$td("Michaelis–Menten kinetics（Michaelis–Menten型反応速度論）"), tags$td("基質濃度が高くなると代謝速度がVmaxへ近づき、飽和する関係。"))
            )
          )),

      div(class = "reference-box app-lead",
          strong("処置群とモデル化の考え方"),
          tags$ul(
            tags$li("Control：吸収・分布・代謝・脳移行・正向反射閾値を含む基準モデル。"),
            tags$li("PHB：Means et al. (1978) の PHB 10 mg/kg i.p. → 24時間後にPTB 50 mg/kg i.p. の条件を主要な根拠とし、肝ミクロソーム酸化的薬物代謝の誘導によるペントバルビタール代謝能の増加を表現。"),
            tags$li("OME：Henry et al. (1986) の omeprazole 40 mg/kg i.v. → 30分後にPTB 45 mg/kg i.p. の条件を主要な根拠とし、高用量ラット条件での酸化的薬物代謝阻害によるペントバルビタール代謝能の低下を表現。")
          ),
          p("F_eff、ka、分布parameter、effect-site parameter、および基準となるVmax・Km・C_RR,thrは共通です。処置差は、PHBではVmaxを相対的に増加、OMEではKmを相対的に増加させてPKへ反映し、さらにPK fit後に求めたM_RRをLRR閾値へ反映します。"),
          p(sprintf("現在の実装値：PHBはVmax ×%.3f、Km ×1.000、M_RR ×%.3f；OMEはVmax ×1.000、Km ×%.3f、M_RR ×%.3f。OMEのPK変化はHenry et al. (1986)のpublished clearance ratio（3.7/5.3）をprimary targetとして求めたCL-primary Km-only translationです。",
                    MODEL_DEFAULTS$treatment$PHB$vmax_mult,
                    MODEL_DEFAULTS$treatment$PHB$M_RR,
                    MODEL_DEFAULTS$treatment$OME$km_mult,
                    MODEL_DEFAULTS$treatment$OME$M_RR))),

      div(class = "reference-box app-lead",
          strong("肝virtual ex vivo ペントバルビタール代謝assay"),
          p(HTML("全ラットに共通のペントバルビタール基質濃度C<sub>std</sub> = 56.6 µg/mL（250 µM）を与えた仮想ex vivo assayとして、")),
          div(style = "margin: 8px 0 10px 18px; padding: 8px 12px; background: #f8f9fa; border-left: 4px solid #6c757d; font-size: 15px;",
              HTML("<b>v<sub>assay</sub> = V<sub>max,treat</sub> × C<sub>std</sub> / (K<sub>m,C,treat</sub> + C<sub>std</sub>)</b><br>",
                   "C<sub>std</sub> = 56.6 µg/mL（250 µM；全群共通）<br>",
                   "基準 V<sub>max</sub> = 2.19 mg/h（300 g reference model）<br>",
                   "基準 K<sub>m,C</sub> = 5.34 µg/mL（virtual ex vivo assay用の濃度ベース基準値）<br>",
                   "V<sub>max,treat</sub>：基準Vmaxに体重補正・Vmax個体差・処置によるVmax相対変化を反映<br>",
                   "K<sub>m,C,treat</sub>：基準Km,Cに処置によるKm相対変化を反映")),
          p("として代謝速度を算出します。解剖時の血中ペントバルビタール濃度は使用しません。Vmaxは全身PK reference modelの基準値2.19 mg/hを起点に、体重補正・個体間変動・処置によるVmax相対変化を反映した各個体のoperative Vmaxを用います。一方、Km,Cはvirtual ex vivo assay用の濃度ベース基準値5.34 µg/mLに、全身PKで採用した処置によるKm相対変化を適用します。したがってPHBではVmax ×2.011・Km ×1.000、OMEではVmax ×1.000・Km ×3.456がvirtual assayにも反映されます。全身PKモデルのKmは量ベースであり、Km,Cと同一の数値ではありません。250 µMという基質条件はKuntzman et al. (1967)のin vitro assay条件に対応します。出力は現在のPKモデルに由来するmg/hであり、実測microsomal specific activityそのものではありません。")),

      div(class = "reference-box app-lead",
          strong("個体間変動（IIV）と測定誤差"),
          p("模式図では概要のみを示しています。実装上のばらつきは以下の通りです。個体間変動と測定誤差はいずれもmultiplicative log-normal variability/errorとして与えています。"),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(
              tags$th("種類"), tags$th("対象"), tags$th("CV"), tags$th("実装")
            )),
            tags$tbody(
              tags$tr(tags$td("個体間変動"), tags$td("BW"), tags$td("5%"), tags$td("30匹の仮想ラットbank生成時にlog-normalに付与")),
              tags$tr(tags$td("個体間変動"), tags$td("F_eff"), tags$td("10%"), tags$td("30匹の仮想ラットbank生成時にlog-normalに付与")),
              tags$tr(tags$td("個体間変動"), tags$td("ka"), tags$td("10%"), tags$td("30匹の仮想ラットbank生成時にlog-normalに付与")),
              tags$tr(tags$td("個体間変動"), tags$td("Vmax"), tags$td("15%"), tags$td("30匹の仮想ラットbank生成時にlog-normalに付与")),
              tags$tr(tags$td("測定誤差"), tags$td("血中PTB濃度"), tags$td("6%"), tags$td("採血ごとにlog-normal measurement errorを付与")),
              tags$tr(tags$td("測定誤差"), tags$td("終末脳内PTB濃度"), tags$td("6%"), tags$td("終末脳測定時にlog-normal measurement errorを付与")),
              tags$tr(tags$td("測定誤差"), tags$td("肝virtual ex vivo PTB代謝活性"), tags$td("10%"), tags$td("virtual assay測定時にlog-normal measurement errorを付与"))
            )
          ),
          p(sprintf("仮想ラットbankは固定seed（%d）で作成しているため、同じbankを用いる限り各ラット固有のBW、F_eff、ka、Vmax個体差は再現されます。",
                    CODE03_VARIABILITY_POLICY$rat_bank_seed)),
          p("独立したIIVを付与していないparameter：Km、k12/k21/k13/k31、Qe、Ve、Kp,e、基準C_RR,thr、M_RR。Kmは体重補正と処置による相対変化のため個体間で値が異なる場合がありますが、独立したKm-IIVは与えていません。"),
          p("正向反射の確認時刻そのものには追加のランダム測定誤差を加えていません。模擬実験実施者が確認操作を行ったvirtual clock上の時刻で判定します。")),

      div(class = "reference-box app-lead",
          strong("この版で実装している最終パラメータ構成"),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(tags$th("群"), tags$th("Vmax 相対変化"), tags$th("Km 相対変化"), tags$th("M_RR"), tags$th("モデル上の意味"))),
            tags$tbody(
              tags$tr(tags$td("Control"), tags$td("×1.000"), tags$td("×1.000"), tags$td("×1.000"), tags$td("基準モデル")),
              tags$tr(tags$td("PHB"), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$PHB$vmax_mult)), tags$td("×1.000"), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$PHB$M_RR)), tags$td("代謝能誘導をVmax増加として表現")),
              tags$tr(tags$td("OME"), tags$td("×1.000"), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$OME$km_mult)), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$OME$M_RR)), tags$td("Henry 1986のclearance低下をKm増加として表現"))
            )
          ),
          p("全身PKのVmax/Km変化と、行動を再現するためのM_RRは役割を分けています。Vmax/KmはPKデータから先に決定し、M_RRはその後に行動差から決定しています。実際の正向反射判定にはC_RR,thr × M_RRを用います。"),
          p("肝virtual ex vivo assayでは、各群に設定されたVmaxとKmの相対変化の両方を同じ標準基質濃度に適用します。PHBではVmax増加、OMEではKm増加がそのまま肝代謝活性の計算に反映されます。")),

      div(class = "reference-box app-lead",
          strong("主な参考文献"),
          tags$ul(
            tags$li(HTML("<b>Hatanaka T, Sato S, Endoh M, Katayama K, Kakemi M, Koizumi T.</b> Effect of chlorpromazine on the pharmacokinetics and pharmacodynamics of pentobarbital in rats. <i>J Pharmacobiodyn.</i> 1988;11(1):18–30. doi:10.1248/bpb1978.11.18. 3-compartment open model、Michaelis–Menten型消失、脳/血漿濃度関係の主要source。")),
            tags$li(HTML("<b>Means JR, Schnell RC, Miya TS, Bousquet WF.</b> Correlation of phenobarbital- and SKF 525-A-induced modification of pentobarbital hypnosis with alteration of in vivo and in vitro pentobarbital metabolism in the rat. <i>Pharmacology.</i> 1978;16(4):181–192. doi:10.1159/000136765. PHB群の主要根拠。")),
            tags$li(HTML("<b>Henry DA, Macdonald IA, Kitchingman G, Bell GD, Langman MJS.</b> Omeprazole effects on oxidative drug metabolism. <i>Clin Exp Pharmacol Physiol.</i> 1986;13:377–381. doi:10.1111/j.1440-1681.1986.tb00916.x. OME群の主要根拠。")),
            tags$li(HTML("<b>Kuntzman R, Ikeda M, Jacobson M, Conney AH.</b> A sensitive method for the determination and isolation of pentobarbital-C14 metabolites and its application to in vitro studies of drug metabolism. <i>J Pharmacol Exp Ther.</i> 1967;157(1):220–226. 肝ペントバルビタール代謝のin vitro assay条件の参考。")),
            tags$li("その他の文献endpointは、モデル構築や外部検証の参考情報として使用しています。")
          ))
    )
  )
)



server <- function(input, output, session) {

  .hosted_shiny <- nzchar(Sys.getenv("SHINY_PORT")) ||
                   nzchar(Sys.getenv("RSCONNECT_CONTENT_URL")) ||
                   nzchar(Sys.getenv("CONNECT_SERVER"))
  if (!.hosted_shiny) {
    session$onSessionEnded(function() {
      try(shiny::stopApp(), silent = TRUE)
    })
  }
  exp_rv <- reactiveVal(NULL)
  refresh <- reactiveVal(0L)
  action_message <- reactiveVal("実験を開始すると、ここに操作結果が表示されます。")
  revealed <- reactiveVal(FALSE)

  # Post-experiment model-internal calculations are relatively expensive.
  # Compute them once per experiment and reuse them across tables, plots, and downloads.
  post_analysis_cache <- reactiveVal(NULL)

  bump <- function() refresh(refresh() + 1L)

  clear_post_analysis_cache <- function() {
    post_analysis_cache(NULL)
    invisible(NULL)
  }

  compute_post_analysis_cache <- function(e) {
    transitions <- setNames(vector("list", length(e$active_rat_ids)), e$active_rat_ids)
    trajectories <- setNames(vector("list", length(e$active_rat_ids)), e$active_rat_ids)

    for (rat_id in e$active_rat_ids) {
      if (is.na(e$dose_time[[rat_id]])) {
        transitions[[rat_id]] <- list(
          onset_min = NA_real_, recovery_min = NA_real_,
          sleep_duration_min = NA_real_, search_horizon_min = NA_real_,
          status = "not dosed"
        )
        trajectories[[rat_id]] <- NULL
      } else {
        r <- as.list(e$rat_bank[e$rat_bank$RatID == rat_id, , drop = FALSE][1, ])
        tr <- as.character(e$treatment[[rat_id]])

        transitions[[rat_id]] <- find_true_righting_reflex_transitions(
          r, dose_mgkg = e$dose_mgkg, treatment = tr
        )

        max_obs <- suppressWarnings(max(c(
          120,
          e$blood_log$elapsed_postdose_min[e$blood_log$RatID == rat_id],
          e$rr_log$elapsed_postdose_min[e$rr_log$RatID == rat_id]
        ), na.rm = TRUE))
        plot_times <- seq(0, max_obs + 20, by = 0.25)
        trajectories[[rat_id]] <- simulate_rat(
          r, e$dose_mgkg, tr, plot_times
        )
      }
    }

    summary <- build_debrief_summary(e, transitions = transitions)
    list(
      transitions = transitions,
      trajectories = trajectories,
      summary = summary
    )
  }

  get_post_analysis_cache <- function(e) {
    x <- post_analysis_cache()
    if (is.null(x)) {
      x <- compute_post_analysis_cache(e)
      post_analysis_cache(x)
    }
    x
  }

  assignments <- reactive({
    c(
      setNames(rep("Control", length(input$control_rats)), input$control_rats),
      setNames(rep("PHB", length(input$phb_rats)), input$phb_rats),
      setNames(rep("OME", length(input$ome_rats)), input$ome_rats)
    )
  })

  assignment_error <- reactive({
    groups <- list(Control = input$control_rats, PHB = input$phb_rats, OME = input$ome_rats)
    lens <- vapply(groups, length, integer(1))
    if (any(lens != 3L)) return("各群ちょうど3匹を選択してください。")
    all_ids <- unlist(groups, use.names = FALSE)
    if (anyDuplicated(all_ids)) return("同じラットを複数群へ割り付けることはできません。")
    NULL
  })

  output$setup_validation <- renderUI({
    err <- assignment_error()
    if (is.null(err)) {
      div(class = "debrief-box", strong("割付チェック："), "Control / PHB / OME 各3匹、重複なし。開始できます。")
    } else {
      div(class = "note-box", strong("割付を修正してください："), err)
    }
  })

  observeEvent(input$random_allocate, {
    ids <- sample(rat_bank$RatID, 9, replace = FALSE)
    updateSelectizeInput(session, "control_rats", selected = ids[1:3])
    updateSelectizeInput(session, "phb_rats", selected = ids[4:6])
    updateSelectizeInput(session, "ome_rats", selected = ids[7:9])
  })

  output$allocation_table <- renderTable({
    assign_now <- assignments()
    x <- rat_bank[, c("RatID", "BW_kg"), drop = FALSE]
    x$BW_g <- round(x$BW_kg * 1000)
    x$Group <- ifelse(x$RatID %in% names(assign_now), unname(assign_now[x$RatID]), "未使用")
    x <- x[, c("RatID", "BW_g", "Group")]
    names(x) <- c("Rat", "体重 (g)", "現在の割付")
    x
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  observeEvent(input$session_file, {
    req(input$session_file$datapath)
    tryCatch({
      e <- load_experiment_session(input$session_file$datapath)
      exp_rv(e)
      clear_post_analysis_cache()
      withProgress(message = "モデル内部情報を準備しています...", value = 0.5, {
        post_analysis_cache(compute_post_analysis_cache(e))
      })
      revealed(TRUE)
      ids <- e$active_rat_ids
      tr <- vapply(ids, function(id) as.character(e$treatment[[id]]), character(1))
      updateSelectizeInput(session, "control_rats", selected = ids[tr == "Control"])
      updateSelectizeInput(session, "phb_rats", selected = ids[tr == "PHB"])
      updateSelectizeInput(session, "ome_rats", selected = ids[tr == "OME"])
      updateNumericInput(session, "dose_mgkg", value = e$dose_mgkg)
      updateRadioButtons(session, "active_rat", choices = ids, selected = ids[1])
      updateSelectInput(session, "debrief_rat", choices = ids, selected = ids[1])
      action_message("保存したsessionを復元しました。モデル内部情報を表示しています。")
      bump()
      updateNavbarPage(session, "main_tab", selected = "4. モデル内部情報の開示")
      showNotification("Sessionを復元しました。", type = "message")
    }, error = function(err) {
      showNotification(paste("Sessionを読み込めません:", conditionMessage(err)), type = "error")
    })
  })

  observeEvent(input$start_experiment, {
    err <- assignment_error()
    if (!is.null(err)) {
      showNotification(err, type = "error")
      return()
    }
    ids <- c(input$control_rats, input$phb_rats, input$ome_rats)
    e <- new_experiment(
      rat_bank = rat_bank,
      active_rat_ids = ids,
      treatment_by_rat = assignments(),
      dose_mgkg = input$dose_mgkg
    )
    exp_rv(e)
    clear_post_analysis_cache()
    revealed(FALSE)
    updateRadioButtons(session, "active_rat", choices = ids, selected = ids[1])
    updateSelectInput(session, "debrief_rat", choices = ids, selected = ids[1])
    action_message(sprintf("実験を開始しました。ペントバルビタール %.1f mg/kg i.p.。まず操作するラットを選んでください。", input$dose_mgkg))
    bump()
    updateNavbarPage(session, "main_tab", selected = "2. 実験操作")
  })

  output$experimentStarted <- reactive(!is.null(exp_rv()))
  outputOptions(output, "experimentStarted", suspendWhenHidden = FALSE)
  output$debriefRevealed <- reactive(isTRUE(revealed()))
  outputOptions(output, "debriefRevealed", suspendWhenHidden = FALSE)

  output$experiment_gate <- renderUI({
    if (is.null(exp_rv())) div(class = "note-box", "先に『1. 実験設定』で実験を開始してください。")
  })
  output$log_gate <- renderUI({
    if (is.null(exp_rv())) div(class = "note-box", "実験開始後に操作ログが表示されます。")
  })
  output$debrief_gate <- renderUI({
    if (is.null(exp_rv())) div(class = "note-box", "先に実験を開始してください。実験終了後にモデル内部情報を開示すると、ここで確認できます。")
  })

  safe_action <- function(expr, success = NULL) {
    if (isTRUE(revealed())) {
      msg <- "実験は終了しており、モデル内部情報が開示されています。新しい実験操作はできません。"
      showNotification(msg, type = "error")
      action_message(msg)
      return(NULL)
    }
    tryCatch({
      value <- force(expr)
      if (!is.null(success)) action_message(success(value))
      bump()
      value
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      action_message(paste("操作できません：", conditionMessage(e)))
      NULL
    })
  }

  observeEvent(input$dose_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      administer_pentobarbital(e, rat_id),
      function(x) sprintf("%s にペントバルビタールを投与しました。現在時刻 %.2f min。", rat_id, e$clock_min)
    )
  })

  observeEvent(input$lrr_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      check_righting_reflex(e, rat_id),
      function(x) sprintf("%s：正向反射 %s（投与後 %.2f min、時計 %.2f min）。",
                          rat_id, ifelse(x$righting_reflex == "absent", "なし（麻酔）", "覚醒"),
                          x$elapsed_postdose_min, x$virtual_clock_min)
    )
  })

  observeEvent(input$lrr_blood_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      collect_lrr_and_blood(e, rat_id, add_assay_error = TRUE),
      function(x) sprintf("%s：正向反射 %s、血中ペントバルビタール濃度 %.2f µg/mL（投与後 %.2f min、時計 %.2f min）。",
                          rat_id,
                          ifelse(x$lrr$righting_reflex == "absent", "なし（麻酔）", "覚醒"),
                          x$blood$measured_plasma_ug_mL,
                          x$blood$elapsed_postdose_min,
                          x$blood$virtual_clock_min)
    )
  })

  quick_wait <- function(minutes) {
    e <- req(exp_rv())
    safe_action(
      wait_minutes(e, minutes),
      function(x) sprintf("%.2f分待機しました。現在時刻 %.2f min。", minutes, e$clock_min)
    )
  }
  observeEvent(input$wait_05_btn, quick_wait(0.5))
  observeEvent(input$wait_1_btn, quick_wait(1.0))
  observeEvent(input$wait_5_btn, quick_wait(5.0))
  observeEvent(input$wait_10_btn, quick_wait(10.0))

  observeEvent(input$wait_btn, {
    e <- req(exp_rv())
    safe_action(
      wait_minutes(e, input$wait_min),
      function(x) sprintf("%.2f分待機しました。現在時刻 %.2f min。", input$wait_min, e$clock_min)
    )
  })

  observeEvent(input$tissue_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      collect_terminal_tissues(e, rat_id),
      function(x) sprintf(
        "%s：組織採取を行いました。脳内ペントバルビタール %.2f µg/g、肝virtual ex vivo ペントバルビタール代謝活性 %.3f mg/h（共通基質条件 %.0f µM）。このラットの組織採取は完了しました。以後このラットには操作できません。",
        rat_id, x$brain$brain_ug_g, x$liver$measured_activity_mg_h, x$liver$substrate_uM
      )
    )
  })

  output$clock_text <- renderText({
    refresh(); e <- req(exp_rv()); fmt_num(e$clock_min, 2)
  })

  output$action_result <- renderUI({
    refresh(); HTML(paste0("<strong>直近の操作：</strong><br>", htmltools::htmlEscape(action_message())))
  })

  output$status_table <- renderTable({
    refresh(); e <- req(exp_rv())
    x <- experiment_status(e)
    x$dose_time_min <- fmt_num(x$dose_time_min)
    x$elapsed_postdose_min <- fmt_num(x$elapsed_postdose_min)
    x$latest_lrr_clock_min <- fmt_num(x$latest_lrr_clock_min)
    x$latest_rr <- ifelse(is.na(x$latest_rr), "未確認",
                           ifelse(x$latest_rr == "absent", "なし（麻酔）", "覚醒"))
    names(x) <- c("Rat", "群", "投与時刻", "投与後時間", "最新の正向反射", "正向反射確認時刻", "採血回数", "組織採取")
    x
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$blood_table <- renderTable({
    refresh(); e <- req(exp_rv())
    if (!nrow(e$blood_log)) return(NULL)
    idx <- rev(seq_len(nrow(e$blood_log)))
    x <- e$blood_log[idx, , drop = FALSE]
    x <- head(x, 10)
    x <- x[, c("RatID", "treatment", "virtual_clock_min", "elapsed_postdose_min", "sample_number", "measured_plasma_ug_mL")]
    x$virtual_clock_min <- round(x$virtual_clock_min, 2)
    x$elapsed_postdose_min <- round(x$elapsed_postdose_min, 2)
    x$measured_plasma_ug_mL <- round(x$measured_plasma_ug_mL, 2)
    names(x) <- c("Rat", "群", "時計", "投与後", "採血#", "測定濃度 µg/mL")
    x
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$lrr_table <- renderTable({
    refresh(); e <- req(exp_rv())
    if (!nrow(e$rr_log)) return(NULL)
    idx <- rev(seq_len(nrow(e$rr_log)))
    x <- e$rr_log[idx, , drop = FALSE]
    x <- head(x, 10)
    x$virtual_clock_min <- round(x$virtual_clock_min, 2)
    x$elapsed_postdose_min <- round(x$elapsed_postdose_min, 2)
    x$righting_reflex <- ifelse(x$righting_reflex == "absent", "なし（麻酔）", "覚醒")
    names(x) <- c("Rat", "群", "Dose", "時計", "投与後", "正向反射")
    x
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$event_log_table <- renderTable({
    refresh(); e <- req(exp_rv())
    if (!nrow(e$event_log)) return(NULL)

    idx <- rev(seq_len(nrow(e$event_log)))
    x <- e$event_log[idx, , drop = FALSE]

    # During the experiment, preserve the current compact latest-30 display.
    # After the experiment is finished/revealed, show the complete operation log.
    if (!isTRUE(revealed())) {
      x <- head(x, 30)
    }

    x$clock_start_min <- round(x$clock_start_min, 2)
    x$clock_end_min <- round(x$clock_end_min, 2)
    x$elapsed_postdose_min <- round(x$elapsed_postdose_min, 2)

    x$event <- .event_label_ja(x$event)
    x$result <- .event_result_ja(x$result)

    names(x) <- c(
      "#", "開始", "終了", "Rat", "操作", "群", "投与後", "結果"
    )
    x
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  # Keep the log reactive even while the log tab is hidden.
  # Together with refresh()/bump(), this makes the latest operation available immediately
  # when the user opens the log tab.
  outputOptions(output, "event_log_table", suspendWhenHidden = FALSE)

  output$download_logs <- downloadHandler(
    filename = function() paste0("Virtual_CYP_PK_experiment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      e <- req(exp_rv())
      td <- tempfile("pkpd_export_")
      dir.create(td)
      export_experiment_csv(e, td)
      old <- setwd(td); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(td))
    },
    contentType = "application/zip"
  )

  output$download_session <- downloadHandler(
    filename = function() paste0("Virtual_CYP_PK_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) {
      e <- req(exp_rv())
      save_experiment_session(e, file, model_version = "Code03-JA-CLprimary-MRR-v1.4")
    },
    contentType = "application/octet-stream"
  )

  observeEvent(input$finish_experiment_btn, {
    e <- req(exp_rv())
    if (!isTRUE(revealed())) {
      withProgress(message = "モデル内部情報を計算しています...", value = 0.5, {
        post_analysis_cache(compute_post_analysis_cache(e))
      })
      revealed(TRUE)
      action_message("実験を終了しました。データ取得は完了です。実験中には見えなかったモデル内部情報を開示しました。以後の実験操作はできません。")
      bump()
    }
    updateNavbarPage(session, "main_tab", selected = "4. モデル内部情報の開示")
  })

  output$latent_table <- renderTable({
    req(revealed()); refresh(); e <- req(exp_rv()); rat_id <- req(input$debrief_rat)
    r <- e$rat_bank[e$rat_bank$RatID == rat_id, , drop = FALSE]
    tr <- as.character(e$treatment[[rat_id]])
    p <- scaled_rat_parameters(as.list(r), tr)
    data.frame(
      `表示項目` = c(
        "Treatment（処置群）",
        "Body weight（体重）",
        "Effective systemic input fraction (F_eff)",
        "Absorption rate constant (ka)",
        "Vmax interindividual relative factor (IIV)",
        "Treatment-related Vmax relative change",
        "Operative maximum metabolic rate (Vmax)",
        "Treatment-related Km relative change",
        "Operative systemic-model Km",
        "残差閾値倍率 (M_RR)",
        "基準正向反射 effect-site閾値 (C_RR,thr)",
        "実効正向反射閾値 (C_RR,thr × M_RR)"
      ),
      `値` = c(
        treatment_label(tr),
        paste0(fmt_num(r$BW_kg, 3), " kg"),
        fmt_num(r$S_IP, 3),
        paste0(fmt_num(r$ka_h_1, 2), " h^-1"),
        fmt_num(r$Vmax_IIV_mult, 3),
        paste0("×", fmt_num(MODEL_DEFAULTS$treatment[[tr]]$vmax_mult, 3)),
        paste0(fmt_num(p$Vmax, 3), " mg/h"),
        paste0("×", fmt_num(MODEL_DEFAULTS$treatment[[tr]]$km_mult, 3)),
        paste0(fmt_num(p$Km, 4), " mg"),
        paste0("×", fmt_num(MODEL_DEFAULTS$treatment[[tr]]$M_RR, 3)),
        paste0(fmt_num(MODEL_DEFAULTS$C_RR_thr, 2), " µg/g"),
        paste0(fmt_num(p$C_RR_thr, 2), " µg/g")
      ),
      `意味` = c(
        "このラットに行った前処置。",
        "投与量の計算や分布容積などの個体差に影響します。",
        "腹腔内投与から全身循環へ入る有効入力を表すモデル上の係数。厳密なbioavailabilityそのものではありません。",
        "腹腔内から吸収される速さ。大きいほど立ち上がりが速くなります。",
        "Interindividual Variability（個体間変動）による、この個体固有のVmaxの相対変化。",
        "処置によってVmaxへ与える相対変化。Control/OMEは×1、PHBは約×2.011。",
        "体重・個体差・処置によるVmax変化をすべて反映した、この個体で実際にODEへ使用する最大代謝速度。",
        "処置によってKmへ与える相対変化。Control/PHBは×1、OMEは約×3.456。",
        "体重補正と処置によるKm変化を反映した、全身PKモデルで実際にODEへ使用する量ベースKm。",
        "PK fit後に文献の麻酔時間比を再現するために設定した残差的な正向反射閾値倍率。純粋なPD parameterとはみなしません。",
        "Controlを基準に校正した共通のeffect-site閾値。",
        "実際の正向反射判定に使用する処置別閾値。Ceがこの値以上のときLRR/hypnosis stateと判定します。"
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$true_transition_table <- renderTable({
    req(revealed()); refresh(); e <- req(exp_rv()); rat_id <- req(input$debrief_rat)
    if (is.na(e$dose_time[[rat_id]])) return(data.frame(Item = "ペントバルビタール未投与", Value = "—"))
    cache <- get_post_analysis_cache(e)
    z <- cache$transitions[[rat_id]]
    data.frame(
      Item = c("真の正向反射消失", "真の正向反射回復", "真の麻酔持続時間", "真値探索の状態"),
      Value = c(if (is.na(z$onset_min)) "麻酔成立せず" else paste0(fmt_num(z$onset_min, 2), " min"),
                if (is.na(z$recovery_min)) "—" else paste0(fmt_num(z$recovery_min, 2), " min"),
                if (is.na(z$sleep_duration_min)) "—" else paste0(fmt_num(z$sleep_duration_min, 2), " min"),
                paste0(z$status, "（探索範囲 ", fmt_num(z$search_horizon_min, 0), " min）")),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$all_latent_table <- renderTable({
    req(revealed()); refresh(); e <- req(exp_rv())
    x <- get_post_analysis_cache(e)$summary
    out <- x[, c("RatID","treatment","BW_kg","F_eff","ka_h_1",
                 "treatment_Vmax_relative_change","operative_Vmax_mg_h",
                 "treatment_Km_relative_change","operative_Km_mg",
                 "M_RR","effective_C_RR_thr_ug_g",
                 "true_Cp_15_ug_mL","true_Cp_30_ug_mL","true_Cp_60_ug_mL","true_Cp_120_ug_mL",
                 "true_LRR_onset_min","true_LRR_recovery_min",
                 "true_LRR_duration_min","hepatic_exvivo_activity_mg_h")]
    names(out) <- c("Rat","群","BW (kg)","F_eff","ka (h^-1)",
                    "Vmax相対変化","Vmax (mg/h)",
                    "Km相対変化","Km (mg)",
                    "M_RR","実効正向反射閾値 (ug/g)",
                    "血中PTB 15分 (ug/mL)","血中PTB 30分 (ug/mL)",
                    "血中PTB 60分 (ug/mL)","血中PTB 120分 (ug/mL)",
                    "真の正向反射消失 (min)","真の正向反射回復 (min)",
                    "真の麻酔持続時間 (min)","肝virtual ex vivo PTB代謝活性 (mg/h)")
    out$`Vmax相対変化` <- paste0("×", sprintf("%.3f", out$`Vmax相対変化`))
    out$`Km相対変化` <- paste0("×", sprintf("%.3f", out$`Km相対変化`))
    out$M_RR <- sprintf("%.3f", out$M_RR)
    num <- sapply(out, is.numeric)
    out[num] <- lapply(out[num], function(z) round(z, 2))
    out
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$download_debrief_summary <- downloadHandler(
    filename = function() paste0("Virtual_CYP_PK_all_rat_post_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      e <- req(exp_rv()); req(revealed())
      write.csv(get_post_analysis_cache(e)$summary, file, row.names = FALSE, na = "")
    }
  )

  output$download_debrief_plot <- downloadHandler(
    filename = function() paste0(input$debrief_rat, "_post_experiment_analysis.png"),
    content = function(file) {
      e <- req(exp_rv()); req(revealed()); rat_id <- req(input$debrief_rat)
      png(file, width = 1500, height = 1000, res = 150)
      on.exit(dev.off(), add = TRUE)
      {
        cache <- get_post_analysis_cache(e)
        plot_debrief_rat(
          e, rat_id,
          transition = cache$transitions[[rat_id]],
          trajectory = cache$trajectories[[rat_id]]
        )
      }
    },
    contentType = "image/png"
  )

  output$download_debrief_all <- downloadHandler(
    filename = function() paste0("Virtual_CYP_PK_full_post_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      e <- req(exp_rv()); req(revealed())
      td <- tempfile("pkpd_debrief_"); dir.create(td)
      cache <- get_post_analysis_cache(e)
      write.csv(cache$summary, file.path(td, "all_rat_post_analysis_summary.csv"),
                row.names = FALSE, na = "")
      export_experiment_csv(e, td)
      for (rat_id in e$active_rat_ids) {
        png(file.path(td, paste0(rat_id, "_post_experiment_analysis.png")),
            width = 1500, height = 1000, res = 150)
        plot_debrief_rat(
          e, rat_id,
          transition = cache$transitions[[rat_id]],
          trajectory = cache$trajectories[[rat_id]]
        )
        dev.off()
      }
      old <- setwd(td); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(td))
    },
    contentType = "application/zip"
  )

  output$group_comparison_plot <- renderPlot({
    req(revealed()); refresh(); e <- req(exp_rv())

    groups <- c("Control","PHB","OME")
    group_labels <- c("Control","PHB","OME")

    ds <- get_post_analysis_cache(e)$summary
    sleep_vals <- lapply(groups, function(g) {
      ds$true_LRR_duration_min[ds$treatment == g]
    })

    dc <- data.frame(
      RatID = ds$RatID,
      treatment = ds$treatment,
      Cp15 = ds$true_Cp_15_ug_mL,
      Cp60 = ds$true_Cp_60_ug_mL,
      Cp120 = ds$true_Cp_120_ug_mL,
      stringsAsFactors = FALSE
    )

    plot_group_points <- function(vals, ylab, main) {
      finite_all <- unlist(lapply(vals, function(x) x[is.finite(x)]), use.names = FALSE)
      if (!length(finite_all)) {
        plot.new(); text(0.5, 0.5, "No model values available"); return(invisible(NULL))
      }
      ymax <- max(finite_all, na.rm = TRUE) * 1.15
      if (!is.finite(ymax) || ymax <= 0) ymax <- 1
      plot(NA, xlim = c(0.5,3.5), ylim = c(0,ymax), xaxt = "n",
           xlab = "Treatment", ylab = ylab, main = main)
      axis(1, at = 1:3, labels = group_labels)
      for (i in seq_along(vals)) {
        z <- vals[[i]][is.finite(vals[[i]])]
        if (length(z)) {
          points(rep(i,length(z)), z, pch = 19)
          points(i, mean(z), pch = 1, cex = 1.7, lwd = 2)
        }
      }
    }

    op <- par(mfrow = c(2,2), mar = c(4.2,5.0,3.2,1.0), oma = c(0,0,1.2,0))
    on.exit(par(op), add = TRUE)

    plot_group_points(
      sleep_vals,
      "True anaesthesia duration (min)",
      "Anaesthesia duration"
    )

    if (is.null(dc) || !nrow(dc)) {
      for (ttl in c("Plasma pentobarbital at 15 min",
                    "Plasma pentobarbital at 60 min",
                    "Plasma pentobarbital at 120 min")) {
        plot.new(); title(main = ttl); text(0.5,0.5,"No model values available")
      }
    } else {
      vals15 <- lapply(groups, function(g) dc$Cp15[dc$treatment == g])
      vals60 <- lapply(groups, function(g) dc$Cp60[dc$treatment == g])
      vals120 <- lapply(groups, function(g) dc$Cp120[dc$treatment == g])

      plot_group_points(vals15, "True plasma concentration (ug/mL)",
                        "Plasma pentobarbital at 15 min")
      plot_group_points(vals60, "True plasma concentration (ug/mL)",
                        "Plasma pentobarbital at 60 min")
      plot_group_points(vals120, "True plasma concentration (ug/mL)",
                        "Plasma pentobarbital at 120 min")
    }

    mtext("Model-internal true values (not measured data)", outer = TRUE, cex = 1.0)
  })

  output$trajectory_plot <- renderPlot({
    req(revealed()); refresh(); e <- req(exp_rv()); rat_id <- req(input$debrief_rat)
    cache <- get_post_analysis_cache(e)
    plot_debrief_rat(
      e, rat_id,
      transition = cache$transitions[[rat_id]],
      trajectory = cache$trajectories[[rat_id]]
    )
  })
}

shinyApp(ui, server)
