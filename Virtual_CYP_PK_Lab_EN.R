# ============================================================
# Virtual CYP-PK Lab — English version, final model
# Open this file as app.R in RStudio and click "Run App" to launch.
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
    assay = "Virtual ex vivo pentobarbital metabolic assay",
    biological_target = "Model-derived hepatic pentobarbital metabolic activity under standardized substrate conditions",
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
  if (is.na(e$dose_time[[rat_id]])) stop("Pentobarbital has not yet been administered: ", rat_id)
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
  if (!is.na(e$dose_time[[rat_id]])) stop("Pentobarbital has already been administered: ", rat_id)
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
    pentobarbital_dose = "Pentobarbital dosing",
    righting_reflex_check = "Righting-reflex check",
    blood_sample = "Blood sampling",
    righting_reflex_and_blood_sample = "Righting-reflex check + blood sampling",
    terminal_tissue_collection = "Terminal tissue collection (liver + brain)",
    wait = "Wait"
  )
  ans <- unname(map[as.character(x)])
  ans[is.na(ans)] <- as.character(x)[is.na(ans)]
  ans
}

.event_result_ja <- function(x) {
  x <- as.character(x)

  x <- sub(
    "^righting reflex present; measured ([0-9.]+) ug/mL$",
    "Righting reflex present (awake)；Plasma PTB concentration \\1 µg/mL",
    x
  )
  x <- sub(
    "^righting reflex absent; measured ([0-9.]+) ug/mL$",
    "Righting reflex absent (hypnosis)；Plasma PTB concentration \\1 µg/mL",
    x
  )
  x <- sub(
    "^measured ([0-9.]+) ug/mL$",
    "Plasma PTB concentration \\1 µg/mL",
    x
  )
  x <- sub(
    "^brain ([0-9.]+) ug/g; virtual ex vivo hepatic PTB activity ([0-9.]+) mg/h at ([0-9.]+) uM substrate$",
    "Brain PTB concentration \\1 µg/g；Hepatic virtual ex vivo PTB activity \\2 mg/h(substrate \\3 µM )",
    x
  )
  x
}

student_event_log <- function(e) {
  x <- e$event_log
  if (!nrow(x)) {
    return(data.frame(
      `Operation #` = integer(),
      `Start time (min)` = numeric(),
      `End time (min)` = numeric(),
      Rat = character(),
      `Action` = character(),
      `Group` = character(),
      `Post-dose time (min)` = numeric(),
      `Result` = character(),
      check.names = FALSE
    ))
  }

  data.frame(
    `Operation #` = x$event_id,
    `Start time (min)` = round(x$clock_start_min, 2),
    `End time (min)` = round(x$clock_end_min, 2),
    Rat = ifelse(is.na(x$RatID), "", x$RatID),
    `Action` = .event_label_ja(x$event),
    `Group` = ifelse(is.na(x$treatment), "", x$treatment),
    `Post-dose time (min)` = round(x$elapsed_postdose_min, 2),
    `Result` = .event_result_ja(x$result),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_blood_log <- function(e) {
  x <- e$blood_log
  data.frame(
    Rat = x$RatID,
    `Group` = x$treatment,
    `PTB dose (mg/kg)` = x$dose_mgkg,
    `Virtual experiment time (min)` = round(x$virtual_clock_min, 2),
    `Post-PTB-dose time (min)` = round(x$elapsed_postdose_min, 2),
    `Blood sample #` = x$sample_number,
    `Measured plasma PTB concentration (µg/mL)` = round(x$measured_plasma_ug_mL, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_rr_log <- function(e) {
  x <- e$rr_log
  rr <- ifelse(
    x$righting_reflex == "absent",
    "Absent (hypnosis)",
    ifelse(x$righting_reflex == "present", "Present (awake)", x$righting_reflex)
  )
  data.frame(
    Rat = x$RatID,
    `Group` = x$treatment,
    `PTB dose (mg/kg)` = x$dose_mgkg,
    `Virtual experiment time (min)` = round(x$virtual_clock_min, 2),
    `Post-PTB-dose time (min)` = round(x$elapsed_postdose_min, 2),
    `Righting reflex` = rr,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_brain_log <- function(e) {
  x <- e$brain_log
  data.frame(
    Rat = x$RatID,
    `Group` = x$treatment,
    `PTB dose (mg/kg)` = x$dose_mgkg,
    `Tissue collection time (min)` = round(x$virtual_clock_min, 2),
    `Post-PTB-dose time (min)` = round(x$elapsed_postdose_min, 2),
    `Measured brain PTB concentration (µg/g)` = round(x$brain_ug_g, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

student_liver_log <- function(e) {
  x <- .normalize_liver_log(e$liver_log)
  data.frame(
    Rat = x$RatID,
    `Group` = x$treatment,
    `Tissue collection time (min)` = round(x$virtual_clock_min, 2),
    `Post-PTB-dose time (min)` = round(x$elapsed_postdose_min, 2),
    `Common substrate concentration (µM)` = x$substrate_uM,
    `Common substrate concentration (µg/mL)` = x$substrate_ug_mL,
    `Measured hepatic virtual_ex_vivo_PTB activity (mg/h)` =
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
  c(Control = "Control (no pretreatment)",
    PHB = "Phenobarbital (PHB: induction of oxidative drug metabolism)",
    OME = "Omeprazole (OME: inhibition of oxidative drug metabolism)")[x]
}

experiment_visual_guide_ja <- function() {
  div(class = "reference-box app-lead",
      div(class = "panel-title-strong", "Experimental workflow"),
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
          <text x="110" y="102" text-anchor="middle" font-size="18" font-weight="700">Group assignment</text>
          <text x="110" y="132" text-anchor="middle" font-size="16">Control (3 rats)</text>
          <text x="110" y="157" text-anchor="middle" font-size="16">PHB (3 rats)</text>
          <text x="110" y="182" text-anchor="middle" font-size="16">OME (3 rats)</text>

          <line x1="200" y1="135" x2="280" y2="135" stroke="#4b5563" stroke-width="2.5" marker-end="url(#arrow)"/>

          <rect x="285" y="43" width="235" height="184" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="402" y="70" text-anchor="middle" font-size="18" font-weight="700">Pentobarbital dosing</text>
          <text x="402" y="101" text-anchor="middle" font-size="14.5">Check righting reflex over time</text>
          <text x="402" y="127" text-anchor="middle" font-size="14.5">RR check + blood sampling</text>
          <text x="402" y="153" text-anchor="middle" font-size="14.5">Sampling guide:</text>
          <text x="402" y="176" text-anchor="middle" font-size="14.5">15, 30, 60, 120 min</text>
          <text x="402" y="204" text-anchor="middle" font-size="14.5">Additional sampling as needed</text>

          <line x1="520" y1="135" x2="595" y2="135" stroke="#4b5563" stroke-width="2.5" marker-end="url(#arrow)"/>

          <rect x="600" y="42" width="225" height="190" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="712" y="66" text-anchor="middle" font-size="17" font-weight="700">Terminal tissue</text>\n          <text x="712" y="86" text-anchor="middle" font-size="17" font-weight="700">collection</text>
          <text x="712" y="112" text-anchor="middle" font-size="15">Brain</text>
          <text x="712" y="133" text-anchor="middle" font-size="13.5">virtual "brain concentration"</text>
          <text x="712" y="158" text-anchor="middle" font-size="15">Liver</text>
          <text x="712" y="180" text-anchor="middle" font-size="13.5">virtual ex vivo metabolic assay</text>
          <text x="712" y="207" text-anchor="middle" font-size="12.5">After collection, no further</text>
          <text x="712" y="224" text-anchor="middle" font-size="12.5">operations for that rat</text>

          <line x1="825" y1="135" x2="895" y2="135" stroke="#4b5563" stroke-width="2.5" marker-end="url(#arrow)"/>

          <rect x="900" y="66" width="195" height="148" rx="12" fill="#f8fafc" stroke="#64748b" stroke-width="2"/>
          <text x="997" y="94" text-anchor="middle" font-size="17" font-weight="700">Compare results</text>
          <text x="997" y="125" text-anchor="middle" font-size="14">Plasma PTB concentration</text>
          <text x="997" y="151" text-anchor="middle" font-size="14">Hypnosis duration</text>
          <text x="997" y="177" text-anchor="middle" font-size="13.5">Terminal brain/liver</text>\n          <text x="997" y="198" text-anchor="middle" font-size="13.5">measurements</text>
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
    plot.new(); text(0.5, 0.5, "Pentobarbital has not yet been administered.")
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
  "   version=\"1.1\"",
  "   id=\"svg75\"",
  "   xmlns:xlink=\"http://www.w3.org/1999/xlink\"",
  "   xmlns=\"http://www.w3.org/2000/svg\" preserveAspectRatio=\"xMidYMid meet\" style=\"width:100%;height:auto;max-width:100%;display:block;\">",
  "  ",
  "  <defs",
  "     id=\"defs9\">",
  "    <rect",
  "       x=\"891.19072\"",
  "       y=\"249.09808\"",
  "       width=\"302.3035\"",
  "       height=\"44.740918\"",
  "       id=\"rect76\" />",
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
  "       gradientTransform=\"matrix(1.0803944,0,0,0.89792859,11.915512,-40.134406)\"",
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
  "       gradientTransform=\"matrix(1.5786585,0,0,0.50200834,-1.0079965,75.118041)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#purpleFill\"",
  "       id=\"linearGradient82\"",
  "       x1=\"402.11637\"",
  "       y1=\"1449.0804\"",
  "       x2=\"402.11637\"",
  "       y2=\"1524.9134\"",
  "       gradientTransform=\"matrix(1.2896764,0,0,0.77538829,-12,0)\"",
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
  "       gradientTransform=\"matrix(1.2896764,0,0,0.77538829,2,0)\"",
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
  "       xlink:href=\"#purpleFill\"",
  "       id=\"linearGradient4\"",
  "       x1=\"1077.5879\"",
  "       y1=\"195.62577\"",
  "       x2=\"1077.5879\"",
  "       y2=\"456.02132\"",
  "       gradientTransform=\"matrix(1.4900517,0,0,0.90394906,-223.02955,-65.770703)\"",
  "       gradientUnits=\"userSpaceOnUse\" />",
  "    <linearGradient",
  "       xlink:href=\"#blueFill\"",
  "       id=\"linearGradient8\"",
  "       x1=\"598.26299\"",
  "       y1=\"139.74283\"",
  "       x2=\"598.26299\"",
  "       y2=\"357.0855\"",
  "       gradientTransform=\"matrix(1.5784673,0,0,0.51881714,-0.87277539,35.886344)\"",
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
  "  </defs>",
  "  <text",
  "     class=\"t title\"",
  "     x=\"900\"",
  "     y=\"62\"",
  "     text-anchor=\"middle\"",
  "     id=\"text9\">Virtual CYP-PK Lab: Simulation Model Schematic</text>",
  "  <!-- absorption depot -->",
  "  <rect",
  "     class=\"concept\"",
  "     x=\"40.199409\"",
  "     y=\"109.9249\"",
  "     width=\"254.55508\"",
  "     height=\"210.08301\"",
  "     id=\"rect10\"",
  "     style=\"fill:url(#linearGradient75)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"170\"",
  "     y=\"150\"",
  "     text-anchor=\"middle\"",
  "     id=\"text10\"><tspan",
  "       id=\"tspan1\"",
  "       x=\"170\"",
  "       y=\"150\">Post-i.p.</tspan><tspan",
  "       id=\"tspan4\"",
  "       x=\"170\"",
  "       y=\"185\">administration</tspan></text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"170\"",
  "     y=\"221\"",
  "     text-anchor=\"middle\"",
  "     id=\"text11\">absorption depot</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"170\"",
  "     y=\"262\"",
  "     text-anchor=\"middle\"",
  "     id=\"text12\">Adep</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"170\"",
  "     y=\"298\"",
  "     text-anchor=\"middle\"",
  "     id=\"text13\">First-order absorption k<tspan",
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
  "     id=\"text14\">Central (plasma)</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"642\"",
  "     y=\"203\"",
  "     text-anchor=\"middle\"",
  "     id=\"text15\">compartment</text>",
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
  "     x=\"944.86694\"",
  "     y=\"109.53293\"",
  "     width=\"340.26614\"",
  "     height=\"110.46988\"",
  "     id=\"rect18\"",
  "     style=\"fill:url(#linearGradient8)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"140\"",
  "     text-anchor=\"middle\"",
  "     id=\"text19\">Shallow peripheral</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"177\"",
  "     text-anchor=\"middle\"",
  "     id=\"text20\">compartment</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1115\"",
  "     y=\"210\"",
  "     text-anchor=\"middle\"",
  "     id=\"text21\">X2</text>",
  "  <rect",
  "     class=\"comp\"",
  "     x=\"944.84631\"",
  "     y=\"316.61215\"",
  "     width=\"340.30737\"",
  "     height=\"106.89084\"",
  "     id=\"rect21\"",
  "     style=\"fill:url(#linearGradient81)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"345\"",
  "     text-anchor=\"middle\"",
  "     id=\"text22\">Deep peripheral</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1115\"",
  "     y=\"382\"",
  "     text-anchor=\"middle\"",
  "     id=\"text23\">compartment</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1115\"",
  "     y=\"415\"",
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
  "     x=\"1384.2167\"",
  "     y=\"112.73103\"",
  "     width=\"384.83359\"",
  "     height=\"232.05228\"",
  "     id=\"rect28\"",
  "     style=\"fill:url(#linearGradient4)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1582\"",
  "     y=\"147\"",
  "     text-anchor=\"middle\"",
  "     id=\"text29\">Brain effect-site</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1582\"",
  "     y=\"184\"",
  "     text-anchor=\"middle\"",
  "     id=\"text30\">compartment</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1582\"",
  "     y=\"217\"",
  "     text-anchor=\"middle\"",
  "     id=\"text31\"><tspan",
  "       id=\"tspan13\"",
  "       x=\"1582\"",
  "       y=\"217\">Xe</tspan><tspan",
  "       id=\"tspan16\"",
  "       x=\"1582\"",
  "       y=\"247\">Ce = Xe / Ve</tspan></text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1582\"",
  "     y=\"277\"",
  "     text-anchor=\"middle\"",
  "     id=\"text32\"><tspan",
  "       id=\"tspan11\"",
  "       x=\"1582\"",
  "       y=\"277\">A virtual brain effect site that changes</tspan><tspan",
  "       x=\"1582\"",
  "       y=\"303.25\"",
  "       id=\"tspan14\">with a delay in response</tspan><tspan",
  "       x=\"1582\"",
  "       y=\"329.5\"",
  "       id=\"tspan15\">to changes in plasma</tspan></text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"1580\"",
  "     y=\"322\"",
  "     text-anchor=\"middle\"",
  "     id=\"text33\"",
  "     style=\"font-size:21px\" />",
  "  <!-- horizontal conceptual arrow to brain -->",
  "  <line",
  "     class=\"dash\"",
  "     x1=\"815\"",
  "     y1=\"272\"",
  "     x2=\"1375.2328\"",
  "     y2=\"272\"",
  "     id=\"line33\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1116\"",
  "     y=\"264\"",
  "     text-anchor=\"middle\"",
  "     id=\"text34\">Brain/effect-site concentration changes</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1116\"",
  "     y=\"290.25\"",
  "     text-anchor=\"middle\"",
  "     id=\"text34b\">with plasma concentration (Qe, Kp,e)</text>",
  "  <!-- intervention specific PK changes -->",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"40,635 5,550 40,465 385,465 420,550 385,635 \"",
  "     id=\"polygon35\"",
  "     style=\"fill:url(#linearGradient76)\"",
  "     transform=\"matrix(0.95848895,0,0,1.2656465,17.285596,-142.83393)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"212\"",
  "     y=\"485\"",
  "     text-anchor=\"middle\"",
  "     id=\"text36\"><tspan",
  "       id=\"tspan6\"",
  "       x=\"212\"",
  "       y=\"485\">Treatment-specific</tspan><tspan",
  "       id=\"tspan8\"",
  "       x=\"212\"",
  "       y=\"520\">metabolic PK changes</tspan></text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"554\"",
  "     text-anchor=\"middle\"",
  "     id=\"text37\">Control：Vmax ×1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"582\"",
  "     text-anchor=\"middle\"",
  "     id=\"text38\">　　　　Km ×1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"610\"",
  "     text-anchor=\"middle\"",
  "     id=\"text39\">PHB：Vmax ×2.011 / Km ×1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"212\"",
  "     y=\"638\"",
  "     text-anchor=\"middle\"",
  "     id=\"text40\">OME：Vmax ×1.000 / Km ×3.456</text>",
  "  <!-- MM elimination -->",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"460,550 500,450 825,450 865,550 825,650 500,650 \"",
  "     id=\"polygon40\"",
  "     style=\"fill:url(#linearGradient86)\"",
  "     transform=\"matrix(1.1682761,0,0,0.99883359,-45.281196,0.64152481)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"732\"",
  "     y=\"500\"",
  "     text-anchor=\"middle\"",
  "     id=\"text41\">Michaelis-Menten elimination</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"732\"",
  "     y=\"548\"",
  "     text-anchor=\"middle\"",
  "     id=\"text42\">rate = Vmax · X1 / (Km + X1)</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"732\"",
  "     y=\"594\"",
  "     text-anchor=\"middle\"",
  "     id=\"text43\">Metabolic elimination from</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"732\"",
  "     y=\"624\"",
  "     text-anchor=\"middle\"",
  "     id=\"text44\">the central compartment</text>",
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
  "     transform=\"matrix(1.0363053,0,0,0.99978848,-75.014858,-0.7387156)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1478\"",
  "     y=\"468\"",
  "     text-anchor=\"middle\"",
  "     id=\"text45\">Loss of Righting Reflex (LRR) criterion</text>",
  "  <text",
  "     class=\"t m\"",
  "     x=\"1478\"",
  "     y=\"507\"",
  "     text-anchor=\"middle\"",
  "     id=\"text46\">Ce ≥ C<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan19\">RR,thr</tspan> × M<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan20\">RR</tspan></text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1478\"",
  "     y=\"540\"",
  "     text-anchor=\"middle\"",
  "     id=\"text47\">Control：M<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan21\">RR</tspan> = 1.000</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1478\"",
  "     y=\"572\"",
  "     text-anchor=\"middle\"",
  "     id=\"text48\">PHB：1.070</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1478\"",
  "     y=\"604\"",
  "     text-anchor=\"middle\"",
  "     id=\"text49\">OME：1.036</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"1478\"",
  "     y=\"630\"",
  "     text-anchor=\"middle\"",
  "     id=\"text50\"",
  "     style=\"font-size:21px\">Residual threshold adjustment fitted</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"1478\"",
  "     y=\"654\"",
  "     text-anchor=\"middle\"",
  "     id=\"text51\"",
  "     style=\"font-size:21px\">after PK fitting to reproduce behavioral differences</text>",
  "  <line",
  "     class=\"dash\"",
  "     x1=\"1572\"",
  "     y1=\"345\"",
  "     x2=\"1572\"",
  "     y2=\"427\"",
  "     id=\"line51\" />",
  "  <!-- outputs -->",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"830,720 795,850 435,850 470,720 \"",
  "     id=\"polygon51\"",
  "     style=\"fill:url(#linearGradient79)\"",
  "     transform=\"matrix(1.1553491,0,0,0.99817645,-3.423318,1.4314902)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"730\"",
  "     y=\"770\"",
  "     text-anchor=\"middle\"",
  "     id=\"text52\">Plasma PTB concentration</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"730\"",
  "     y=\"815\"",
  "     text-anchor=\"middle\"",
  "     id=\"text53\">Cplasma at blood-sampling time</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"662\"",
  "     y=\"845\"",
  "     text-anchor=\"middle\"",
  "     id=\"text54\" />",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"728\"",
  "     y1=\"650\"",
  "     x2=\"728\"",
  "     y2=\"717\"",
  "     id=\"line54\" />",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"40,720 395,720 360,850 5,850 \"",
  "     id=\"polygon54\"",
  "     style=\"fill:url(#linearGradient80)\"",
  "     transform=\"matrix(1.162733,0,0,1.2146163,23.497566,-154.2655)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"260\"",
  "     y=\"763\"",
  "     text-anchor=\"middle\"",
  "     id=\"text55\">Terminal liver virtual ex vivo</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"260\"",
  "     y=\"800\"",
  "     text-anchor=\"middle\"",
  "     id=\"text56\">PTB metabolic activity</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"260\"",
  "     y=\"834\"",
  "     text-anchor=\"middle\"",
  "     id=\"text57\"",
  "     style=\"font-size:21px\">Calculated using treatment-specific</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"260\"",
  "     y=\"858\"",
  "     text-anchor=\"middle\"",
  "     id=\"text58\"",
  "     style=\"font-size:21px\">Vmax and Km values</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"224\"",
  "     y1=\"660.65131\"",
  "     x2=\"224\"",
  "     y2=\"717.9176\"",
  "     id=\"line58\" />",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"1740,720 1705,850 1225,850 1260,720 \"",
  "     id=\"polygon58\"",
  "     style=\"fill:url(#linearGradient87)\"",
  "     transform=\"matrix(0.92750309,0,0,1.0009007,74.21948,-0.70705017)\" />",
  "  <polygon",
  "     class=\"out\"",
  "     points=\"1225,850 1260,720 1740,720 1705,850 \"",
  "     id=\"polygon58-4\"",
  "     style=\"fill:url(#linearGradient2);stroke:#159447;stroke-width:2.8\"",
  "     transform=\"matrix(1.2213988,0,0,1.2735551,-340.22819,-14.346003)\" />",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1448\"",
  "     y=\"764\"",
  "     text-anchor=\"middle\"",
  "     id=\"text59\">Brain-related outputs</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1448\"",
  "     y=\"798\"",
  "     text-anchor=\"middle\"",
  "     id=\"text60\">LRR duration</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1448\"",
  "     y=\"828\"",
  "     text-anchor=\"middle\"",
  "     id=\"text61\">+ terminal brain PTB concentration</text>",
  "  <line",
  "     class=\"solid\"",
  "     x1=\"1448\"",
  "     y1=\"670\"",
  "     x2=\"1448\"",
  "     y2=\"717\"",
  "     id=\"line61\" />",
  "  <!-- notes row -->",
  "  <rect",
  "     class=\"note\"",
  "     x=\"34.760632\"",
  "     y=\"899.76062\"",
  "     width=\"561.56366\"",
  "     height=\"168.43463\"",
  "     id=\"rect61\" />",
  "  <rect",
  "     class=\"note\"",
  "     x=\"619.91357\"",
  "     y=\"900.31763\"",
  "     width=\"527.08472\"",
  "     height=\"168.50725\"",
  "     id=\"rect61-9\"",
  "     style=\"fill:#ffffff;stroke:#4b5563;stroke-width:1.69843\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"661.78076\"",
  "     y=\"965.98291\"",
  "     id=\"text63-2\"",
  "     style=\"font-size:21px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\"><tspan",
  "       id=\"tspan2\"",
  "       x=\"661.78076\"",
  "       y=\"965.98291\">Interindividual variability (log-normal):</tspan><tspan",
  "       x=\"661.78076\"",
  "       y=\"994.41962\"",
  "       id=\"tspan3\">BW 5%、F<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan23\">eff</tspan> 10%、ka 10%、Vmax 15% CV</tspan><tspan",
  "       x=\"661.78076\"",
  "       y=\"1020.6696\"",
  "       id=\"tspan5\">Measurement error (log-normal):</tspan><tspan",
  "       x=\"661.78076\"",
  "       y=\"1046.9196\"",
  "       id=\"tspan7\">Blood 6%, brain 6%, liver virtual assay 10% CV</tspan></text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"660.1463\"",
  "     y=\"933.70123\"",
  "     id=\"text62-0\"",
  "     style=\"font-weight:700;font-size:28px;font-family:Arial, 'Yu Gothic', 'Noto Sans JP', sans-serif;fill:#111827\">Variability and measurement error</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"60.289913\"",
  "     y=\"930.14496\"",
  "     id=\"text62\"><tspan",
  "       id=\"tspan9\"",
  "       x=\"60.289913\"",
  "       y=\"930.14496\">Common model structure</tspan><tspan",
  "       id=\"tspan10\"",
  "       x=\"60.289913\"",
  "       y=\"965.14496\">and parameters across groups</tspan></text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"62\"",
  "     y=\"994\"",
  "     id=\"text63\">F_eff, ka, k12, k21, k13, k31, Vp, Qe, Kp,e, Ve, and</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"62\"",
  "     y=\"1026\"",
  "     id=\"text64\">reference Vmax / Km / C<tspan",
  "   style=\"font-size:17.3333px\"",
  "   id=\"tspan22\">RR,thr</tspan>.</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"62\"",
  "     y=\"1058\"",
  "     id=\"text65\"",
  "     style=\"font-size:21px\">Treatment effects use the relative changes above.</text>",
  "  <text",
  "     class=\"t h\"",
  "     x=\"1222\"",
  "     y=\"937\"",
  "     id=\"text66\">Liver virtual ex vivo assay conditions</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1188\"",
  "     y=\"979\"",
  "     id=\"text67\">Fixed substrate concentration: Cstd = 56.6 µg/mL (250 µM)</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1188\"",
  "     y=\"1012\"",
  "     id=\"text68\">Metabolic rate is calculated using the Vmax and Km</text>",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1188\"",
  "     y=\"1042\"",
  "     id=\"text69\">assigned to each group under the same substrate condition</text>",
  "  <!-- legend -->",
  "  <rect",
  "     class=\"note\"",
  "     x=\"34.916622\"",
  "     y=\"1089.9166\"",
  "     width=\"1742.0958\"",
  "     height=\"130.98248\"",
  "     id=\"rect69\" />",
  "  <rect",
  "     class=\"comp\"",
  "     x=\"67\"",
  "     y=\"1125\"",
  "     width=\"95\"",
  "     height=\"56\"",
  "     id=\"rect70\"",
  "     style=\"fill:url(#linearGradient85)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"187\"",
  "     y=\"1146\"",
  "     id=\"text70\">Systemic compartment</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"187\"",
  "     y=\"1175\"",
  "     id=\"text71\">Included in systemic mass balance</text>",
  "  <rect",
  "     class=\"concept\"",
  "     x=\"508\"",
  "     y=\"1125\"",
  "     width=\"95\"",
  "     height=\"56\"",
  "     id=\"rect71\"",
  "     style=\"fill:url(#linearGradient82)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"628\"",
  "     y=\"1146\"",
  "     id=\"text72\">Conceptual / driven compartment</text>",
  "  <text",
  "     class=\"t xs\"",
  "     x=\"628\"",
  "     y=\"1175\"",
  "     id=\"text73\">Not included in systemic mass balance</text>",
  "  <polygon",
  "     class=\"process\"",
  "     points=\"1055,1153 1070,1125 1165,1125 1180,1153 1165,1181 1070,1181 \"",
  "     id=\"polygon73\"",
  "     style=\"fill:url(#linearGradient83)\"",
  "     transform=\"translate(-66)\" />",
  "  <text",
  "     class=\"t s\"",
  "     x=\"1139\"",
  "     y=\"1162\"",
  "     id=\"text74\">Process / decision rule</text>",
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
  "     id=\"text75\">Observed / derived output</text>",
  "  <text",
  "     xml:space=\"preserve\"",
  "     id=\"text76\"",
  "     style=\"fill:#000000;font-size:21px;-inkscape-font-specification:'Arial, Normal';font-family:Arial;white-space:pre;shape-inside:url(#rect76)\" />",
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
    "1. Experimental setup",
    fluidPage(
      br(),
      div(class = "app-subtitle", "A virtual pharmacology lab for learning how induction or inhibition of hepatic microsomal oxidative drug metabolism affects pentobarbital PK and hypnosis duration."),
      div(class = "reference-box app-lead",
          strong("What you will do in this lab"),
          p("In this virtual lab, pentobarbital is administered intraperitoneally to rats, and the effects of induction or inhibition of hepatic microsomal oxidative drug metabolism on pentobarbital PK and hypnosis duration are compared using plasma concentrations and the righting reflex."),
          p(HTML("<b>The righting reflex</b> is the reflex by which an animal placed on its back returns by itself to a normal posture. In this lab, loss of the righting reflex is used as the indicator of hypnosis.")),
          p("The experiment includes three groups: Control, phenobarbital pretreatment (PHB), and omeprazole pretreatment (OME)."),
          p("If terminal tissue collection is performed, you can review the brain pentobarbital concentration and a virtual ex vivo pentobarbital metabolic assay that mimics an experiment using excised liver tissue.")),
      experiment_visual_guide_ja(),
      div(class = "note-box app-lead",
          strong("Suggested workflow"),
          p('Assign three rats to each group. After pentobarbital dosing, check the righting reflex over time. Plasma concentrations should be measured by selecting "Righting-reflex check + blood sampling" at approximately 15, 30, 60, and 120 min after dosing. Additional measurements may be added if needed.'),
          p("The experiment log records the actual post-dose time for each operation. When comparing groups, compare data obtained at post-dose times that are as similar as possible."),
          p("No additional operations can be performed on a rat after terminal tissue collection. If you need both brain and liver information, plan blood sampling and righting-reflex checks first and perform tissue collection at the end. Terminal tissue collection does not advance the virtual clock."),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(tags$th("Action"), tags$th("Virtual time advanced"))),
            tags$tbody(
              tags$tr(tags$td("Pentobarbital dosing"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$dose))),
              tags$tr(tags$td("Righting-reflex check"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$rr_check))),
              tags$tr(tags$td("Righting-reflex check + blood sampling"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$rr_blood))),
              tags$tr(tags$td("Terminal tissue collection (liver + brain; includes hepatic virtual ex vivo assay)"), tags$td(sprintf("%.2f min / rat", EXPERIMENT_ACTION_MINUTES$terminal_tissues))),
              tags$tr(tags$td("Wait"), tags$td("Advances by the entered waiting time"))
            )
          )),
      div(class = "reference-box app-lead",
          strong("Reference pretreatment conditions"),
          tags$ul(
            tags$li(HTML("<b>Control:</b> No pretreatment affecting oxidative drug metabolism.")),
            tags$li(HTML("<b>Phenobarbital / PHB:</b> Means et al. (1978). The main basis is PHB 10 mg/kg i.p., followed 24 h later by pentobarbital 50 mg/kg i.p. PMID: 634995")),
            tags$li(HTML("<b>Omeprazole / OME:</b> Henry et al. (1986). The main basis is omeprazole 40 mg/kg i.v. administered 30 min before pentobarbital, followed by pentobarbital 45 mg/kg i.p. PMID: 3742882"))
          ),
          p(HTML("<b>Note:</b> OME 40 mg/kg i.v. is a high-dose rat experimental condition used to evaluate drug interactions. It does not represent a clinical dose.")),
          p("These are the literature conditions used as the basis for modeling. In the actual simulated experiment, the pentobarbital dose set on the screen is used in common across all three groups.")),
      fluidRow(
        column(
          8,
          div(class = "note-box",
              strong("How to assign rats:"),
              tags$ol(
                tags$li("Select three rats each for Control, phenobarbital (PHB), and omeprazole (OME)."),
                tags$li("The same rat cannot be assigned to more than one group."),
                tags$li('If you prefer, click "Randomly assign 9 rats".'),
                tags$li('When the assignment check turns green, click "Start experiment with this assignment".')
              )),
          actionButton("random_allocate", "Randomly assign 9 rats", class = "btn-default"),
          br(), br(),
          fluidRow(
            column(4, selectizeInput("control_rats", "Control (3 rats)",
                                     choices = rat_bank$RatID, selected = character(0),
                                     multiple = TRUE, options = list(maxItems = 3))),
            column(4, selectizeInput("phb_rats", "Phenobarbital / PHB (3 rats)",
                                     choices = rat_bank$RatID, selected = character(0),
                                     multiple = TRUE, options = list(maxItems = 3))),
            column(4, selectizeInput("ome_rats", "Omeprazole / OME (3 rats)",
                                     choices = rat_bank$RatID, selected = character(0),
                                     multiple = TRUE, options = list(maxItems = 3)))
          ),
          fluidRow(
            column(6, numericInput("dose_mgkg", "Pentobarbital dose (mg/kg, i.p.)", value = 50, min = 20, max = 60, step = 1)),
            column(6, br(), actionButton("start_experiment", "Start experiment with this assignment", class = "btn-primary", width = "100%"))
          ),
          br(),
          uiOutput("setup_validation"),
          div(class = "panel-title-strong", "List of 30 rats (information visible before the experiment)"),
          tableOutput("allocation_table")
        ),
        column(
          4,
          div(class = "action-box",
              div(class = "panel-title-strong", "Restore a saved experiment"),
              p("Upload a previously saved session file (.rds) to restore the same rats, assignment, operation log, and measurement results, and directly open the model-internal-information screen."),
              fileInput("session_file", "Session replay file (.rds)", accept = c(".rds"))),
          div(class = "action-box",
              div(class = "panel-title-strong", "Virtual rat population"),
              p("The 30 rats are fixed individuals, and each has different pharmacokinetics."),
              p("During the experiment, individual parameters other than body weight are hidden."),
              p("The individual parameters hidden during the experiment are explained together with their meanings in the post-experiment model-internal-information screen.")),
        )
      )
    )
  ),

  tabPanel(
    "2. Experimental operations",
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
                    radioButtons("active_rat", "Rat to operate on (showing the 9 assigned rats)",
                                 choices = character(0), selected = character(0))),
                actionButton("dose_btn", sprintf("Pentobarbital dosing (%.1f min)", EXPERIMENT_ACTION_MINUTES$dose), width = "100%"), br(), br(),
                actionButton("lrr_btn", sprintf("Check righting reflex (%.1f min)", EXPERIMENT_ACTION_MINUTES$rr_check), width = "100%"), br(), br(),
                actionButton("lrr_blood_btn", sprintf("RR check + blood sampling (%.1f min)", EXPERIMENT_ACTION_MINUTES$rr_blood), width = "100%"),
                tags$small("* During blood sampling, the righting reflex is also recorded at the same operation time point."), br(), br(),
                div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:8px;",
                    actionButton("wait_05_btn", "+0.5 min", class = "btn-default"),
                    actionButton("wait_1_btn", "+1 min", class = "btn-default"),
                    actionButton("wait_5_btn", "+5 min", class = "btn-default"),
                    actionButton("wait_10_btn", "+10 min", class = "btn-default")),
                numericInput("wait_min", "Custom wait time (min)", value = 5, min = 0, step = 0.5),
                actionButton("wait_btn", "Wait for the specified time", width = "100%"),
                tags$hr(),
                actionButton(
                  "tissue_btn",
                  HTML(sprintf("Terminal tissue collection<br>(liver + brain) (%.1f min)", EXPERIMENT_ACTION_MINUTES$terminal_tissues)),
                  width = "100%", class = "btn-warning",
                  style = "white-space:normal;height:auto;min-height:48px;line-height:1.25;padding:7px 10px;"
                ),
                br(), br(),
                actionButton(
                  "finish_experiment_btn",
                  HTML("Finish experiment<br>and disclose model internals"),
                  width = "100%", class = "btn-success",
                  style = "white-space:normal;height:auto;min-height:48px;line-height:1.25;padding:7px 10px;"
                ),
                tags$small("* Clicking this ends data collection, and no further experimental operations can be performed. The app then moves to the model-internal-information screen.")),
            div(class = "student-result", uiOutput("action_result"))
          ),
          column(
            9,
            div(class = "panel-title-strong", "Current rat status"),
            tableOutput("status_table"),
            tags$hr(),
            div(class = "panel-title-strong", "Most recent blood-sampling results"),
            tableOutput("blood_table"),
            tags$hr(),
            div(class = "panel-title-strong", "Righting-reflex check history"),
            tableOutput("lrr_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "3. Experiment log",
    fluidPage(
      br(),
      uiOutput("log_gate"),
      tableOutput("event_log_table"),
      br(),
      downloadButton("download_logs", "Save measurement data and experiment log as CSV ZIP"),
      tags$span("　"),
      downloadButton("download_session", "Save reproducible session file")
    )
  ),

  tabPanel(
    "4. Disclosure of model internals",
    fluidPage(
      br(),
      uiOutput("debrief_gate"),
      conditionalPanel(
        condition = "output.experimentStarted == true",
        conditionalPanel(
          condition = "output.debriefRevealed == false",
          div(class = "note-box",
              'Click "Finish experiment and disclose model internals" in the Experimental operations tab to display model internal information that was hidden during the experiment.')
        ),
        conditionalPanel(
          condition = "output.debriefRevealed == true",
          div(class = "debrief-box",
              strong("Disclosure of model internal information (not experimental results):"),
              "What is shown here is not the measurement results directly obtained by the student performing the simulated experiment, but the individual parameters, true concentration profiles, and true righting-reflex transition times that were set and calculated internally in the simulation. Interpret them by comparing them with your own experiment log and measured values."),
          div(class = "reference-box",
              strong("Model-relative changes used for each treatment group:"),
              p(sprintf("PHB：Vmax ×%.3f、Km ×1.000、M_RR ×%.3f。OME：Vmax ×1.000、Km ×%.3f、M_RR ×%.3f。",
                        MODEL_DEFAULTS$treatment$PHB$vmax_mult,
                        MODEL_DEFAULTS$treatment$PHB$M_RR,
                        MODEL_DEFAULTS$treatment$OME$km_mult,
                        MODEL_DEFAULTS$treatment$OME$M_RR)),
              p("The relative changes in Vmax/Km are an operational translation used to reproduce the literature-observed pentobarbital PK changes within the fixed-structure model. M_RR is a residual threshold multiplier obtained after PK fitting to reproduce behavioral differences, and it cannot be interpreted alone as a pure PD parameter.")),
          div(class = "note-box",
              strong("Terms:"),
              "PK = Pharmacokinetics, PD = Pharmacodynamics, ",
              "times of loss and recovery of the righting reflex, ",
              "CYP = Cytochrome P450, ",
              "IIV = Interindividual Variability."),
          fluidRow(
            column(5,
                   selectInput("debrief_rat", "Rat to inspect", choices = NULL),
                   tableOutput("latent_table"),
                   tableOutput("true_transition_table"),
                   downloadButton("download_debrief_plot", "Save this rat's model-internal plot as PNG")),
            column(7, plotOutput("trajectory_plot", height = "500px"))
          ),
          tags$hr(),
          div(class = "panel-title-strong", "Group comparison (true internal values; not measured experimental results)"),
          plotOutput("group_comparison_plot", height = "720px"),
          tags$hr(),
          div(class = "panel-title-strong", "Model internal information for all used rats"),
          tableOutput("all_latent_table"),
          downloadButton("download_debrief_summary", "Save all-rat model internal information as CSV"),
          tags$span("　"),
          downloadButton("download_debrief_all", "Save all model internal information (tables + all figures) as ZIP")
        )
      )
    )
  ),

  tabPanel(
    "5. Reference information",
    fluidPage(
      br(),
      div(
        style = "width:100%; text-align:center; margin:0 auto 16px auto;",
        tags$div(
          HTML(MODEL_SCHEMATIC_SVG),
          style = "width:100%; max-width:1800px; margin:0 auto; overflow:hidden;"
        )
      ),
      div(class = "app-subtitle", "Reference information for understanding the model and experiment"),

      div(class = "reference-box app-lead",
          strong("Model structure used in this app"),
          p("The systemic pharmacokinetics of pentobarbital are represented by a 3-compartment open model based on Hatanaka et al. (1988), with an additional first-order absorption depot for intraperitoneal dosing. Elimination from the central compartment is described by Michaelis–Menten-type saturable metabolism."),
          div(style = "margin: 8px 0 10px 18px; padding: 8px 12px; background: #f8f9fa; border-left: 4px solid #6c757d; font-size: 15px;",
              HTML(
                "<b>Intraperitoneal absorption:</b> dA<sub>dep</sub>/dt = −k<sub>a</sub>A<sub>dep</sub><br>",
                "<b>Central compartment:</b> dX<sub>1</sub>/dt = S<sub>IP</sub>k<sub>a</sub>A<sub>dep</sub> − (k<sub>12</sub>+k<sub>13</sub>)X<sub>1</sub> + k<sub>21</sub>X<sub>2</sub> + k<sub>31</sub>X<sub>3</sub> − V<sub>max</sub>X<sub>1</sub>/(K<sub>m</sub>+X<sub>1</sub>)<br>",
                "<b>Peripheral compartment:</b> dX<sub>2</sub>/dt = k<sub>12</sub>X<sub>1</sub> − k<sub>21</sub>X<sub>2</sub><br>",
                "<b>Deep compartment:</b> dX<sub>3</sub>/dt = k<sub>13</sub>X<sub>1</sub> − k<sub>31</sub>X<sub>3</sub><br>",
                "<b>Plasma concentration:</b> C<sub>plasma</sub> = X<sub>1</sub>/V<sub>p</sub>"
              )),
          p("Adep (absorption depot) is the absorption depot representing the amount of drug remaining after intraperitoneal dosing that has not yet entered the systemic circulation. Drug moves from this depot to the central compartment according to ka. Because Adep is not counted as a systemic distribution compartment, the systemic distribution part remains a 3-compartment model consisting of X1–X3."),
          p("In Hatanaka et al. (1988), rat pentobarbital plasma concentration profiles were described by a 3-compartment open model with Michaelis–Menten elimination. This app uses that structure and source parameters as the basis, while adding intraperitoneal dosing and educational interindividual differences.")),

      div(class = "reference-box app-lead",
          strong('Virtual "brain concentration" (Ce) and the righting reflex'),
          p("Brain-related behavior is represented using a latent effect-site state Xe that follows changes in plasma concentration with a delay. Because Xe is not included in the systemic mass balance, it is treated as a conceptual state separate from the systemic three-compartment model consisting of X1–X3."),
          div(style = "margin: 8px 0 10px 18px; padding: 8px 12px; background: #f8f9fa; border-left: 4px solid #6c757d; font-size: 15px;",
              HTML(
                "<b>Latent effect site:</b> X<sub>e</sub> is a brain-related latent effect-site state that follows changes in plasma concentration with a delay<br>",
                '<b>Effect-site concentration:</b> C<sub>e</sub> is the latent effect-site concentration calculated as X<sub>e</sub> / V<sub>e</sub>, displayed in the app as the virtual "brain concentration."<br>',
                "<b>Righting reflex:</b> C<sub>e</sub> ≥ C<sub>RR,thr</sub> × M<sub>RR</sub> is treated as the loss of righting reflex (LRR) / hypnosis state when Ce reaches or exceeds this threshold"
              )),
          p("The reference C_RR,thr is shared across the reference condition, and in treatment groups the effective threshold is expressed as C_RR,thr × M_RR using M_RR derived from residual threshold analysis after PK fitting. This app does not add an independent dynamic PD model; instead, the righting reflex is judged from the relation between Ce and the treatment-specific effective threshold. M_RR cannot be interpreted alone as a pure PD parameter.")),

      div(class = "reference-box app-lead",
          strong("Abbreviations, symbols, and model terms"),
          p("The main abbreviations and symbols are summarized below. You do not need to memorize the numerical values themselves."),
          p(HTML('<b>Important:</b> The value displayed in the UI as "brain concentration" is the latent effect-site concentration (C<sub>e</sub>). Q<sub>e</sub>, V<sub>e</sub>, and K<sub>p,e</sub> are parameters that define the latent effect site.')),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(tags$th("Notation"), tags$th("Formal name / full spelling (or meaning for nonstandard symbols)"), tags$th("Meaning in this app"))),
            tags$tbody(
              tags$tr(tags$td("PK"), tags$td("Pharmacokinetics"), tags$td("The process by which a drug is absorbed, distributed, metabolized, and eliminated, causing its concentration to change over time.")),
              tags$tr(tags$td("PD"), tags$td("Pharmacodynamics"), tags$td("The relationship between drug concentration and effect. In this app, no independent dynamic PD model is used; effects are judged using the virtual brain concentration (Ce) and the righting-reflex threshold.")),
              tags$tr(tags$td("i.p."), tags$td("intraperitoneal"), tags$td("A route of administration in which the solution is administered into the peritoneal cavity.")),
              tags$tr(tags$td("BW"), tags$td("Body weight"), tags$td("The body weight of the rat. It is also used for body-size scaling of model parameters.")),
              tags$tr(tags$td("CYP / P450"), tags$td("Cytochrome P450"), tags$td("A family of enzymes involved in the metabolism of many drugs, especially in the liver.")),
              tags$tr(tags$td("PHB"), tags$td("Phenobarbital"), tags$td("In this app, it is treated as a pretreatment that induces hepatic drug-metabolizing capacity.")),
              tags$tr(tags$td("OME"), tags$td("Omeprazole"), tags$td("Under high-dose rat conditions, inhibition of oxidative drug metabolism has been reported; in this app, the decrease in clearance reported by Henry et al. (1986) is operationally represented as an increase in Km.")),
              tags$tr(tags$td("LRR"), tags$td("Loss of Righting Reflex"), tags$td("A state in which an animal placed on its back cannot return by itself to a normal posture. In this app, it is the behavioral marker for the onset side of hypnosis.")),
              tags$tr(tags$td("RORR"), tags$td("Return of Righting Reflex"), tags$td("The state in which the righting reflex is again observed. In this app, it is the behavioral marker for the recovery side of hypnosis.")),
              tags$tr(tags$td("Adep"), tags$td("Absorption depot"), tags$td("A state representing the amount of pentobarbital not yet absorbed into the systemic circulation after intraperitoneal dosing. It is not counted as a systemic distribution compartment.")),
              tags$tr(tags$td("X1"), tags$td("Central compartment amount"), tags$td("The amount of drug in the central compartment, mainly representing the plasma side.")),
              tags$tr(tags$td("X2"), tags$td("Shallow peripheral compartment amount"), tags$td("The amount of drug in the peripheral side that exchanges relatively quickly with the central compartment.")),
              tags$tr(tags$td("X3"), tags$td("Deep peripheral compartment amount"), tags$td("The amount of drug that exchanges more slowly with the central compartment.")),
              tags$tr(tags$td("Xe"), tags$td("Latent effect-site state"), tags$td("A brain-related latent effect-site state that follows plasma concentrations with a delay. It is not included in the mass balance of systemic X1–X3.")),
              tags$tr(tags$td("Ve"), tags$td("Effect-site distribution volume"), tags$td("The apparent volume used to calculate Ce from Xe.")),
              tags$tr(tags$td("F_eff"), tags$td("Effective systemic input fraction"), tags$td("A dimensionless model coefficient representing the effective input reaching the systemic circulation from the intraperitoneal depot. It is not a directly measured standard bioavailability value.")),
              tags$tr(tags$td("ka"), tags$td("First-order absorption rate constant"), tags$td("Represents the rate at which drug is absorbed from the intraperitoneal depot. Larger values indicate faster absorption.")),
              tags$tr(tags$td("k12 / k21"), tags$td("Inter-compartmental rate constants"), tags$td("The drug transfer rate between X1 and X2. k12 is central → peripheral, and k21 is peripheral → central.")),
              tags$tr(tags$td("k13 / k31"), tags$td("Inter-compartmental rate constants"), tags$td("The drug transfer rate between X1 and X3. k13 is central → deep, and k31 is deep → central.")),
              tags$tr(tags$td("Vmax"), tags$td("Maximum metabolic rate"), tags$td("The maximum metabolic rate when Michaelis–Menten metabolism is saturated. In this app, the PK change caused by PHB treatment is represented as a relative increase in Vmax.")),
              tags$tr(tags$td("Km"), tags$td("Michaelis constant"), tags$td("A constant describing how readily Michaelis–Menten metabolism becomes saturated. In the systemic PK model it is a mass-based parameter, and in this app the PK change caused by OME treatment is represented as a relative increase in Km.")),
              tags$tr(tags$td("Vp"), tags$td("Central / plasma distribution volume"), tags$td("The apparent volume used to calculate plasma concentration Cplasma from X1.")),
              tags$tr(tags$td("Qe"), tags$td("Effect-site equilibration parameter"), tags$td("A model parameter that determines the equilibration rate of the latent effect site following plasma concentration.")),
              tags$tr(tags$td("Kp,e"), tags$td("Effect-site-to-plasma equilibrium ratio"), tags$td("A coefficient that determines the equilibrium concentration ratio between the latent effect site and plasma.")),
              tags$tr(tags$td("Cplasma"), tags$td("Plasma concentration"), tags$td("The plasma concentration calculated from the central compartment.")),
              tags$tr(tags$td("Ce"), tags$td("Effect-site concentration"), tags$td("The latent effect-site concentration calculated by dividing Xe by Ve. It is displayed in the app as the virtual brain concentration.")),
              tags$tr(tags$td("C_RR,thr"), tags$td("Reference RR threshold"), tags$td("The reference Ce threshold used in common for judging LRR and RORR. The treatment-specific effective threshold is C_RR,thr × M_RR.")),
              tags$tr(tags$td("M_RR"), tags$td("Residual threshold multiplier"), tags$td("A residual threshold multiplier derived after PK fitting to reproduce literature hypnosis-duration ratios. It defines the treatment-specific effective threshold as C_RR,thr × M_RR and cannot be interpreted alone as a pure PD parameter.")),
              tags$tr(tags$td("IIV"), tags$td("Interindividual variability"), tags$td("Differences among individual rats. In this app, log-normal interindividual variability is applied to BW, F_eff, ka, and Vmax.")),
              tags$tr(tags$td("CV"), tags$td("Coefficient of variation"), tags$td("An index expressing variability as a proportion of the mean. In this app, it is used to specify the magnitude of interindividual variability and measurement error.")),
              tags$tr(tags$td("Cstd"), tags$td("Standard substrate concentration"), tags$td("The pentobarbital substrate concentration applied in common to all rats in the hepatic virtual ex vivo assay.")),
              tags$tr(tags$td("Km,C"), tags$td("Concentration-form Michaelis constant"), tags$td("The concentration-based Km used in calculations for the hepatic virtual ex vivo assay. It has a different reference value from the mass-based Km in the systemic PK model, but the same treatment-related relative change in Km is applied.")),
              tags$tr(tags$td("v_assay"), tags$td("Virtual assay metabolic rate"), tags$td("The hepatic pentobarbital metabolic rate calculated under standardized substrate conditions. It is not the measured microsomal specific activity itself.")),
              tags$tr(tags$td("ex vivo"), tags$td("ex vivo"), tags$td("An experiment performed using tissue removed from the body. The hepatic assay in this app is a virtual assay modeled on that concept.")),
              tags$tr(tags$td("Michaelis–Menten"), tags$td("Michaelis–Menten kinetics"), tags$td("A relationship in which metabolic rate approaches Vmax and saturates as substrate concentration increases."))
            )
          )),

      div(class = "reference-box app-lead",
          strong("Treatment groups and modeling rationale"),
          tags$ul(
            tags$li("Control: the reference model including absorption, distribution, metabolism, brain transition, and the righting-reflex threshold."),
            tags$li("PHB: mainly based on the condition in Means et al. (1978), PHB 10 mg/kg i.p. followed 24 h later by PTB 50 mg/kg i.p., representing increased pentobarbital metabolic capacity due to induction of hepatic microsomal oxidative drug metabolism."),
            tags$li("OME: mainly based on the condition in Henry et al. (1986), omeprazole 40 mg/kg i.v. followed 30 min later by PTB 45 mg/kg i.p., representing reduced pentobarbital metabolic capacity under high-dose rat conditions due to inhibition of oxidative drug metabolism.")
          ),
          p("F_eff, ka, the distribution parameters, the effect-site parameters, and the reference Vmax, Km, and C_RR,thr are shared. Treatment effects are reflected in PK by relatively increasing Vmax for PHB and relatively increasing Km for OME, and M_RR derived after PK fitting is additionally reflected in the LRR threshold."),
          p(sprintf("Current implemented values: PHB uses Vmax ×%.3f, Km ×1.000, M_RR ×%.3f; OME uses Vmax ×1.000, Km ×%.3f, M_RR ×%.3f. The PK change for OME is a CL-primary, Km-only translation derived using the published clearance ratio in Henry et al. (1986) (3.7/5.3) as the primary target.",
                    MODEL_DEFAULTS$treatment$PHB$vmax_mult,
                    MODEL_DEFAULTS$treatment$PHB$M_RR,
                    MODEL_DEFAULTS$treatment$OME$km_mult,
                    MODEL_DEFAULTS$treatment$OME$M_RR))),

      div(class = "reference-box app-lead",
          strong("Hepatic virtual ex vivo pentobarbital metabolic assay"),
          p(HTML("As a virtual ex vivo assay using a pentobarbital substrate concentration common to all rats, C<sub>std</sub> = 56.6 µg/mL (250 µM),")),
          div(style = "margin: 8px 0 10px 18px; padding: 8px 12px; background: #f8f9fa; border-left: 4px solid #6c757d; font-size: 15px;",
              HTML("<b>v<sub>assay</sub> = V<sub>max,treat</sub> × C<sub>std</sub> / (K<sub>m,C,treat</sub> + C<sub>std</sub>)</b><br>",
                   "C<sub>std</sub> = 56.6 µg/mL (250 µM; common to all groups)<br>",
                   "Reference V<sub>max</sub> = 2.19 mg/h (300 g reference model)<br>",
                   "Reference K<sub>m,C</sub> = 5.34 µg/mL (concentration-based reference value for the virtual ex vivo assay)<br>",
                   "V<sub>max,treat</sub>: reference Vmax with body-weight scaling, Vmax interindividual variability, and treatment-related relative Vmax change applied<br>",
                   "K<sub>m,C,treat</sub>: reference Km,C with the treatment-related relative Km change applied")),
          p("The metabolic rate is calculated in this way. The plasma pentobarbital concentration at the time of dissection is not used. Vmax starts from the systemic PK reference-model value of 2.19 mg/h and uses the rat-specific operative Vmax incorporating body-weight scaling, interindividual variability, and treatment-related relative changes in Vmax. In contrast, Km,C uses the concentration-based reference value of 5.34 µg/mL for the virtual ex vivo assay, together with the same treatment-related relative change in Km adopted in the systemic PK model. Thus, Vmax ×2.011 / Km ×1.000 for PHB and Vmax ×1.000 / Km ×3.456 for OME are also reflected in the virtual assay. The systemic PK-model Km is mass-based and is not numerically identical to Km,C. The substrate condition of 250 µM corresponds to the in vitro assay condition in Kuntzman et al. (1967). The output is mg/h derived from the current PK model and is not the measured microsomal specific activity itself.")),

      div(class = "reference-box app-lead",
          strong("Interindividual variability (IIV) and measurement error"),
          p("Only the overview is shown in the schematic. The implemented sources of variability are listed below. Both interindividual variability and measurement error are applied as multiplicative log-normal variability/error."),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(
              tags$th("Type"), tags$th("Target"), tags$th("CV"), tags$th("Implementation")
            )),
            tags$tbody(
              tags$tr(tags$td("Interindividual variability"), tags$td("BW"), tags$td("5%"), tags$td("Applied as log-normal variability when generating the bank of 30 virtual rats")),
              tags$tr(tags$td("Interindividual variability"), tags$td("F_eff"), tags$td("10%"), tags$td("Applied as log-normal variability when generating the bank of 30 virtual rats")),
              tags$tr(tags$td("Interindividual variability"), tags$td("ka"), tags$td("10%"), tags$td("Applied as log-normal variability when generating the bank of 30 virtual rats")),
              tags$tr(tags$td("Interindividual variability"), tags$td("Vmax"), tags$td("15%"), tags$td("Applied as log-normal variability when generating the bank of 30 virtual rats")),
              tags$tr(tags$td("Measurement error"), tags$td("Plasma PTB concentration"), tags$td("6%"), tags$td("A log-normal measurement error is applied at each blood sampling")),
              tags$tr(tags$td("Measurement error"), tags$td("Terminal brain PTB concentration"), tags$td("6%"), tags$td("A log-normal measurement error is applied at terminal brain measurement")),
              tags$tr(tags$td("Measurement error"), tags$td("Hepatic virtual ex vivo PTB activity"), tags$td("10%"), tags$td("A log-normal measurement error is applied at virtual-assay measurement"))
            )
          ),
          p(sprintf("The virtual rat bank is generated with a fixed seed (%d), so as long as the same bank is used, the rat-specific differences in BW, F_eff, ka, and Vmax are reproducible.",
                    CODE03_VARIABILITY_POLICY$rat_bank_seed)),
          p("Parameters without independently assigned IIV: Km, k12/k21/k13/k31, Qe, Ve, Kp,e, the reference C_RR,thr, and M_RR. Km may differ among rats because of body-weight scaling and treatment-related relative changes, but no independent Km-IIV is assigned."),
          p("No additional random measurement error is added to the timing of righting-reflex checks themselves. The judgment is made at the time on the virtual clock when the learner performs the check operation.")),

      div(class = "reference-box app-lead",
          strong("Final parameter configuration implemented in this version"),
          tags$table(class = "table table-condensed table-bordered",
            tags$thead(tags$tr(tags$th("Group"), tags$th("Vmax relative change"), tags$th("Km relative change"), tags$th("M_RR"), tags$th("Model interpretation"))),
            tags$tbody(
              tags$tr(tags$td("Control"), tags$td("×1.000"), tags$td("×1.000"), tags$td("×1.000"), tags$td("Reference model")),
              tags$tr(tags$td("PHB"), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$PHB$vmax_mult)), tags$td("×1.000"), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$PHB$M_RR)), tags$td("Represents metabolic induction as an increase in Vmax")),
              tags$tr(tags$td("OME"), tags$td("×1.000"), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$OME$km_mult)), tags$td(sprintf("×%.3f", MODEL_DEFAULTS$treatment$OME$M_RR)), tags$td("Represents the Henry 1986 decrease in clearance as an increase in Km"))
            )
          ),
          p("Changes in systemic-PK Vmax/Km and M_RR used to reproduce behavioral differences serve different roles. Vmax/Km are determined first from PK data, and M_RR is then determined from behavioral differences. Actual righting-reflex decisions use C_RR,thr × M_RR."),
          p("In the hepatic virtual ex vivo assay, both the Vmax and Km relative changes assigned to each group are applied to the same standard substrate concentration. In PHB, the increase in Vmax, and in OME, the increase in Km, are directly reflected in the calculation of hepatic metabolic activity.")),

      div(class = "reference-box app-lead",
          strong("Key references"),
          tags$ul(
            tags$li(HTML("<b>Hatanaka T, Sato S, Endoh M, Katayama K, Kakemi M, Koizumi T.</b> Effect of chlorpromazine on the pharmacokinetics and pharmacodynamics of pentobarbital in rats. <i>J Pharmacobiodyn.</i> 1988;11(1):18–30. doi:10.1248/bpb1978.11.18. Primary source for the 3-compartment open model, Michaelis–Menten elimination, and the brain/plasma concentration relationship.")),
            tags$li(HTML("<b>Means JR, Schnell RC, Miya TS, Bousquet WF.</b> Correlation of phenobarbital- and SKF 525-A-induced modification of pentobarbital hypnosis with alteration of in vivo and in vitro pentobarbital metabolism in the rat. <i>Pharmacology.</i> 1978;16(4):181–192. doi:10.1159/000136765. Main reference for the PHB group.")),
            tags$li(HTML("<b>Henry DA, Macdonald IA, Kitchingman G, Bell GD, Langman MJS.</b> Omeprazole effects on oxidative drug metabolism. <i>Clin Exp Pharmacol Physiol.</i> 1986;13:377–381. doi:10.1111/j.1440-1681.1986.tb00916.x. Main reference for the OME group.")),
            tags$li(HTML("<b>Kuntzman R, Ikeda M, Jacobson M, Conney AH.</b> A sensitive method for the determination and isolation of pentobarbital-C14 metabolites and its application to in vitro studies of drug metabolism. <i>J Pharmacol Exp Ther.</i> 1967;157(1):220–226. Reference for in vitro assay conditions of hepatic pentobarbital metabolism.")),
            tags$li("Other literature endpoints were used as reference information for model construction and external validation.")
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
  action_message <- reactiveVal("When the experiment starts, operation results will be shown here.")
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
    if (any(lens != 3L)) return("Select exactly three rats for each group.")
    all_ids <- unlist(groups, use.names = FALSE)
    if (anyDuplicated(all_ids)) return("The same rat cannot be assigned to multiple groups.")
    NULL
  })

  output$setup_validation <- renderUI({
    err <- assignment_error()
    if (is.null(err)) {
      div(class = "debrief-box", strong("Assignment check:"), "Three rats each in Control / PHB / OME, with no duplicates. You can start.")
    } else {
      div(class = "note-box", strong("Please revise the assignment:"), err)
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
    x$Group <- ifelse(x$RatID %in% names(assign_now), unname(assign_now[x$RatID]), "Unused")
    x <- x[, c("RatID", "BW_g", "Group")]
    names(x) <- c("Rat", "Body weight (g)", "Current assignment")
    x
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  observeEvent(input$session_file, {
    req(input$session_file$datapath)
    tryCatch({
      e <- load_experiment_session(input$session_file$datapath)
      exp_rv(e)
      clear_post_analysis_cache()
      withProgress(message = "Preparing model internal information...", value = 0.5, {
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
      action_message("The saved session was restored. Model internal information is displayed.")
      bump()
      updateNavbarPage(session, "main_tab", selected = "4. Disclosure of model internals")
      showNotification("Session restored.", type = "message")
    }, error = function(err) {
      showNotification(paste("Could not load session:", conditionMessage(err)), type = "error")
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
    action_message(sprintf("The experiment has started. Pentobarbital %.1f mg/kg i.p. First select the rat to operate on.", input$dose_mgkg))
    bump()
    updateNavbarPage(session, "main_tab", selected = "2. Experimental operations")
  })

  output$experimentStarted <- reactive(!is.null(exp_rv()))
  outputOptions(output, "experimentStarted", suspendWhenHidden = FALSE)
  output$debriefRevealed <- reactive(isTRUE(revealed()))
  outputOptions(output, "debriefRevealed", suspendWhenHidden = FALSE)

  output$experiment_gate <- renderUI({
    if (is.null(exp_rv())) div(class = "note-box", 'Please start the experiment first in "1. Experimental setup".')
  })
  output$log_gate <- renderUI({
    if (is.null(exp_rv())) div(class = "note-box", "The operation log will appear after the experiment starts.")
  })
  output$debrief_gate <- renderUI({
    if (is.null(exp_rv())) div(class = "note-box", "Please start the experiment first. After the experiment ends and model internals are disclosed, they will appear here.")
  })

  safe_action <- function(expr, success = NULL) {
    if (isTRUE(revealed())) {
      msg <- "The experiment has ended and model internal information has been disclosed. No new experimental operations can be performed."
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
      action_message(paste("Operation failed:", conditionMessage(e)))
      NULL
    })
  }

  observeEvent(input$dose_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      administer_pentobarbital(e, rat_id),
      function(x) sprintf("Pentobarbital was administered to %s. Current clock time: %.2f min.", rat_id, e$clock_min)
    )
  })

  observeEvent(input$lrr_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      check_righting_reflex(e, rat_id),
      function(x) sprintf("%s: righting reflex %s (post-dose %.2f min, clock %.2f min).",
                          rat_id, ifelse(x$righting_reflex == "absent", "Absent (hypnosis)", "Present (awake)"),
                          x$elapsed_postdose_min, x$virtual_clock_min)
    )
  })

  observeEvent(input$lrr_blood_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      collect_lrr_and_blood(e, rat_id, add_assay_error = TRUE),
      function(x) sprintf("%s: righting reflex %s, plasma pentobarbital concentration %.2f µg/mL (post-dose %.2f min, clock %.2f min).",
                          rat_id,
                          ifelse(x$lrr$righting_reflex == "absent", "Absent (hypnosis)", "Present (awake)"),
                          x$blood$measured_plasma_ug_mL,
                          x$blood$elapsed_postdose_min,
                          x$blood$virtual_clock_min)
    )
  })

  quick_wait <- function(minutes) {
    e <- req(exp_rv())
    safe_action(
      wait_minutes(e, minutes),
      function(x) sprintf("Waited %.2f min. Current clock time: %.2f min.", minutes, e$clock_min)
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
      function(x) sprintf("Waited %.2f min. Current clock time: %.2f min.", input$wait_min, e$clock_min)
    )
  })

  observeEvent(input$tissue_btn, {
    e <- req(exp_rv()); rat_id <- req(input$active_rat)
    safe_action(
      collect_terminal_tissues(e, rat_id),
      function(x) sprintf(
        "%s: terminal tissue collection completed. Brain pentobarbital %.2f µg/g; hepatic virtual ex vivo pentobarbital metabolic activity %.3f mg/h (common substrate condition %.0f µM). Tissue collection for this rat is complete, and no further operations can be performed on this rat.",
        rat_id, x$brain$brain_ug_g, x$liver$measured_activity_mg_h, x$liver$substrate_uM
      )
    )
  })

  output$clock_text <- renderText({
    refresh(); e <- req(exp_rv()); fmt_num(e$clock_min, 2)
  })

  output$action_result <- renderUI({
    refresh(); HTML(paste0("<strong>Latest action:</strong><br>", htmltools::htmlEscape(action_message())))
  })

  output$status_table <- renderTable({
    refresh(); e <- req(exp_rv())
    x <- experiment_status(e)
    x$dose_time_min <- fmt_num(x$dose_time_min)
    x$elapsed_postdose_min <- fmt_num(x$elapsed_postdose_min)
    x$latest_lrr_clock_min <- fmt_num(x$latest_lrr_clock_min)
    x$latest_rr <- ifelse(is.na(x$latest_rr), "Not checked",
                           ifelse(x$latest_rr == "absent", "Absent (hypnosis)", "Present (awake)"))
    names(x) <- c("Rat", "Group", "Dose time", "Post-dose time", "Latest righting reflex", "RR check time", "Blood samples", "Tissue collection")
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
    names(x) <- c("Rat", "Group", "Clock", "Post-dose", "Blood sample #", "Measured concentration µg/mL")
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
    x$righting_reflex <- ifelse(x$righting_reflex == "absent", "Absent (hypnosis)", "Present (awake)")
    names(x) <- c("Rat", "Group", "Dose", "Clock", "Post-dose", "Righting reflex")
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
      "#", "Start", "End", "Rat", "Action", "Group", "Post-dose", "Result"
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
      withProgress(message = "Calculating model internal information...", value = 0.5, {
        post_analysis_cache(compute_post_analysis_cache(e))
      })
      revealed(TRUE)
      action_message("The experiment has ended and data collection is complete. Model internal information that was hidden during the experiment is now disclosed. No further experimental operations can be performed.")
      bump()
    }
    updateNavbarPage(session, "main_tab", selected = "4. Disclosure of model internals")
  })

  output$latent_table <- renderTable({
    req(revealed()); refresh(); e <- req(exp_rv()); rat_id <- req(input$debrief_rat)
    r <- e$rat_bank[e$rat_bank$RatID == rat_id, , drop = FALSE]
    tr <- as.character(e$treatment[[rat_id]])
    p <- scaled_rat_parameters(as.list(r), tr)
    data.frame(
      `Displayed item` = c(
        "Treatment",
        "Body weight",
        "Effective systemic input fraction (F_eff)",
        "Absorption rate constant (ka)",
        "Vmax interindividual relative factor (IIV)",
        "Treatment-related Vmax relative change",
        "Operative maximum metabolic rate (Vmax)",
        "Treatment-related Km relative change",
        "Operative systemic-model Km",
        "Residual threshold multiplier (M_RR)",
        "Reference righting-reflex effect-site threshold (C_RR,thr)",
        "Effective righting-reflex threshold (C_RR,thr × M_RR)"
      ),
      `Value` = c(
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
      `Meaning` = c(
        "The pretreatment applied to this rat.",
        "Affects individual differences in dose calculation and distribution volume.",
        "A model coefficient representing the effective input from intraperitoneal dosing into the systemic circulation. It is not the strict bioavailability itself.",
        "The absorption rate from the intraperitoneal site. Larger values produce a faster rise.",
        "The rat-specific relative change in Vmax due to interindividual variability (IIV).",
        "The treatment-specific relative change applied to Vmax. Control/OME are ×1; PHB is about ×2.011.",
        "The maximum metabolic rate actually used in the ODE for this rat after incorporating body weight, interindividual variability, and treatment effects on Vmax.",
        "The treatment-specific relative change applied to Km. Control/PHB are ×1; OME is about ×3.456.",
        "The mass-based Km actually used in the systemic PK model after body-weight scaling and treatment effects on Km.",
        "A residual righting-reflex threshold multiplier fitted after PK fitting to reproduce literature hypnosis-duration ratios. It is not regarded as a pure PD parameter.",
        "The common effect-site threshold calibrated using the Control group as the reference.",
        "The treatment-specific threshold actually used for righting-reflex decisions. When Ce is at or above this value, the state is judged as LRR/hypnosis."
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$true_transition_table <- renderTable({
    req(revealed()); refresh(); e <- req(exp_rv()); rat_id <- req(input$debrief_rat)
    if (is.na(e$dose_time[[rat_id]])) return(data.frame(Item = "Pentobarbital not administered", Value = "—"))
    cache <- get_post_analysis_cache(e)
    z <- cache$transitions[[rat_id]]
    data.frame(
      Item = c("True LRR onset", "True LRR recovery", "True LRR duration", "True-state search status"),
      Value = c(if (is.na(z$onset_min)) "Hypnosis not achieved" else paste0(fmt_num(z$onset_min, 2), " min"),
                if (is.na(z$recovery_min)) "—" else paste0(fmt_num(z$recovery_min, 2), " min"),
                if (is.na(z$sleep_duration_min)) "—" else paste0(fmt_num(z$sleep_duration_min, 2), " min"),
                paste0(z$status, " (search horizon ", fmt_num(z$search_horizon_min, 0), " min)") ),
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
    names(out) <- c("Rat","Group","BW (kg)","F_eff","ka (h^-1)",
                    "Vmax relative change","Vmax (mg/h)",
                    "Km relative change","Km (mg)",
                    "M_RR","Effective righting-reflex threshold (ug/g)",
                    "Plasma PTB 15 min (ug/mL)","Plasma PTB 30 min (ug/mL)",
                    "Plasma PTB 60 min (ug/mL)","Plasma PTB 120 min (ug/mL)",
                    "True LRR onset (min)","True LRR recovery (min)",
                    "True LRR duration (min)","Hepatic virtual ex vivo PTB activity (mg/h)")
    out$`Vmax relative change` <- paste0("×", sprintf("%.3f", out$`Vmax relative change`))
    out$`Km relative change` <- paste0("×", sprintf("%.3f", out$`Km relative change`))
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
