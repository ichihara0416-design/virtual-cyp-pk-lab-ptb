# Public reproducibility package:
# Computational input CSV files are limited to:
#   data/evidence_A_B.csv
#   data/control_variability_benchmarks.csv
#   data/literature_fixed_inputs.csv
# Literature-source exclusion/provenance/context files are documentation only
# and are not required to execute the quantitative pipeline.
options(stringsAsFactors=FALSE)
if(!requireNamespace("deSolve",quietly=TRUE)) stop("Install package 'deSolve'.")
DIRS<-list(outputs="outputs",figures="figures",tables="tables")
invisible(lapply(DIRS,dir.create,recursive=TRUE,showWarnings=FALSE))
ODE_RTOL<-1e-9; ODE_ATOL<-1e-11; GRID_STEP_MIN<-0.10; REFINE_STEP_MIN<-0.02
SOURCE_PARAMETERS<-list(BW_ref=0.300,k12=223,k21=13.5,k13=20.6,k31=1.37,
 Vmax_ref=2.19,Km_ref=0.0593,Qe_ref=0.0246,Vp_ref=0.0111,Kpe=1.38,Ve_ref=0.00174)

LITERATURE_FIXED_INPUTS <- read.csv(
  "data/literature_fixed_inputs.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)
cal_inputs <- LITERATURE_FIXED_INPUTS[
  LITERATURE_FIXED_INPUTS$input_group == "reference_calibration", , drop = FALSE
]
cal_required <- c("Cp40_30","onset40","duration30","C_RR_thr_anchor")
if (!all(cal_required %in% cal_inputs$input_id)) {
  stop("literature_fixed_inputs.csv is missing one or more reference-calibration inputs.")
}
CONTROL_TARGETS <- setNames(
  cal_inputs$value[match(cal_required, cal_inputs$input_id)],
  cal_required
)
# Qe_ref, Ve_ref, and Kpe originate from brain-distribution parameters in the source model.
# They are repurposed here to construct a latent effect-site state; Xe/Ce are not measured brain PK.
# Reference/control targets. The C_RR_thr numerical anchor is the 15.84 ug/g intercept
# reported by Kato et al. (1969) for whole-brain PTB concentration at the end of narcosis
# after PTB 30 mg/kg i.p. It is used only as a soft numerical anchor for the latent
# effect-site threshold scale; Ce is not interpreted as a measured whole-brain concentration.
F_GRID<-seq(.20,1,.05); KA_GRID<-seq(1,30,1); CLRR_GRID<-seq(14,19,.25)
CANDIDATE_FORMS<-c("Vmax","Km"); MULTIPLIER_BOUNDS<-c(.001,20)
MECHANISTIC_ADJUDICATION<-data.frame(
 drug=c("Phenobarbital","Omeprazole","SKF-525A","Diazepam"),
 decision=c("Vmax_favored","Km_favored","retain_both","no_unique_form"),
 stringsAsFactors=FALSE)
BASE_IIV_CV<-c(BW=.05,F_eff=.10,ka=.10,Vmax=.15)
PLASMA_MEASUREMENT_CV<-.06; EFFECT_SITE_MEASUREMENT_CV<-.06; HEPATIC_VIRTUAL_ASSAY_CV<-.10
VAR_SEED<-20260821; VAR_N<-200; APP_RAT_BANK_N<-30
HEPATIC_CSTD_UG_ML<-56.6; APP_PTB_DOSE_MGKG<-50; MC_REPS<-10000; MC_SEED<-20260918
EVIDENCE<-read.csv("data/evidence_A_B.csv",check.names=FALSE)
# Quantitative-analysis policy: do not infer PTB administration route.
# Gupta & Gupta 1977a (Toxicology; female rats) explicitly states PTB 50 mg/kg i.p.
# and is therefore retained in Evidence B. Gupta & Gupta 1977b (J Pharm Pharmacol;
# male rats) does not explicitly state the PTB route and is not encoded in EVIDENCE.
# Every quantitative row must have an explicitly curated i.p. PTB regimen.
if(any(!grepl("i\\.p\\.",EVIDENCE$PTB_regimen,ignore.case=TRUE)))
  stop("Every quantitative EVIDENCE row must have an explicitly curated i.p. PTB regimen.")
if(any(grepl("J Pharm|1977b",EVIDENCE$study,ignore.case=TRUE)))
  stop("Gupta & Gupta 1977b (male J Pharm Pharmacol paper) must not enter quantitative EVIDENCE.")
