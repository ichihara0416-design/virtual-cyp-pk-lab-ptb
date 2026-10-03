# QA for the route-restricted unified A+B public reproducibility pipeline.
# No exclusion/provenance/context CSV files are required; eligibility is enforced
# directly from the curated quantitative input data/evidence_A_B.csv.
# Gupta & Gupta 1977a (Toxicology; female rats) explicitly used PTB 50 mg/kg i.p.
# and is eligible for Evidence B. Gupta & Gupta 1977b (J Pharm Pharmacol; male rats)
# has no explicit PTB route and must remain outside quantitative computation.

source("R/00_constants.R")
P<-read.csv("outputs/pk_fits_A_B.csv")
B<-read.csv("outputs/calculated_vs_reported_behavior_A_B.csv")
A<-P[P$evidence_tier=="A",]
EB<-P[P$evidence_tier=="B",]
BA<-B[B$evidence_tier=="A",]
BB<-B[B$evidence_tier=="B",]

n_conditions<-nrow(EVIDENCE)
n_A_conditions<-length(unique(EVIDENCE$condition_id[EVIDENCE$evidence_tier=="A"]))
n_B_conditions<-length(unique(EVIDENCE$condition_id[EVIDENCE$evidence_tier=="B"]))
required_status<-c("finite_duration","no_threshold_crossing","no_recovery_within_horizon")
status_present<-"prediction_status" %in% names(B)
all_ip<-all(grepl("i\\.p\\.",EVIDENCE$PTB_regimen,ignore.case=TRUE))
female_gupta_ids<-c("ENDO2_5_7d_Gupta1977","ENDO2_5_15d_Gupta1977","ENDO5_7d_Gupta1977","ENDO5_15d_Gupta1977")
female_gupta_present<-all(female_gupta_ids %in% EVIDENCE$condition_id)
female_gupta_study_ok<-all(EVIDENCE$study[EVIDENCE$condition_id %in% female_gupta_ids]=="Gupta & Gupta 1977a (Toxicology)")
forbidden_male_gupta<-any(grepl("1977b|J Pharm",EVIDENCE$study,ignore.case=TRUE))

checks<-data.frame(
 check=c(
   "26_conditions_x_2_forms",
   "six_A_conditions",
   "twenty_B_conditions",
   "four_female_Gupta_Toxicology_conditions_present",
   "female_Gupta_rows_identified_as_1977a_Toxicology",
   "male_Gupta_1977b_absent_from_quantitative_input",
   "all_quantitative_PTB_regimens_explicitly_ip",
   "behavior_not_in_PK_fit",
   "two_forms_each",
   "A_B_tier_preserved",
   "behavior_rows_match_PK_rows",
   "prediction_status_present",
   "A_calculated_behavior_ratios_finite",
   "A_behavior_status_finite",
   "B_prediction_status_recognized",
   "finite_status_matches_finite_ratio",
   "no_threshold_status_has_zero_crossings"
 ),
 pass=c(
   n_conditions==26 && nrow(P)==52,
   n_A_conditions==6 && length(unique(A$condition_id))==6,
   n_B_conditions==20 && length(unique(EB$condition_id))==20,
   female_gupta_present,
   female_gupta_study_ok,
   !forbidden_male_gupta,
   all_ip,
   all(!P$behavior_used_in_pk_fit),
   all(table(P$condition_id)==2),
   setequal(unique(P$evidence_tier),c("A","B")),
   nrow(B)==nrow(P),
   status_present,
   all(is.finite(BA$calculated_behavior_ratio)),
   status_present && all(BA$prediction_status=="finite_duration"),
   status_present && all(BB$prediction_status %in% required_status),
   status_present && all((B$prediction_status=="finite_duration") == is.finite(B$calculated_behavior_ratio)),
   status_present && ("n_threshold_crossings" %in% names(B)) &&
     all(B$n_threshold_crossings[B$prediction_status=="no_threshold_crossing"]==0)
 )
)

behavior_prediction_summary<-data.frame(
 evidence_tier=c("A","B"),
 total_candidates=c(nrow(BA),nrow(BB)),
 finite_duration=c(sum(BA$prediction_status=="finite_duration"),sum(BB$prediction_status=="finite_duration")),
 no_threshold_crossing=c(sum(BA$prediction_status=="no_threshold_crossing"),sum(BB$prediction_status=="no_threshold_crossing")),
 no_recovery_within_horizon=c(sum(BA$prediction_status=="no_recovery_within_horizon"),sum(BB$prediction_status=="no_recovery_within_horizon"))
)

write.csv(checks,"outputs/QA.csv",row.names=FALSE)
write.csv(behavior_prediction_summary,"outputs/behavior_prediction_status_summary.csv",row.names=FALSE)
write.csv(data.frame(
  n_total_conditions=n_conditions,n_A_conditions=n_A_conditions,n_B_conditions=n_B_conditions,
  female_Gupta_1977a_in_quantitative_input=female_gupta_present,
  male_Gupta_1977b_in_quantitative_input=forbidden_male_gupta,
  all_PTB_regimens_explicitly_ip=all_ip),
  "outputs/quantitative_evidence_policy_QA.csv",row.names=FALSE)
print(checks)
print(behavior_prediction_summary)
if(any(!checks$pass))stop("QA failed")
