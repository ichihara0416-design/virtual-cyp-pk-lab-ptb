source("R/01_model_core.R"); REF<-readRDS("outputs/reference_fit.rds")
form_mult<-function(form,m)if(form=="Vmax")c(vmax=m,km=1) else c(vmax=1,km=m)
pred_pk<-function(r,form,m){mm<-form_mult(form,m);dose<-r$ptb_dose_mgkg
 if(r$pk_target_type=="apparent_k_ratio"){tc<-split_num(r$control_sampling_times_min);tt<-split_num(r$treatment_sampling_times_min)
  return(model_k(REF$F_eff,REF$ka,REF$C_RR_thr,dose,tt,mm["vmax"],mm["km"])/model_k(REF$F_eff,REF$ka,REF$C_RR_thr,dose,tc))}
 if(r$pk_target_type=="clearance_ratio"){tt<-split_num(r$pk_fit_times_min)
  return(terminal_CL(REF$F_eff,REF$ka,REF$C_RR_thr,dose,tt,mm["vmax"],mm["km"])/terminal_CL(REF$F_eff,REF$ka,REF$C_RR_thr,dose,tt))}
 if(r$pk_target_type %in% c("matched_concentration_ratios","single_concentration_ratio")){tt<-split_num(r$control_sampling_times_min)
  return(plasma_at(REF$F_eff,REF$ka,REF$C_RR_thr,dose,tt,mm["vmax"],mm["km"])/plasma_at(REF$F_eff,REF$ka,REF$C_RR_thr,dose,tt))}
 stop("Unsupported target")}
fit_one<-function(r,form){obs<-split_num(r$observed_pk_ratio_vector);fn<-function(logm)safe_log_sse(pred_pk(r,form,exp(logm)),obs)
 q<-optimize(fn,log(MULTIPLIER_BOUNDS),tol=1e-10);m<-exp(q$minimum);pp<-pred_pk(r,form,m)
 btol<-1e-4*diff(log(MULTIPLIER_BOUNDS));bh<-abs(q$minimum-log(MULTIPLIER_BOUNDS[1]))<btol||abs(q$minimum-log(MULTIPLIER_BOUNDS[2]))<btol
 data.frame(condition_id=r$condition_id,drug=r$drug,study=r$study,evidence_tier=r$evidence_tier,pk_target_type=r$pk_target_type,
 model_form=form,multiplier=m,objective=q$objective,observed_pk_ratio=paste(signif(obs,10),collapse=";"),
 predicted_pk_ratio=paste(signif(pp,10),collapse=";"),n_pk_observables=length(obs),boundary_hit=bh,behavior_used_in_pk_fit=FALSE)}
out<-list();n<-0
for(i in seq_len(nrow(EVIDENCE)))for(form in CANDIDATE_FORMS){n<-n+1;out[[n]]<-fit_one(EVIDENCE[i,,drop=FALSE],form)}
FITS<-do.call(rbind,out);write.csv(FITS,"outputs/pk_fits_A_B.csv",row.names=FALSE);saveRDS(FITS,"outputs/pk_fits_A_B.rds")
