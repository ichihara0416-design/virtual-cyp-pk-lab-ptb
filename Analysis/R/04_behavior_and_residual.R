source("R/01_model_core.R")
REF<-readRDS("outputs/reference_fit.rds"); FITS<-readRDS("outputs/pk_fits_A_B.rds")
lookup<-setNames(seq_len(nrow(EVIDENCE)),EVIDENCE$condition_id)

status_for<-function(drug,form,tier){
 if(tier=="B")return("exploratory_sparse_PK")
 d<-MECHANISTIC_ADJUDICATION$decision[match(drug,MECHANISTIC_ADJUDICATION$drug)]
 if(d=="Vmax_favored")return(ifelse(form=="Vmax","primary","model_form_sensitivity"))
 if(d=="Km_favored")return(ifelse(form=="Km","primary","model_form_sensitivity"))
 if(d=="retain_both")return("retained_model_form_uncertainty")
 if(d=="no_unique_form")return("retained_no_unique_model_form")
 "not_adjudicated"
}

# Diagnostic classification of the treatment trajectory.
# This does not change the model or the definition of LRR duration.
behavior_diagnostic<-function(F_eff,ka,C_RR_thr,dose_mgkg,vmax_mult=1,km_mult=1,
                              M_RR=1,max_time_min=720){
 z<-simulate_rat(F_eff,ka,C_RR_thr,dose_mgkg,
                 vmax_mult=vmax_mult,km_mult=km_mult,
                 M_RR=M_RR,max_time_min=max_time_min)
 thr<-C_RR_thr*M_RR
 cr<-crossings(z$time_min,z$Ce-thr)
 cr<-cr[cr>1e-8]
 ncr<-length(cr)
 pred_status<-if(ncr==0L) "no_threshold_crossing" else
              if(ncr==1L) "no_recovery_within_horizon" else
              "finite_duration"
 dur<-if(ncr>=2L) cr[2]-cr[1] else NA_real_
 list(duration_min=dur,prediction_status=pred_status,n_crossings=ncr,
      max_Ce=max(z$Ce,na.rm=TRUE),threshold=thr)
}

rows<-lapply(seq_len(nrow(FITS)),function(i){
 f<-FITS[i,]; r<-EVIDENCE[lookup[[f$condition_id]],]
 vm<-if(f$model_form=="Vmax")f$multiplier else 1
 km<-if(f$model_form=="Km")f$multiplier else 1

 d0<-lrr_endpoints(REF$F_eff,REF$ka,REF$C_RR_thr,r$ptb_dose_mgkg,
                   max_time_min=720)["duration_min"]
 dg<-behavior_diagnostic(REF$F_eff,REF$ka,REF$C_RR_thr,r$ptb_dose_mgkg,
                         vmax_mult=vm,km_mult=km,max_time_min=720)
 d1<-dg$duration_min
 pr<-unname(d1/d0)
 obs<-r$observed_behavior_ratio
 le<-if(is.finite(pr)&&pr>0&&is.finite(obs)&&obs>0) log(pr/obs) else NA_real_

 data.frame(
   f,
   status=status_for(f$drug,f$model_form,f$evidence_tier),
   observed_behavior_ratio=obs,
   calculated_behavior_ratio=pr,
   calculated_to_reported=if(is.finite(pr)&&is.finite(obs)&&obs!=0) pr/obs else NA_real_,
   log_error=le,
   abs_log_error=if(is.finite(le)) abs(le) else NA_real_,
   fold_error=if(is.finite(le)) exp(abs(le)) else NA_real_,
   direction_concordant=if(is.finite(pr)&&is.finite(obs)) (pr-1)*(obs-1)>=0 else NA,
   prediction_status=dg$prediction_status,
   n_threshold_crossings=dg$n_crossings,
   max_Ce=dg$max_Ce,
   LRR_threshold=dg$threshold,
   selection_based_on_behavior=FALSE
 )
})

PRED<-do.call(rbind,rows)
write.csv(PRED,"outputs/calculated_vs_reported_behavior_A_B.csv",row.names=FALSE)
saveRDS(PRED,"outputs/calculated_vs_reported_behavior_A_B.rds")

# Residual M_RR is diagnostic and estimated only for Evidence A.
A<-PRED[PRED$evidence_tier=="A",]
mrows<-lapply(seq_len(nrow(A)),function(i){
 f<-A[i,]; r<-EVIDENCE[lookup[[f$condition_id]],]
 vm<-if(f$model_form=="Vmax")f$multiplier else 1
 km<-if(f$model_form=="Km")f$multiplier else 1
 d0<-lrr_endpoints(REF$F_eff,REF$ka,REF$C_RR_thr,r$ptb_dose_mgkg,
                   max_time_min=720)["duration_min"]
 obs<-r$observed_behavior_ratio

 fn <- function(logm){
   m <- exp(logm)
   
   d1 <- lrr_endpoints(
     REF$F_eff, REF$ka, REF$C_RR_thr,
     r$ptb_dose_mgkg,
     vm, km, m,
     max_time_min = 720
   )["duration_min"]
   
   if(!is.finite(d1) || d1 <= 0 ||
      !is.finite(d0) || d0 <= 0 ||
      !is.finite(obs) || obs <= 0){
     return(1e12)
   }
   
   val <- log((d1/d0)/obs)^2
   
   if(!is.finite(val)) 1e12 else val
 }
 
 q<-optimize(fn,log(c(.2,2.5)),tol=1e-9)
 m<-exp(q$minimum)
 data.frame(condition_id=f$condition_id,drug=f$drug,model_form=f$model_form,
            status=f$status,PK_multiplier=f$multiplier,M_RR=m,
            residual_objective=q$objective)
})
M<-do.call(rbind,mrows)
write.csv(M,"outputs/M_RR_EvidenceA.csv",row.names=FALSE)
