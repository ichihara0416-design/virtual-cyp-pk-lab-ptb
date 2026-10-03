# Reference-calibration targets are read from data/literature_fixed_inputs.csv via 00_constants.R.
source("R/01_model_core.R")
pred_control<-function(F,ka,thr,step=REFINE_STEP_MIN){
 z40<-simulate_rat(F,ka,thr,40,max_time_min=180,step_min=step); z30<-simulate_rat(F,ka,thr,30,max_time_min=180,step_min=step)
 l40<-lrr_from_trajectory(z40,thr); l30<-lrr_from_trajectory(z30,thr)
 c(Cp40_30=approx(z40$time_min,z40$Cp,xout=30)$y,onset40=l40["onset_min"],duration30=l30["duration_min"],C_RR_thr_anchor=thr)}
obj<-function(th){if(any(!is.finite(th))||th[1]<=0||th[1]>1||th[2]<=0||th[3]<=0)return(1e12)
 p<-tryCatch(pred_control(th[1],th[2],th[3]),error=function(e)rep(NA,4));if(any(!is.finite(p)))return(1e12);sum((p/CONTROL_TARGETS-1)^2)}
g<-expand.grid(F=F_GRID,ka=KA_GRID,thr=CLRR_GRID); g$objective<-NA_real_
for(i in seq_len(nrow(g))) g$objective[i]<-obj(as.numeric(g[i,1:3]))
starts<-head(g[order(g$objective),],10)
fits<-do.call(rbind,lapply(seq_len(nrow(starts)),function(i){q<-optim(as.numeric(starts[i,1:3]),obj,method="L-BFGS-B",
 lower=c(.05,.2,13),upper=c(1,50,20),control=list(maxit=600));data.frame(F_eff=q$par[1],ka=q$par[2],C_RR_thr=q$par[3],objective=q$value,convergence=q$convergence)}))
fits<-fits[order(fits$objective),]; REF<-as.list(fits[1,c("F_eff","ka","C_RR_thr")]); saveRDS(REF,"outputs/reference_fit.rds")
write.csv(fits,"outputs/reference_refinement.csv",row.names=FALSE)
p<-pred_control(REF$F_eff,REF$ka,REF$C_RR_thr)
write.csv(data.frame(endpoint=names(CONTROL_TARGETS),published=CONTROL_TARGETS,model=unname(p),model_to_published=unname(p/CONTROL_TARGETS)),
 "outputs/control_calibration.csv",row.names=FALSE)
