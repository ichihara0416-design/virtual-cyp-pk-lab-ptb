source("R/00_constants.R")
scaled_parameters<-function(BW_kg,F_eff,ka,C_RR_thr,P=SOURCE_PARAMETERS,vmax_mult=1,km_mult=1,M_RR=1,vmax_iiv=1){
 s<-BW_kg/P$BW_ref
 list(F_eff=F_eff,ka=ka,k12=P$k12,k21=P$k21,k13=P$k13,k31=P$k31,
 Vp=P$Vp_ref*s,Ve=P$Ve_ref*s,Qe=P$Qe_ref*s^.75,Kpe=P$Kpe,
 Km=P$Km_ref*s*km_mult,Vmax=P$Vmax_ref*s^.75*vmax_mult*vmax_iiv,C_RR_thr=C_RR_thr*M_RR)
}
# Brain-distribution parameters reported in the source model were repurposed
# to construct a latent effect-site state (Xe/Ce). Xe is not included in systemic
# mass balance and Ce is not an independently measured brain concentration.
rhs<-function(t,state,p){with(as.list(c(state,p)),{
 Cp<-X1/Vp; Ce<-Xe/Ve; elim<-if(X1<=0)0 else Vmax*X1/(Km+X1)
 list(c(-ka*Adep,F_eff*ka*Adep-(k12+k13)*X1+k21*X2+k31*X3-elim,
 k12*X1-k21*X2,k13*X1-k31*X3,Qe*(Cp-Ce/Kpe)))})}
simulate_rat<-function(F_eff,ka,C_RR_thr,dose_mgkg,vmax_mult=1,km_mult=1,M_RR=1,
 BW_kg=SOURCE_PARAMETERS$BW_ref,vmax_iiv=1,max_time_min=180,step_min=REFINE_STEP_MIN){
 p<-scaled_parameters(BW_kg,F_eff,ka,C_RR_thr,vmax_mult=vmax_mult,km_mult=km_mult,
 M_RR=M_RR,vmax_iiv=vmax_iiv)
 tt<-seq(0,max_time_min/60,by=step_min/60)
 y0<-c(Adep=dose_mgkg*BW_kg,X1=0,X2=0,X3=0,Xe=0)
 z<-as.data.frame(deSolve::ode(y0,tt,rhs,p,method="lsoda",rtol=ODE_RTOL,atol=ODE_ATOL))
 z$time_min<-z$time*60; z$Cp<-z$X1/p$Vp; z$Ce<-z$Xe/p$Ve; z
}
crossings<-function(t,y){i<-which(y[-length(y)]*y[-1]<=0 & y[-length(y)]!=y[-1]); if(!length(i))return(numeric())
 vapply(i,function(j)t[j]-y[j]*(t[j+1]-t[j])/(y[j+1]-y[j]),numeric(1))}
lrr_from_trajectory<-function(z,thr){cr<-crossings(z$time_min,z$Ce-thr); cr<-cr[cr>1e-8]
 on<-if(length(cr)>=1)cr[1] else NA; rec<-if(length(cr)>=2)cr[2] else NA
 c(onset_min=on,recovery_min=rec,duration_min=if(is.finite(on)&&is.finite(rec))rec-on else NA)}
lrr_endpoints<-function(F_eff,ka,C_RR_thr,dose_mgkg,vmax_mult=1,km_mult=1,M_RR=1,
 BW_kg=SOURCE_PARAMETERS$BW_ref,vmax_iiv=1,max_time_min=720){
 z<-simulate_rat(F_eff,ka,C_RR_thr,dose_mgkg,vmax_mult,km_mult,M_RR,BW_kg,vmax_iiv,max_time_min)
 lrr_from_trajectory(z,C_RR_thr*M_RR)}
plasma_at<-function(F_eff,ka,C_RR_thr,dose,times,vmax=1,km=1){
 z<-simulate_rat(F_eff,ka,C_RR_thr,dose,vmax,km,max_time_min=max(times)+10)
 approx(z$time_min,z$Cp,xout=times)$y}
model_k<-function(F_eff,ka,C_RR_thr,dose,times,vmax=1,km=1){
 cp<-plasma_at(F_eff,ka,C_RR_thr,dose,times,vmax,km); ok<-is.finite(cp)&cp>0
 if(sum(ok)<3)return(NA_real_); k<--coef(lm(log(cp[ok])~times[ok]))[2]; unname(if(k>0)k else NA)}
terminal_CL<-function(F_eff,ka,C_RR_thr,dose,times,vmax=1,km=1){
 cp<-plasma_at(F_eff,ka,C_RR_thr,dose,times,vmax,km); fit<-lm(log(cp)~times)
 lam<--unname(coef(fit)[2]); C0<-exp(unname(coef(fit)[1]))
 Vd<-(F_eff*dose*SOURCE_PARAMETERS$BW_ref)/C0; Vd*lam*1000}
split_num<-function(x){if(is.na(x)||!nzchar(x))numeric() else as.numeric(strsplit(as.character(x),";",fixed=TRUE)[[1]])}
safe_log_sse<-function(pred,obs){ok<-is.finite(pred)&pred>0&is.finite(obs)&obs>0;if(!all(ok))1e12 else sum(log(pred/obs)^2)}
