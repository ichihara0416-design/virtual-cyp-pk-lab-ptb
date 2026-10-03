source("R/01_model_core.R");REF<-readRDS("outputs/reference_fit.rds")
bench<-read.csv("data/control_variability_benchmarks.csv")
mp_ln<-function(mean,cv,z) mean*exp(-.5*log1p(cv^2)+sqrt(log1p(cv^2))*z)
mult_ln<-function(cv,z) exp(-.5*log1p(cv^2)+sqrt(log1p(cv^2))*z)
cv_s<-function(x)sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)
set.seed(VAR_SEED);Z<-data.frame(BW=rnorm(VAR_N),F=rnorm(VAR_N),ka=rnorm(VAR_N),V=rnorm(VAR_N),err=rnorm(VAR_N))
pop<-data.frame(BW=mp_ln(SOURCE_PARAMETERS$BW_ref,BASE_IIV_CV["BW"],Z$BW),F=mp_ln(REF$F_eff,BASE_IIV_CV["F_eff"],Z$F),
 ka=mp_ln(REF$ka,BASE_IIV_CV["ka"],Z$ka),V=mult_ln(BASE_IIV_CV["Vmax"],Z$V),err=mult_ln(PLASMA_MEASUREMENT_CV,Z$err))
out<-data.frame(plasma25=rep(NA_real_,VAR_N),sleep25=rep(NA_real_,VAR_N),sleep30=rep(NA_real_,VAR_N),sleep50=rep(NA_real_,VAR_N))
for(i in 1:VAR_N){args<-pop[i,];z25<-simulate_rat(args$F,args$ka,REF$C_RR_thr,25,BW_kg=args$BW,vmax_iiv=args$V,max_time_min=70)
 out$plasma25[i]<-approx(z25$time_min,z25$Cp,xout=60)$y*args$err
 out$sleep25[i]<-lrr_endpoints(args$F,args$ka,REF$C_RR_thr,25,BW_kg=args$BW,vmax_iiv=args$V,max_time_min=600)["duration_min"]
 out$sleep30[i]<-lrr_endpoints(args$F,args$ka,REF$C_RR_thr,30,BW_kg=args$BW,vmax_iiv=args$V,max_time_min=600)["duration_min"]
 out$sleep50[i]<-lrr_endpoints(args$F,args$ka,REF$C_RR_thr,50,BW_kg=args$BW,vmax_iiv=args$V,max_time_min=720)["duration_min"]}
simcv<-c(cv_s(out$plasma25),cv_s(out$sleep25),cv_s(out$sleep30),cv_s(out$sleep50))
q<-data.frame(benchmark_id=bench$benchmark_id,literature_cv=bench$cv,simulated_cv=simcv,ratio=simcv/bench$cv)
write.csv(q,"outputs/control_variability_qualification.csv",row.names=FALSE)
# fixed app rat bank
set.seed(VAR_SEED);z<-data.frame(BW=rnorm(APP_RAT_BANK_N),F=rnorm(APP_RAT_BANK_N),ka=rnorm(APP_RAT_BANK_N),V=rnorm(APP_RAT_BANK_N))
bank<-data.frame(RatID=sprintf("Rat_%02d",1:APP_RAT_BANK_N),BW_kg=mp_ln(.3,.05,z$BW),F_eff=mp_ln(REF$F_eff,.10,z$F),
 ka=mp_ln(REF$ka,.10,z$ka),Vmax_iiv=mult_ln(.15,z$V),C_RR_thr=REF$C_RR_thr);write.csv(bank,"outputs/virtual_rat_bank.csv",row.names=FALSE)
# app parameters derive only from unified A+B pipeline Evidence-A primary forms and M_RR.
P<-read.csv("outputs/calculated_vs_reported_behavior_A_B.csv");M<-read.csv("outputs/M_RR_EvidenceA.csv")
pick<-function(id,form){p<-P[P$condition_id==id&P$model_form==form,];m<-M[M$condition_id==id&M$model_form==form,];c(pk=p$multiplier[1],thr=m$M_RR[1])}
phb<-pick("PHB10_Means1978","Vmax");ome<-pick("OME40_Henry1986","Km")
app<-data.frame(group=c("Control","PHB","OME"),Vmax_multiplier=c(1,phb["pk"],1),Km_multiplier=c(1,1,ome["pk"]),M_RR=c(1,phb["thr"],ome["thr"]))
write.csv(app,"outputs/app_parameters.csv",row.names=FALSE)
