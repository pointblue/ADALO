# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(ggplot2)
library(gbm)
library(Metrics)

pth<-"/home/lsalas/adalo/winter/m5dataNew2_noGeo_wLog/modelGOF/"

makejpg<-function(pobj,pth,fnam,wd=480,ht=480){
	jpeg(filename=paste(pth,fnam,".jpeg",sep=""),width = wd, height = ht, quality = 100)
		print(pobj)
	dev.off()
	return(1)
}
#load model files
load("/home/lsalas/adalo/winter/m5dataNew2_noGeo_wLog/mdlLst_w.RData")

Mode<-function(x,...){
	ux<-na.omit(unique(x))
	mx<-ux[which.max(tabulate(match(x, ux)))]
	return(mx)
}

spp<-names(mdlLst)
#spp<-names(mdlLst_b)
sum.df<-data.frame()
fit.metrics<-data.frame()
for(sps in spp){
	spmd<-mdlLst[[sps]]
	#spmd<-mdlLst_b[[sps]]
	
	mdl<-spmd[["model"]]
	obs<-mdl$data$y
	fit<-mdl$fit
	spgof<-spmd[["mgof"]]
	#cvfit<-spgof[["cvfit"]]
	cvfit<-mdl$fold.fit
	
	pdf<-data.frame(obs=obs,fit=fit,crossval=cvfit)
	
	p1<-ggplot(data=pdf,aes(x=obs,y=fit)) + geom_point() + 
			geom_abline(intercept = 0, slope = 1, color="blue") + labs(x="Observed",y="Predicted",title=sps)
	q<-makejpg(p1,pth,fnam=paste(sps,"_Obs_Fit",sep=""),wd=250,ht=250)
	meancve<-mean(pdf$crossval,na.rm=T);modecve<-Mode(pdf$crossval)
	p2<-ggplot(data=pdf,aes(x=obs,y=crossval)) + geom_point() + 
			geom_hline(yintercept=meancve,linetype="dashed",color="blue") +
			geom_hline(yintercept=modecve,linetype="dashed",color="red") +
			labs(x="Observed",y="Cross-validation error",title=sps)
	q<-makejpg(p2,pth,fnam=paste(sps,"_Obs_vs_Crossval",sep=""),wd=250,ht=250)
	p3<-ggplot(data=pdf,aes(x=crossval)) + geom_density() + labs(x="Cross-validation error",title=sps)
	q<-makejpg(p3,pth,fnam=paste(sps,"_Crossval",sep=""),wd=200,ht=200)
	
	varinf<-spgof[["varinf"]]
	row.names(varinf)<-NULL
	varinf$Sp_period<-sps
	varinf<-subset(varinf,rel.inf>0)
	varinf$rel.inf<-round(varinf$rel.inf,2)
	vrin<-varinf;names(vrin)<-c("Variable","Rel.Influence","Sp_period")
	vrin$PlotOrder<-nrow(vrin):1
	vrin$Variable<-reorder(vrin$Variable,vrin$PlotOrder)
	p4<-ggplot(data=vrin,aes(x=Variable,y=Rel.Influence)) + geom_histogram(stat="identity") +
			coord_flip() + labs(x="Relative Influence",title=sps)
	q<-makejpg(p4,pth,fnam=paste(sps,"_RelInf_BarPlot",sep=""),ht=450)
	sum.df<-rbind(sum.df,vrin)
	write.csv(vrin,file=paste(pth,sps,"_VarImportance.csv",sep=""))
	
	#based on the top vars in varinf, need to do plot(mdl,i.var=x) where x is the name of the var
	#do this for the top 9 vars, or as many vars as available if fewer
	pvars<-na.omit(varinf[1:9,])
	pdp.df<-data.frame()
	for(i in 1:nrow(pvars)){
		vv<-as.character(pvars$var[i])
		vinf<-as.character(round(pvars$rel.inf[i],1))
		w<-plot(mdl,i.var=vv,return.grid=TRUE)
		names(w)<-c("Predictor","Response")
		w$Covariate<-paste(vv,"rel.inf =",vinf)
		w$order<-i
		pdp.df<-rbind(pdp.df,w)
	}
	p5<-ggplot(data=pdp.df,aes(x=Predictor,y=Response)) + geom_line() + 
			facet_wrap(~Covariate,ncol=3,scales="free") + labs(title=sps)
	q<-makejpg(p5,pth,fnam=paste(sps,"_RelInf",sep=""),ht=450)
	
	#jpeg(filename=paste(pth,sps,"_optimization.jpg",sep=""),quality=100,width=300,height=300)
	#	gbm.perf(mdl,method="cv")
	#dev.off()
	rmsev<-rmse(pdf$obs,pdf$fit)
	#rmslev<-rmsle(pdf$obs,pdf$fit)	#data are already logged!
	ssres<-sum(se(pdf$obs,pdf$fit))
	mobs<-mean(pdf$obs)
	sstot<-sum((pdf$obs-mobs)^2)
	er2<-1-(ssres/sstot)
	corfit<-cor(fit,obs);corcvfit<-cor(cvfit,obs)
	gfm<-data.frame(SpSeason=sps,meanObs=mobs,RMSE=rmsev,estR2fit=er2,R2fit=corfit^2,R2cvfit=corcvfit^2)	#,RMSLE=rmslev
	fit.metrics<-rbind(fit.metrics,gfm)
}

save(sum.df,file=paste(pth,"VarImportance.RData",sep=""))
write.csv(sum.df,file=paste(pth,"VarImportance.csv",sep=""))

save(fit.metrics,file=paste(pth,"GOFmetrics.RData",sep=""))
write.csv(fit.metrics,file=paste(pth,"GOFmetrics.csv",sep=""))
