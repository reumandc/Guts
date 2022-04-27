#This script analyzes the data from the same viewpoint as Lei's paper

#***prelims

#packages needed (accessed with ::) are graphics, grDevices, stats, latex2exp

rm(list=ls())
set.seed(101)

#where to get the data from
datloc<-"../results/dataList.Rds"

#where to put the results
resloc<-"../results/Leiresults/"
if (!dir.exists(resloc))
{
  dir.create(resloc)
}

#load the data - see top of CleanDat.R for description
dat<-readRDS(file=datloc)

#source the functions you'll need
source("PhiVert.R")
source("CPE.R")
source("SAE.R")
source("Scom.R")
source("Scomip.R")
source("Spop.R")
source("varrat.R")
source("shannon.R")
source("simpson.R")

#***now calculate all the quantities of Lei for each subject

leires<-data.frame(subject=1:length(dat),Scom=NA,Spop=NA,Scomip=NA,SAE=NA,CPE=NA,varrat=NA,PhiVert=NA,
                   richness1=NA,richness2=NA,shannon1=NA,shannon2=NA,simpson1=NA,simpson2=NA,days=NA)
for (subject in 1:length(dat))
{
  sdat<-dat[[subject]]
  
  #remove days when no sample was available, relies on the check in CleanDat
  #that either all or none of the data are missing for a day
  sdat<-sdat[-which(is.na(sdat[,1])),]
  if (any(is.na(sdat)))
  {
    stop("Error in LeiAnalysis: checkpoint 1")
  }
  
  #remove genera which were never present
  sdat<-sdat[,-which(apply(FUN=max,X=sdat,MARGIN=2)==0)]
  
  #save the stripped down data back in dat, for use below in the clustering section
  dat[[subject]]<-sdat
  
  #now fill in the values
  leires[subject,"Scom"]<-Scom(sdat)
  leires[subject,"Spop"]<-Spop(sdat)
  leires[subject,"Scomip"]<-Scomip(sdat)
  leires[subject,"SAE"]<-SAE(sdat)
  leires[subject,"CPE"]<-CPE(sdat)
  leires[subject,"varrat"]<-varrat(sdat)
  leires[subject,"PhiVert"]<-PhiVert(sdat)
  leires[subject,"richness1"]<-dim(sdat)[2] #total number of taxa ever seen
  leires[subject,"richness2"]<-mean(apply(FUN=function(x){sum(x>0)},MARGIN=1,X=sdat)) #average across time of numbers detected in each sample
  leires[subject,"shannon1"]<-shannon(apply(FUN=mean,X=sdat,MARGIN=2)) #take the mean abundance of each species and then compute shannon
  leires[subject,"shannon2"]<-mean(apply(FUN=shannon,MARGIN=1,X=sdat)) #compute shannon for each time step and then average across time
  leires[subject,"simpson1"]<-simpson(apply(FUN=mean,X=sdat,MARGIN=2)) #take the mean abundance of each species and then compute simpson
  leires[subject,"simpson2"]<-mean(apply(FUN=simpson,MARGIN=1,X=sdat)) #compute simpson for each time step and then average across time
  leires[subject,"days"]<-dim(sdat)[1]
}
leires$invsimpson1<-1/leires$simpson1
leires$invsimpson2<-1/leires$simpson2

saveRDS(leires,file=paste0(resloc,"NumericResults.Rds"))

#***check a few identities

if (any(abs(leires$Scom-leires$PhiVert*leires$Spop)>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 2")
}
if (any(abs(leires$PhiVert-leires$SAE*leires$CPE)>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 3")
}
if (any(abs(leires$Scomip-leires$SAE*leires$Spop)>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 4")
}
if (any(abs(leires$CPE-1/sqrt(leires$varrat))>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 5")
}

#***make a few comparisons between measures of diversity

grDevices::pdf(file=paste0(resloc,"CorrelationBetweenTwoMeasuresOfRichness.pdf"))
graphics::plot(leires$richness1,leires$richness2,type="p",xlab="richness 1",ylab="richness 2")
grDevices::dev.off()
RichnessCor<-stats::cor(leires$richness1,leires$richness2) 
saveRDS(RichnessCor,paste0(resloc,"CorrelationBetweenTwoMeasuresOfRichness.Rds"))

grDevices::pdf(file=paste0(resloc,"CorrelationBetweenTwoMeasuresOfShannon.pdf"))
plot(leires$shannon1,leires$shannon2,type="p",xlab="shannon 1",ylab="shannon 2")
grDevices::dev.off()
ShannonCor<-stats::cor(leires$shannon1,leires$shannon2) 
saveRDS(ShannonCor,paste0(resloc,"CorrelationBetweenTwoMeasuresOfShannon.Rds"))

grDevices::pdf(file=paste0(resloc,"CorrelationBetweenTwoMeasuresOfSimpson.pdf"))
plot(leires$simpson1,leires$simpson2,type="p",xlab="simpson 1",ylab="simpson 2")
grDevices::dev.off()
SimpsonCor<-stats::cor(leires$simpson1,leires$simpson2) 
saveRDS(SimpsonCor,paste0(resloc,"CorrelationBetweenTwoMeasuresOfSimpson.Rds"))


grDevices::pdf(file=paste0(resloc,"RichnessVsShannon1.pdf"))
plot(leires$richness1,leires$shannon1,type="p",xlab="richness 1",ylab="shannon 1")
grDevices::dev.off()
RichnessShannon1Cor<-stats::cor(leires$richness1,leires$shannon1)
saveRDS(RichnessShannon1Cor,paste0(resloc,"CorRichnessShannon1.Rds"))

grDevices::pdf(file=paste0(resloc,"RichnessVsInvSimpson1.pdf"))
plot(leires$richness1,1/leires$simpson1,type="p",xlab="richness 1",ylab="1/(simpson 1)")
grDevices::dev.off()
RichnessInvSimpson1Cor<-stats::cor(leires$richness1,1/leires$simpson1)
saveRDS(RichnessInvSimpson1Cor,paste0(resloc,"CorRichnessInvSimpson1.Rds"))

grDevices::pdf(file=paste0(resloc,"ShannonVsInvSimpson1.pdf"))
plot(leires$shannon1,1/leires$simpson1,type="p",xlab="shannon 1",ylab="1/(simpson 1)")
grDevices::dev.off()
ShannonInvSimpson1Cor<-stats::cor(leires$shannon1,1/leires$simpson1)
saveRDS(ShannonInvSimpson1Cor,paste0(resloc,"CorShannonInvSimpson1.Rds"))


grDevices::pdf(file=paste0(resloc,"RichnessVsShannon2.pdf"))
plot(leires$richness2,leires$shannon2,type="p",xlab="richness 2",ylab="shannon 2")
grDevices::dev.off()
RichnessShannon2Cor<-stats::cor(leires$richness2,leires$shannon2)
saveRDS(RichnessShannon2Cor,paste0(resloc,"CorRichnessShannon2.Rds"))

grDevices::pdf(file=paste0(resloc,"RichnessVsInvSimpson2.pdf"))
plot(leires$richness2,1/leires$simpson2,type="p",xlab="richness 2",ylab="1/(simpson 2)")
grDevices::dev.off()
RichnessInvSimpson2Cor<-stats::cor(leires$richness2,1/leires$simpson2)
saveRDS(RichnessInvSimpson2Cor,paste0(resloc,"CorRichnessInvSimpson2.Rds"))

grDevices::pdf(file=paste0(resloc,"ShannonVsInvSimpson2.pdf"))
plot(leires$shannon2,1/leires$simpson2,type="p",xlab="shannon 2",ylab="1/(simpson 2)")
grDevices::dev.off()
ShannonInvSimpson2Cor<-stats::cor(leires$shannon2,1/leires$simpson2)
saveRDS(ShannonInvSimpson2Cor,paste0(resloc,"CorShannonInvSimpson2.Rds"))

#The upshot of the above is basically that it should hopefully not matter too much what 
#index you use, but you better generete plots using each of them to make sure.

#***now make plots to compare our results to those of Lei

#In his manuscript, Lei plots the various components of his theory against different measures
#of diversity. This does the same for the gut microbiome data.
#
#Args
#leires       The dataframe constructed above
#divmeasure   The diversity measure to use. One of the column names of the leires dataframe.
#plotloc      The path to save the plot to, including file name with the extension .pdf
#
#Output
#A data frame with regression and other stats for plots of each component of Lei's theory 
#against the diversity measure. Also generates a plot and saves it to plotloc.
#
MakeLeiPlots<-function(leires,divmeasure,plotloc)
{
  #the x axis of all panels
  x<-leires[,divmeasure]
  xlimits<-range(x)
  
  #set up the pdf for plotting
  panwd<-3
  panht<-panwd
  ylabwd<-0.7
  xlabht<-0.7
  gap<-0.1
  totwd<-ylabwd+panwd+gap
  totht<-xlabht+5*(panht+gap)
  grDevices::pdf(file=plotloc,width=totwd,height=totht)
  
  #receptacle for numeric results
  numres<-list()
  
  #get numeric results for Scom
  y<-leires[,"Scom"]
  m<-stats::lm(y~x)
  Rsq<-summary(m)$r.squared
  p<-anova(m)["Pr(>F)"]
  p<-p[1,1]
  slope<-unname(coef(m)[2])
  int<-unname(coef(m)[1])
  numres[1]<-list(Scom=c(Rsq=Rsq,p=p,slope=slope,int=int))
  
  #plot Scom
  graphics::par(fig=c((ylabwd)/totwd,
            (ylabwd+panwd)/totwd,
            (xlabht+4*(panht+gap))/totht,
            (xlabht+4*(panht+gap)+panht)/totht),
      mai=c(0,0,0,0),mgp=c(3,0.75,0))
  ylimits<-range(y)
  ylimits[2]<-ylimits[2]+.2*diff(ylimits)
  graphics::plot(x,y,
                 xlim=xlimits,ylim=ylimits,
                 xaxt="n")
  axis(side=1,labels=FALSE)
  mtext(latex2exp::TeX("$S_{com}$"),side=2,line=1.8)
  text(xlimits[1],ylimits[2],
       paste0("Rsq=",round(Rsq,3),"; p=",round(p,3),"; slope=",round(slope,3)),
       adj=c(0,1))
  abline(a=int,b=slope)
  
  #get numeric results for Spop
  y<-leires[,"Spop"]
  m<-stats::lm(y~x)
  Rsq<-summary(m)$r.squared
  p<-anova(m)["Pr(>F)"]
  p<-p[1,1]
  slope<-unname(coef(m)[2])
  int<-unname(coef(m)[1])
  numres[2]<-list(Spop=c(Rsq=Rsq,p=p,slope=slope,int=int))
  
  #plot Spop
  graphics::par(fig=c((ylabwd)/totwd,
                      (ylabwd+panwd)/totwd,
                      (xlabht+3*(panht+gap))/totht,
                      (xlabht+3*(panht+gap)+panht)/totht),
                mai=c(0,0,0,0),mgp=c(3,0.75,0),new=TRUE)
  ylimits<-range(y)
  ylimits[2]<-ylimits[2]+.2*diff(ylimits)
  graphics::plot(x,y,
                 xlim=xlimits,ylim=ylimits,
                 xaxt="n")
  axis(side=1,labels=FALSE)
  mtext(latex2exp::TeX("$S_{pop}$"),side=2,line=1.8)
  text(xlimits[1],ylimits[2],
       paste0("Rsq=",round(Rsq,3),"; p=",round(p,3),"; slope=",round(slope,3)),
       adj=c(0,1))
  abline(a=int,b=slope)
  
  #get numeric results for asynchrony
  y<-leires[,"PhiVert"]
  m<-stats::lm(y~x)
  Rsq<-summary(m)$r.squared
  p<-anova(m)["Pr(>F)"]
  p<-p[1,1]
  slope<-unname(coef(m)[2])
  int<-unname(coef(m)[1])
  numres[3]<-list(PhiVert=c(Rsq=Rsq,p=p,slope=slope,int=int))
  
  #plot asynchrony
  graphics::par(fig=c((ylabwd)/totwd,
                      (ylabwd+panwd)/totwd,
                      (xlabht+2*(panht+gap))/totht,
                      (xlabht+2*(panht+gap)+panht)/totht),
                mai=c(0,0,0,0),mgp=c(3,0.75,0),new=TRUE)
  ylimits<-range(y)
  ylimits[2]<-ylimits[2]+.2*diff(ylimits)
  graphics::plot(x,y,
                 xlim=xlimits,ylim=ylimits,
                 xaxt="n")
  axis(side=1,labels=FALSE)
  mtext(latex2exp::TeX("$\\Phi$"),side=2,line=1.8)
  text(xlimits[1],ylimits[2],
       paste0("Rsq=",round(Rsq,3),"; p=",round(p,3),"; slope=",round(slope,3)),
       adj=c(0,1))
  abline(a=int,b=slope)
  
  #get numeric results for CPE
  y<-leires[,"CPE"]
  m<-stats::lm(y~x)
  Rsq<-summary(m)$r.squared
  p<-anova(m)["Pr(>F)"]
  p<-p[1,1]
  slope<-unname(coef(m)[2])
  int<-unname(coef(m)[1])
  numres[4]<-list(CPE=c(Rsq=Rsq,p=p,slope=slope,int=int))
  
  #plot CPE
  graphics::par(fig=c((ylabwd)/totwd,
                      (ylabwd+panwd)/totwd,
                      (xlabht+1*(panht+gap))/totht,
                      (xlabht+1*(panht+gap)+panht)/totht),
                mai=c(0,0,0,0),mgp=c(3,0.75,0),new=TRUE)
  ylimits<-range(y)
  ylimits[2]<-ylimits[2]+.2*diff(ylimits)
  graphics::plot(x,y,
                 xlim=xlimits,ylim=ylimits,
                 xaxt="n")
  axis(side=1,labels=FALSE)
  mtext("CPE",side=2,line=1.8)
  text(xlimits[1],ylimits[2],
       paste0("Rsq=",round(Rsq,3),"; p=",round(p,3),"; slope=",round(slope,3)),
       adj=c(0,1))
  abline(a=int,b=slope)
  
  #get numeric results for SAE
  y<-leires[,"SAE"]
  m<-stats::lm(y~x)
  Rsq<-summary(m)$r.squared
  p<-anova(m)["Pr(>F)"]
  p<-p[1,1]
  slope<-unname(coef(m)[2])
  int<-unname(coef(m)[1])
  numres[5]<-list(SAE=c(Rsq=Rsq,p=p,slope=slope,int=int))
  
  #plot SAE
  graphics::par(fig=c((ylabwd)/totwd,
                      (ylabwd+panwd)/totwd,
                      (xlabht+0*(panht+gap))/totht,
                      (xlabht+0*(panht+gap)+panht)/totht),
                mai=c(0,0,0,0),mgp=c(3,0.75,0),new=TRUE)
  ylimits<-range(y)
  ylimits[2]<-ylimits[2]+.2*diff(ylimits)
  graphics::plot(x,y,
                 xlim=xlimits,ylim=ylimits,
                 xaxt="n")
  axis(side=1,labels=TRUE)
  mtext("SAE",side=2,line=1.8)
  mtext(divmeasure,side=1,line=1.8)
  text(xlimits[1],ylimits[2],
       paste0("Rsq=",round(Rsq,3),"; p=",round(p,3),"; slope=",round(slope,3)),
       adj=c(0,1))
  abline(a=int,b=slope)
  
  #finish up the plotting return numeric results
  grDevices::dev.off()
  return(numres)
}

MakeLeiPlots(leires,divmeasure="richness1",plotloc=paste0(resloc,"VsRichness1.pdf"))
MakeLeiPlots(leires,divmeasure="richness2",plotloc=paste0(resloc,"VsRichness2.pdf"))
MakeLeiPlots(leires,divmeasure="shannon1",plotloc=paste0(resloc,"VsShannon1.pdf"))
MakeLeiPlots(leires,divmeasure="shannon1",plotloc=paste0(resloc,"VsShannon2.pdf"))
MakeLeiPlots(leires,divmeasure="simpson1",plotloc=paste0(resloc,"VsSimpson1.pdf"))
MakeLeiPlots(leires,divmeasure="simpson2",plotloc=paste0(resloc,"VsSimpson2.pdf"))
MakeLeiPlots(leires,divmeasure="invsimpson1",plotloc=paste0(resloc,"VsInvSimpson1.pdf"))
MakeLeiPlots(leires,divmeasure="invsimpson2",plotloc=paste0(resloc,"VsInvSimpson2.pdf"))

#***Some additional plots to support some math thinking Reuman did on 2022 04 21

pdf(file=paste0(resloc,"VRvsDiversity.pdf"))
plot(leires$shannon1,leires$varrat,type="p",xlab="shannon 1",ylab="variance ratio")
dev.off()
h<-cor.test(leires$shannon1,leires$varrat) #highly significant
saveRDS(h,paste0(resloc,"CorellationTestShannon1VsClassicVR.Rds"))

#***Now do some clustering to investigate possible causes of CPE going down as diversity goes up.
#You have to have the clvr package installed, ask Dan. Trial with the first subject.

sdat<-dat[[20]]

#make the data for this subject into the format expected by the clvr package
sdat<-t(as.matrix(sdat))

#do some clustering - get raw results
clustres<-clustvr(dat=sdat,matopt="vrclassic")
names(clustres)
length(clustres$clusres)
lcr<-length(clustres$clusres)
clustres$clusres[[lcr]]
sum(clustres$clusres[[lcr]]$Clust==1)
sum(clustres$clusres[[lcr]]$Clust==2)
sum(clustres$clusres[[lcr]]$Clust==3)
sum(clustres$clusres[[lcr]]$StrMem[clustres$clusres[[lcr]]$Clust==1])
sum(clustres$clusres[[lcr]]$StrMem[clustres$clusres[[lcr]]$Clust==2])
sum(clustres$clusres[[lcr]]$StrMem[clustres$clusres[[lcr]]$Clust==3])
clustres$modres
sum(clustres$clusres[[3]]$StrMem)
summary(clustres)
print(clustres)

#apply getmat
getmatres<-getmat(clustvr_obj=clustres,core_thresh=NA,scaleroot=3,plotopt="yes")
names(getmatres)
getmatres$node_reorder
getmatres$conf_info

getmatres$mat_agg_pos
getmatres$mat_agg_neg
getmatres$mat_agg_vrcf

getmatres$mat_core_pos
getmatres$mat_core_neg
getmatres$mat_core_vrcf

#try getimp
gires<-getimp(clustres)
class(gires)
head(gires)

#now try cor, briefly
clustres<-clustvr(dat=sdat,matopt="cor")
getmatres<-getmat(clustvr_obj=clustres,core_thresh=NA,scaleroot=1,plotopt="yes")
#Looks like cluster 4 and the non-core species in cluster 5 might be negatively correlated 
#with other species, but otherwise I see a lot of synchrony. I wonder if those species
#are the also the ones that are not very variable?

#To help answer a version of that question, how about just looking at the correlation
#matrix and also the matrix consisting of geometric means of species variances and seeing
#how they might be related to ach other?
cm<-cor(t(sdat))
vars<-apply(FUN=var,X=sdat,MARGIN=1)
varmat<-sqrt(vars %o% vars)
cm<-cm-diag(diag(cm))
varmat<-varmat-diag(diag(varmat))
plot(varmat,cm,type="p")
plot(log10(varmat),cm,type="p")
cor.test(as.vector(varmat),as.vector(cm),method="spearman")
cor.test(as.vector(varmat),as.vector(cm),method="pearson")
#You'd have to do matrix regression to get meaningful p-values, but
#certainly looks like there is a strong tendency for the pairs of 
#species exhibiting negative correlations with each other to only
#be the ones with stupidly small geomtric means of variances. So it 
#appears there is no real compensatory dynamics.

#***Now try to get some summary stats for all subjects

allcf<-c()
for (counter in 1:20)
{
  sdat<-dat[[counter]]
  sdat<-t(as.matrix(sdat))
  
  cr<-clustvr(dat=sdat,matopt="vrclassic")
  gmr<-getmat(clustvr_obj=cr,core_thresh=NA,scaleroot=1,plotopt="no")
  allcf<-c(allcf,gmr$conf_inf[4])
}
#Looks like everyone but the 9th ad the 20th subjects are showing overwhelming synchrony
#
#When you use some of code from the previous *** section on the 9th subject, you see much
#more noticeable compensatory dynamics, and you see much more negative correlation for 
#pairs of species which are jointly substantially variable. That subject actually has the lowest
#phi_LdM of all the subjects, but has high Vsyn so has comparable Vcom with everyone else.
#
#The 20th subject is really less like 9 and more like the others, though maybe a bit less so.

