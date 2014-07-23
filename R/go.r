require(PBSadmb)
fndir<-"c:/users/jim/documents/dropbox/R_common/"
outdir="C:/Users/jim/Documents/dropbox/models/atka/prg/"
Figdir="C:/Users/jim/Documents/dropbox/models/atka/Figs/"
outdir="../prg/"
getwd()
source(paste(fndir,"adfunctions.r",sep=""))
source(paste(fndir,"adfunctions2.r",sep=""))
getwd()
names(mod1)
mod2.2$Like_Comp
mod1$Like_Comp
mod1$Like_Comp_names
SSB_Lastyr=read.table("clipboard")
mod2.2=readList(paste(outdir,"for_R.rep",sep=""))
mod1.2=readList(paste(outdir,"for_R.rep",sep=""))
mod0=readList(paste(outdir,"arc\\mod0_R.rep",sep=""))
mod1=readList(paste(outdir,"arc\\mod1_R.rep",sep=""))
mod2=readList(paste(outdir,"arc\\mod2_R.rep",sep=""))
mod0.1=readList(paste(outdir,"arc\\mod0.1_R.rep",sep=""))
mod1.1=readList(paste(outdir,"arc\\mod1.1_R.rep",sep=""))
mod2.1=readList(paste(outdir,"arc\\mod2.1_R.rep",sep=""))
dev.off()
detach(dat)
mod2.2$tau
names(mod1 )
pdf(paste(Figdir,"SurvFit.pdf",sep=""),width=9, height=7)
IndexFit(mod2,yf=1989,yl=2013,f=1,main="Model 2", ,ylab="Survey biomass (t)")
IndexFit(mod2.2,yf=1989,yl=2013,f=1,main="Model 2.2", ,ylab="Survey biomass (t)")
IndexFit(mod1.2,yf=1989,yl=2013,f=1,main="Model 2.2", ,ylab="Survey biomass (t)")
IndexFit(mod1,yf=1989,yl=2013,f=1,main="Model 1", ,ylab="Survey biomass (t)")
IndexFit(mod3,yf=1989,yl=2013,f=1,main="Model 3", ,ylab="Survey biomass (t)")
rec_age=1
mod1.2$q_1
mod2$q_1
p.rec.hist(mod1,main="Model 1",ylab="Recruitment",fy=1977,ly=2013,plotmean="F",ylim=c(0,2700))
p.rec.hist(mod1.2,main="Model 1.2",ylab="Recruitment",fy=1977,ly=2013,plotmean="F",ylim=c(0,2700))
p.rec.hist(mod2,main="Model 2",ylab="Recruitment",fy=1977,ly=2013,plotmean="F",ylim=c(0,2700))
p.rec.hist(mod2.2,main="Model 2.2",ylab="Recruitment",fy=1977,ly=2013,plotmean="F",ylim=c(0,2700))
p.rec.hist(mod2.2,main="Model 2.2",ylab="Recruitment",fy=1977,ly=2013,plotmean="F",ylim=c(0,2700))
lines(1977:2013,rep(mean(mod2$R[,2]),37),lty=3,lwd=3,col="blue")
# Survey fit
p.sur.stk(mod2,f=1)
p.catch.fit(mod2,f=1,y)
plot(mod12$SSB,typ="b",ylab="SSB",xlab="Year",ylim=c(0,.85e6))

pdf(paste(Figdir,"TotB.pdf",sep=""),width=9, height=7)
p.biom.pol(mod2,typ="SSB",main="Model 2",new=F,fy=1977,ly=2013)
p.biom.pol(mod2,typ="TB",main="Model 2",new=F,fy=1977,ly=2013)
lines(mod1$TotBiom[,1],mod1$TotBiom[,2],col="black",lty=2,lwd=3)
p.biom.pol(mod1,typ="TB",main="Model 1",new=F,fy=1977,ly=2013)
dev.off()

styr=1977
p.stock.rec(mod2,main="Atka mackerel, model 2")
p.stock.rec(mod2.2,main="Atka mackerel, model 2")
p.stock.rec(mod1.2,main="Atka mackerel, model 1.2")
#p.eff.n(mod1,typ="F",f=1)
#p.eff.n(mod1,typ="S",f=1)
p.eff.n(mod2,typ="F",f=1,main="Model 2")
p.eff.n(mod2.2,typ="F",f=1,main="Model 2")
p.eff.n(mod1,typ="F",f=1,main="Model 1")
p.eff.n(mod1.2,typ="F",f=1,main="Model 1.2")
p.eff.n(mod2,typ="S",f=1,main="Model 2")
#AgeFitsSrv(mod1,rec_age=1,case_label="2013 assessment")
#AgeFits(mod1,rec_age=1,case_label="2013 assessment")
AgeFitsSrv(mod2,rec_age=1,case_label="2013 assessment model 2")
AgeFits(mod2,rec_age=1,case_label="2013 2 assessment")
AgeFits(mod2.2,rec_age=1,case_label="2013 2.2 assessment")
AgeFits(mod1.2,rec_age=1,case_label="2013 1.2 assessment")
dev.off()
detach(dat)
# spawning biomass and last year's estimates 
p.biom.pol(mod1,typ="TB",new=F)
p.biom.pol(mod2.2,typ="TB",new=F)
p.biom.pol(mod1.2,typ="TB",new=F)
  full.f(mod1,f=1)
lines(1977:2013,mod1$F_fsh_1[,3],lwd=2)
names(mod1)

# Rec
p.biom.stk(mod1,typ="R")
# Numbers at age
p.bub.age(mod1,siz=600)
dev.off()


p.rec.hist(mod0,fy=1977,ly=2013,main="Model 0")
p.rec.hist(mod1,fy=1977,ly=2013,main="Model 1")
p.rec.hist(mod2,fy=1977,ly=2013,main="Model 2")
lines(modsigmaR$R[,1],modsigmaR$R[,2],col="purple",lwd=2)
)
dev.off()

p.biom.pol(mod0,typ="SSB",new=F)
p.biom.pol(mod1,typ="SSB",new=F)
p.biom.pol(mod2,typ="SSB",new=F,main="Model 2".ly=2013)
lines(mod0$SSB[,1],mod0$SSB[,2],col="red")
lines(mod1$SSB[,1],mod1$SSB[,2],col="green")

#++++SSB CV figure=========================
plot(d3$SSB[,1],d3$SSB[,3]/d3$SSB[,2],typ="l",lty=2,ylim=c(0,.4),ylab="CV on spawning biomass",xlab="Year",cex.lab=1.4)
lines(d3$SSB[,1],d1$SSB[,3]/d1$SSB[,2],lwd=2)
lines(d3$SSB[,1],d2$SSB[,3]/d2$SSB[,2],lty=1)
lines(d3$SSB[,1],d7$SSB[,3]/d7$SSB[,2],lty=3)
legend(1968,.4, c("sigma_d=0.1","sigma_d=0.2","sigma_d=0.3", "sigma_d=1.0"),lty=c(1,1,2,3),lwd=c(2,1,1,1))

lines(modvsel$SSB[,1],modvsel$SSB[,2],col="red")
lines(mod2$SSB[,1],mod1$SSB[,2],col="purple",lwd=2)
lines(mod2$SSB[,1],mod2$SSB[,2],col="green",lwd=2)
lines(mod2$SSB[,1],mod3$SSB[,2],col="pink",lwd=2)
lines(mod2$SSB[,1],mod4$SSB[,2],col="black",lwd=2)

lines(modestM$SSB[,1],modestM$SSB[,2],col="salmon",lwd=2)
lines(modsigmaR$SSB[,1],modsigmaR$SSB[,2],col="purple",lwd=2)
modestM$Index_Q_1
names(mod1)

#++++Selectivity figure=========================
SelLastYr=c(0.00228954, 0.0333104,	0.337153,	0.879725,	1.13117,	1.32736,	1.619,	1.61509,	1.46665,	1.29413,	1.29413)
SelLastYr[[1]][1:11]
d1=readList(paste(outdir,"arc\\ds.1_R.rep",sep=""))
d2=readList(paste(outdir,"arc\\ds.2_R.rep",sep=""))
d3=readList(paste(outdir,"arc\\ds.3_R.rep",sep=""))
d7=readList(paste(outdir,"arc\\ds1.0_R.rep",sep=""))
q1.3=readList(paste(outdir,"arc\\q1.3_R.rep",sep=""))
plot(d1$N[36,3:12],typ="p",pch=19)
lines(d7$N[36,3:12])
k=36
plot(1:11,d3$sel_fsh_1[k,3:13]/max(d3$sel_fsh_1[k,3:13]),typ="l", ylab="Selectivity",xlab="Age",lwd=3, cex.lab=1.8)
lines(d1$sel_fsh_1[k,3:13]/max(d1$sel_fsh_1[k,3:13]),lty=2)
lines(d7$sel_fsh_1[k,3:13]/max(d7$sel_fsh_1[k,3:13]),lty=3)
lines(q1.3$sel_fsh_1[k,3:13]/max(q1.3$sel_fsh_1[k,3:13]),lty=3)
lines(SelLastYr/max(SelLastYr),lty=4)
SelLastYr
abline(h=.5)
legend(1,.95, c("sigma_d=0.3","sigma_d=0.1","sigma_d=1.0", "2011 Assessment"),lty=1:4,lwd=c(3,1,1,1,1))
#END ofSelectivity figure=========================


IndexFit(mod2,yf=1989,yl=2012,f=1,main="Model 2",ylab="Survey biomass (t)")
IndexFit(mod1,yf=1989,yl=2012,f=1,main="Model 1",ylab="Survey biomass (t)")
lines(mod1$TotBiom[,1],mod1$TotBiom[,2],lty=2,lwd=2)
pdf("Atka_2013.pdf",width=9, height=7)
Mntns(mod0,"Model 0")
Mntns(mod1,"Model 1")
Mntns(mod2,"Model 2")
mod1$Q_Survey_1
mod2$Q_Survey_1
dev.off()

pdf(paste(Figdir,"Selectivity.pdf",sep=""),width=7, height=11)
par(mfrow=c(1,2))
sel.age.mountain(mod1, f=1, new="F",typ="F", xvec=c(1:11),main="Model 1")
sel.age.mountain(mod2, f=1, new="F",typ="F", xvec=c(1:11),main="Model 2")
sel.age.mountain(mod2.2, f=1, new="F",typ="F", xvec=c(1:11),main="Model 2.2")
sel.age.mountain(mod1.2, f=1, new="F",typ="F", xvec=c(1:11),main="Model 2.2")
dev.off()
par(mfcol=c(1,1),mar=c(5,5,4,2) + 0.1)  

p.catch.fit(mod1,f=1,ylab="Catch biomass (t)",ylim=c(0,120000))
                 
spwn_ratio(mod1,main="Model 1")
cont.f.age.res(mod1, typ = "F", f = 1, lage = 1, hage = 11, cl ="COL")
p.bub.age(mod1,lage=1,hage=11,fy=1977,ly=2011,siz=100)
detach(dat)
Plot_Phase()
Plot_Fspr()
AgeFits(mod2,f=1,case_label="2013 assessment",rec_age=1)
AgeFits(mod1,f=1,case_label="2013 assessment",rec_age=1)
                 
modsigmaR$R
                 xlab="Age",ylab="Year",zscale=2.5,new=F,cex.yax=1.,fy=1980)
detach(dat)
IndexFit(mod,yf=1980,yl=2010,f=2,main=main)

par(mfcol=c(1,1))

# show fit to catch biomass
CatchFit(mod2.2)

# show spawning biomass relative to population with no fishing
spwn_ratio(mod1,fy=1977,ly=2013) 
fix(spwn_ratio) 

detach(dat)
# example of writing multiple plots to pdf file:
#pdf("figs/agefits.pdf",width=9, height=7)
  #AgeFits(am1,f=1)
#dev.off()

# another example of writing multiple plots to pdf file:
#pdf("figs/survey_fit.pdf",width=7, height=9)
#Mntns(am1,"Model 1")
    # Indices(am1,"Model 1")
par(mfrow=c(1,1))
IndexFit(mod1,main="Model 1",yf=1990,ylab="Survey biomass (t)")
detach(dat) 
Plot_Fspr()

#spwn_ratio(am1) 
#plt_proj(am1)
#dev.off()

#=====================
# Retro
# get all retrospectives
#=====================
setwd(outdir)
getwd(outdir)
for (i in 0:9) {
  rn=paste("retro\\m2_r",i,"_R.rep",sep="")
  mn=paste("retro2",i,sep="")
  assign(mn,readList(rn))
  print(rn)
}

for (i in 0:9) {
  rn=paste("retro\\m1_r",i,"_R.rep",sep="")
  mn=paste("retro1",i,sep="")
  assign(mn,readList(rn))
  print(rn)
}
pdf(paste(Figdir,"Retro_Mods.pdf",sep=""),width=9, height=6)
p.biom.pol(retro10,typ="SSB",main="Model 1",new=F,fy=1977,ly=2013)
for (i in 1:9) {
  rn=paste("retro1",i,sep="");
  lines(get(rn)$SSB[,1],get(rn)$SSB[,2],col=i)
  lr=length(get(rn)$SSB[,1])
  points(get(rn)$SSB[lr,1],get(rn)$SSB[lr,2],pch=19,col=i)
}
p.biom.pol(retro20,typ="SSB",main="Model 2",new=F,fy=1977,ly=2013)
for (i in 1:9) {
  rn=paste("retro2",i,sep="");
  lines(get(rn)$SSB[,1],get(rn)$SSB[,2],col=i)
  lr=length(get(rn)$SSB[,1])
  points(get(rn)$SSB[lr,1],get(rn)$SSB[lr,2],pch=19,col=i)
}

plot(retro20$SSB[,1],rep(0,length(retro20$SSB[,1])),
     ylim=c(-.7,.7),
     ylab="Relative difference from terminal year",
     type="l",xlab="Year",lty=2,lwd=2)

dev.off()

p.biom.pol(retro2,typ="SSB",main="Model 1",new=F,fy=1977,ly=2013)
for (i in 1:9) {
  rn=paste("retro1",i,sep="");
  lines(get(rn)$SSB[,1],get(rn)$SSB[,2],col=i)
}
pdf(paste(Figdir,"Retro_Mod2.pdf",sep=""),width=7, height=9)
par(mfrow=c(2,1))
p.biom.pol(retro0,typ="SSB",main="Model 2",new=F,fy=1977,ly=2012)
#plot(retro0$SSB[,1],retro0$SSB[,2],ylim=c(0,550),      ylab="Spawning biomass (kt)",type="l",lwd=2,xlab="",lty=2)
ssb=1966:2013
retro0$R
names(retro1)
rrr=1977:2012
for (i in 0:10) {
  rn=paste("retro",i,sep="");
  lines(get(rn)$SSB[,1],get(rn)$SSB[,2],col=i)
  ssb=rbind(ssb,c(t(get(rn)$SSB[,2]),rep(NA,i)))
  rrr=rbind(rrr,c(t(get(rn)$R[,2]),rep(NA,i)))
  }
write.csv(ssb,"Atka_SSB.csv")
write.csv(rrr,"Atka_rec.csv")

system("atka_ssb.csv")
?write.csv
rrr
rep(NA,2)
plot(retro0$SSB[,1],rep(0,length(retro0$SSB[,1])),
     ylim=c(-.7,.7),
     ylab="Relative difference from terminal year",
     type="l",xlab="Year",lty=2,lwd=2)
for (i in 1:10) {
  rn=paste("retro",i,sep="");
  lines(get(rn)$SSB[,1],
        (get(rn)$SSB[,2]/retro0$SSB[1:(48-i),2])-1,col=i)
  }


dev.off()
p.biom.pol(mod2,typ="SSB",n.mod=1,main="Model 2",new=F,fy=1977,ly=2013)

p.biom.pol(mod2,typ="SSB",n.mod=2,mod1,main="Model 1",new=F,fy=1977,ly=2013)
lines(SSB_Lastyr[,1],SSB_Lastyr[,2]/2,lwd=2,col="red")

# Plot selectivity in multiple crappy panels
p.select.hist(mod1,typ="F",h="T",f=1,lage=1,hage=11,fy=1985,ly=2000)
# Plot selectivity in multiple crappy color grayscale 
c.select(mod1)
dev.off()  