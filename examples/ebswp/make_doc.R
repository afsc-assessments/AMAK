rm(list=ls())
library(r4ak)
library(sa4all)
sa4all::draft(authors = "Kelli F. Johnson", create_dir = TRUE, type="ak")
setwd("doc")
bookdown::render_book("00a.Rmd", clean = FALSE, output_dir = getwd())
setwd("..")
source("R/prelims.R")
#source("R/compareRuns.r")
#        Read in the output of the assessment
#base <- readList("runs/base/For_R.rep")
base <- read_admb("runs/base/amak")
A<-base

names(base)
base$run_name
base$fit
names(base$fit)
#modlyr   <- readList("2019_Final/For_R.rep")
pm <- read_admb("~/_mymods/ebswp/runs/base/pm")

M <- list(base,pm)
M <- list(base)
names(M) <- c("AMAK")
names(M) <- c("AMAK","Pollock Model")
names(pm)

plot_survey(M,which_survey=c(2),xlim=c(1990,2020))
plot_survey(M,which_survey=c(5),xlim=c(1990,2020))
plot_survey(M,which_survey=c(1,2))
plot_agefits()
plot_sel(M,styr=1990)
plot_sel(M,styr=1990,type="survey")
plot_mnage(M)
plot_recruitment(M,xlim=c(1989.5, 2021.5))
plot_ssb(M,xlim=c(1963, 2021))
plot_srr(M)

	
length(M)
#=====================
#   SSB 
#=====================
df  <- data.table(Model = "base", base$SSB )
#df <- rbind(df, data.table(Model="lastyr",modlyr$SSB))

#df  <- data.table(Model = "Non-parametric", mod16.0b$SSB )
#df <- rbind(df, data.table(Model="3-parameter",mod3par$SSB))
# for (i in 2:3) df <- rbind(df, data.table(Model=paste0("Model ",i),lstOuts[[i]]$SSB))
names(df) <- c("Model","yr","SSB","SE","lb","ub")
bdf <- filter(df,yr>1960,yr<=2021) %>% arrange(yr);bdf

p1 <- ggplot(data=bdf,aes(x=yr,y=SSB,alpha=0.3,color=Model)) + scale_y_continuous(limits=c(0,4000)) + ylab("Spawning biomass") + 
          xlab("Year") +  theme_few(base_size=19) + geom_line(data=bdf,aes(x=yr,y=SSB,color=Model)) +
          geom_ribbon(data=bdf ,aes(x=yr,y=SSB,ymin=lb,ymax=ub,fill=Model),alpha=.3)  + guides(alpha=FALSE,col=FALSE) ;p1
print(p1)
#=====================
#   Rec 
#=====================
rdf <- cbind(data.table(base$R,"base"))
names(rdf) <- c("yr","R","se","lb","ub","case")
rdf  <- rdf[yr>1990&yr<2020,.(yr,R=R,lb=lb,ub=ub,case)]
mnR <- mean(base$R)
dodge <- position_dodge(width=0.8)
ggplot(rdf,aes(x=yr-1,y=R,fill=case)) + xlab("Year class") + ylab("Age 1 recruits (thousands)") + #ylim(c(0,18000)) +
       geom_bar(width=0.75,position="dodge",stat="identity",color="black") + 
       scale_x_continuous(breaks=seq(1990,2018,2)) +
       geom_errorbar(aes(ymin=lb,ymax=ub),width=.3,colour="blue",position=dodge) + mytheme + geom_hline(aes(yintercept=mnR))

#=====================
#    Fishing mortality
#=====================
library(data.table)
M <- base
fdf <- data.table(M$F_fsh_1)
fdf$C_B <- M$Obs_catch_1/M$TotBiom[1:length(M$Obs_catch_1),2]
names(fdf) <- c("Year","MeanF","F","C_B")
fdf.m <-melt(fdf,id="Year",value="Estimate",variable="Type")
fdf.m
ggplot(fdf.m,aes(x=Year,y=Estimate,color=Type)) + xlab("Year") +  
       geom_line(size=2.) + scale_x_continuous(breaks=seq(1977,2019,3)) + mytheme  
       geom_hline(aes(yintercept=mnR),linetype="dashed") + theme(legend.position="none")


#=====================
# Fit to survey data
#=====================
AgeFitsSrv(base,rec_age=1,case_label=captyr)

mdf <- .get_bts_df(base)
mdf2 <- mdf %>% filter(!is.na(obs))
ggplot(mdf,aes(x=year,y=pre)) + geom_line(width=2,color="blue") + .THEME + geom_point(data=mdf2,aes(x=year,y=obs),size=2,color="red") + expand_limits(y = 0) + 
                            geom_errorbar(data=mdf2,aes(x=year,ymax=ub,ymin=lb),width=.5) + ylab("Survey biomass (t)")+ xlab("Year") + 
                            scale_x_continuous(breaks=seq(1970,2019,2), limits=c(1990,2019)) 
IndexFit(base,yf=1990,yl=2019,f=1,main=captyr,ylab="Survey biomass (t)")
AgeFitsSrv(modlyr,rec_age=1,case_label=caplyr)

AgeFitsSrv(base,rec_age=1,case_label="16.0b")

#=====================
# Fit to fishery data
#=====================
AgeFits(base,rec_age=1,case_label="Fishery")
#AgeFits(mod20,rec_age=1,case_label="Model 20.0")
#AgeFits(modlyr,rec_age=1,case_label=caplyr)

dt <- data.table(modlyr$Obs_Survey_1[,1:4],base$Obs_Survey_1[,3])
names(dt) <- c("Year","Observed","Model_lastyr","sd","Model_16.0b")
dt <- data.table(dt)[Year>1986,.(Year, Observed, Model_lastyr,Model_16.0b, lb=Observed-1.96*sd,ub=Observed+1.96*sd),]
dtp <-melt(dt,id="Year")
dtp
ggplot(dt,aes(x=Year,y=Model_16.0b) ) + geom_point(data=dtp[variable=="Observed"],size=3) + geom_line(data=dtp[variable!="Observed"]) + ylim(c(0,1800000)) + labs(x="Year",y="Survey biomass") + mytheme #+
,aes(x=Age,y=value,colour=variable)) 
dt[variable!="ub"&variable!="lb"] %>% ggplot(aes(x=Year,y=value,col=variable)) + geom_point(data=dt[variable=="Observed"],size=3) + geom_line(data=dt[variable!="Observed"]) + ylim(c(0,1800000)) + labs(x="Year",y="Survey biomass") + mytheme #+
            geom_line(data=)

#=====================
#Selectivity
#=====================
tt <- data.frame(base$sel_fsh_1[,3:13]) %>% mutate(mak = do.call(pmax, (.))) 
tt
df <- data.frame(base$sel_fsh_1[,2], base$sel_fsh_1[,3:13] ) ; names(df) <- c("yr",1:11)
df %>% mutate(max=max(c_across(2:12))) %>% mutate(2:12/max)

df <- data.frame(mod3par$sel_fsh_1[,2:13] ); names(df) <- c("yr",1:11)
df <- data.frame(mod3par$sel_fsh_1[,2:13] ); names(df) <- c("yr",1:11)
df
sdf <- gather(df,age,sel,2:12) %>% filter(yr>1976) %>% mutate(age=as.numeric(age)) #+ arrange(age,yr)
max(sdf$sel )
ggplot(sdf,aes(x=age,y=as.factor(yr),height = sel)) + geom_density_ridges(stat = "identity",scale = 5.8, alpha = .9,color="blue",fill="yellow",size=.5)+ xlim(c(1,11))+ ylab("Year") + xlab("Age (years)") + scale_y_discrete(limits=rev(levels(as.factor(sdf$yr)))) + theme_few()
#ggplot(sdf,aes(x=age,y=as.factor(yr),height = sel)) + geom_density_ridges(stat = "identity",scale = 5.8, alpha = .4,color="blue",fill="yellow",size=.5)+ xlim(c(1,11))+ mytheme + ylab("Year") + xlab("Age (years)") + scale_y_discrete(limits=rev(levels(as.factor(sdf$yr))))
