install.packages("Bolstad")
install.packages("readxl")
install.packages("ggplot2")
library(ggplot2)
library("readxl")
library(Bolstad)
install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
install.packages('tidybayes')
install.packages('R2jags')
library(R2jags)
library(ggplot2)
library(tidyr)
library(tidybayes)
install.packages('pander')
install.packages('dplyr')
install.packages('stargazer')
library( pander )     # formatting tables
library( dplyr )      # data wrangling
library( stargazer )  # regression tables
install.packages('rpart')
library(rpart)
install.packages('randomForest')
install.packages('mcmc')
install.packages('brms')
install.packages('tidyverse')
install.packages('rstan')
install.packages('broom')
install.packages('sjstats')
yes

install.packages('sjPlot')
library(sjstats)
library(sjPlot)
library(randomForest)
library(mcmc)
library(brms)
library(tidyverse)
library(rstan)
library(broom)
install.packages('pander')
library(knitr)
if(!require('coda')) {
  install.packages('coda')
  library('coda')
}
knit2html("example.rmd")
# installing/loading the package:
if(!require(installr)) { install.packages("installr"); require(installr)} #load / install+load installr 

FDI <- read_excel("//Users/vuthihanhdung/Desktop/BAYES.xlsx", 
                  sheet = "BAYES")
attach(FDI)
summary(FDI)
FDI%>%ggplot(aes(x=FDI))+
  geom_density(alpha=0.5,fill="red",ol="black")+
  theme_bw(20)
stan_func1 <- "
  real stan_func1(real x, real l, real b, real t){
    if(x > t){
      return(.5+inv_logit(l)/2*(1-exp(-exp(b)*(x-t))));
    } else{
      return(.5);
    }
  }
"

# Save code snippet so that it can be integrated into our model with `brms`
stanvar_func1 <- stanvar(scode = stan_func1, block = "functions")
#FDI
y=log(FDI$FDI)
x1=log(FDI$GDP)
x2=(FDI$INF)
x3=(FDI$UNE)
df=data.frame(y,x1,x2,x3)
mod_formula = y ~ x1 + x2 + x3 
summary(mod_formula)
#xac dinh phan phoi tien nghiem  cho cac tham so
outcome_prior=brmsfamily(family='gamma',link='log')
mod_prior=get_prior(data=df,
                    formula=bf(mod_formula),
                    family=outcome_prior)
mod_prior
#xac dinh hau nghiem
set.seed(123)
rstan_options(auto_write = TRUE)
options(mc.cores=parallel::detectCores())
#kiem tra pham chat chuoi mcmc
#tinh dai dien
bayesm1=brm(data=df,formula = mod_formula,family='Gamma',prior=mod_prior,chain=5,warmup=1000)
plot(bayesm1)
bayesm1$fit
#tinh chinh xac
mcmcvector <- as.mcmc(bayesm1) #trich xuat chuoi mcmc tu bayesm1 model
traceplot(mcmcvector)
effectiveSize(mcmcvector)#chung cho cac chuoi
lapply(mcmcvector,effectiveSize)
#Kiem dinh gia thuyet Bayes
hypothesis(bayesm1,"x1>0",alpha=0.01)
hypothesis(bayesm1,"x2>0",alpha=0.01)
hypothesis(bayesm1,"x3>0",alpha=0.01)






#duoi tim thủ công

#prior intercept(Mu for Gamma)
data_frame(t_student= rstudent_t(n=1000,df=3,mu=22.6,sigma=2.5),
           gaussian= rnorm(n=1000,mean=22.6,sd=2.5))%>%
  gather(1,2,key="prior",value="value")%>%
  ggplot(aes(x=value,fill=prior))+
  geom_density(alpha=0.3,col="black")+
  theme_bw()
#prior for beta1
data_frame(t_student= rstudent_t(n=1000,df=3,mu=3.1,sigma=2.5),
           gaussian= rnorm(n=1000,mean=0,sd=2.5))%>%
  gather(1,2,key="prior",value="value")%>%
  ggplot(aes(x=value,fill=prior))+
  geom_density(alpha=0.3,col="black")+
  theme_bw()
#prior for beta2
data_frame(gamma=rgamma(n=1000,shape=0.01,scale=0.01))%>%

  ggplot(aes(x=gamma))+
  geom_density(alpha=0.3,col="violet")+
  theme_bw()
mod_prior=c(prior(student_t(3,0,2.5),class="b"),
            prior(student_t(3,0,2.5),class="b",coef="x1"),
            prior(student_t(3,0,2.5),class="b",coef="x2"),
            prior(student_t(3,0,2.5),class="b",coef="x3"),
            prior(student_t(3,22.6,2.5),class="Intercept"),
            prior(gamma(0.01, 0.01),class="shape"))
mod_prior   
mod_control=list(adapt_delta=0.99,
                 max_treedepth=30)  
mod <- brm(data=df,
           formula=bf(mod_formula),
           family=Gamma(link='log'),
           prior=mod_prior,
           control=mod_control,
           seed=123,
           iter=2000,
           warmup=1000,
           chains=1)
stancode(mod)
print(mod,digit=5)

beta=exp(1.96089)
beta1
beta1=exp(0.04393)
beta2=exp(0.00130)
beta3=exp(0.01152)


HDIF= function( sampleVec,credMass=0.975 ) {
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}

SUMK=function(paramSampleVec,compVal=NULL , ROPE=NULL , credMass=0.975) {
  meanParam = mean( paramSampleVec )
  medianParam = median( paramSampleVec )
  dres = density( paramSampleVec )
  modeParam = dres$x[which.max(dres$y)]
  hdiLim = HDIF( paramSampleVec , credMass=credMass )
  if ( !is.null(compVal) ) {
    pcgtCompVal = ( 100 * sum( paramSampleVec > compVal ) 
                    / length( paramSampleVec ) )
  } else {
    compVal=NA
    pcgtCompVal=NA
  }
  if ( !is.null(ROPE) ) {
    pcltRope = ( 100 * sum( paramSampleVec < ROPE[1] ) 
                 / length( paramSampleVec ) )
    pcgtRope = ( 100 * sum( paramSampleVec > ROPE[2] ) 
                 / length( paramSampleVec ) )
    pcinRope = 100-(pcltRope+pcgtRope)
  } else { 
    ROPE = c(NA,NA)
    pcltRope=NA 
    pcgtRope=NA 
    pcinRope=NA 
  }  
  return( c( Mean=meanParam , Median=medianParam , Mode=modeParam , 
             HDIlevel=credMass , LL=hdiLim[1] , UL=hdiLim[2] , 
             CompVal=compVal , PcntGtCompVal=pcgtCompVal , 
             ROPElow=ROPE[1] , ROPEhigh=ROPE[2] ,
             PcntLtROPE=pcltRope , PcntInROPE=pcinRope , PcntGtROPE=pcgtRope ) )
}

summaryKruschke=function(MCMC,compVal=NULL, rope=NULL,credMass=NULL){
  summaryInfo = NULL
  summaryInfo = cbind(summaryInfo, "Estimated"= SUMK(MCMC,
                                                     compVal=compVal,
                                                     ROPE=rope,credMass=credMass))
  return(summaryInfo)
}

summaryKruschke(MCMC =exp(temp_df[[1]]) ,compVal=0,rope=c(1,5),credMass=0.95)

tidyMCMC(mod,
         conf.int=TRUE,
         conf.method="HPDinterval")%>% pander()
mod %>% spread_draws(b_x1,
                  b_x2,
                  b_x3)%>% . [,-c(1,3)]%>%
  mutate(fragment=factor(rep(c(1:10),e=nrow(.)/10)))-> post_df
View(post_df) 
#HDI
para_list <- post_df%>%select(-c(fragment, .iteration))%>%colnames()
out_df=tibble(Parameter=rep(NA,13),
              Metric=rep(NA,13),
              Estimate=rep(NA,13))

for(para in c(2:4)){
  temp_df=post_df%>%select(para)
  out_mat=summaryKruschke(MCMC = exp(temp_df[[1]]),
                          compval=2,
                          ROPE=c(1,1.5),
                          credMass=0.95)%>%as.matrix()
  out_df=bind_rows(out_df,
                   tibble(Parameter=colnames(temp_df),
                          Metric=rownames(out_mat),
                          Estimate=as.vector(out_mat)))%>%
  na.omit()
}
out_df%>%split(.$Parameter)
MCMC = exp(temp_df[[1]])
para
#bayes_factor
thres=seq(from=1,to=5,by=0.1)
bf_df=tibble(Parameter=rep(NA,1),
             threshold=rep(NA,1),
             Bayes_factor=rep(NA,1))                  
for(para in c(2:4)){
  for(i in (1:21)){
    temp_df = post_df%>%select(para)%>%exp(.)
    hyp=paste(colnames(temp_df),">",thres[i],sep=" ")
    bf=brms::hypothesis(temp_df,hyp,alpha=0.05)
    bf_df=bind_rows(bf_df,
                    tibble(Parameter = colnames(temp_df),
                           threshold=thres[i],
                           Bayes_factor = bf$hypothesis$Evid.Ratio))%>%na.omit()
  }
}
bf_df%>%
  ggplot(aes(x=threshold,
             y=Bayes_factor,
             fill=Bayes_factor))+
  geom_path()+
  geom_point(show.legend = F,
             size=3,
             shape=21,
             col="black")+
  theme_bw(10)+
  geom_text(aes(label=round(bf_df$Bayes_factor,2)),
            col="black",
            nudge_y=1,
            nudge_x=0,
            show.legend= F,
            angle= 45,
            size=3)+
  scale_fill_gradient(low="gold",high="red")+
  geom_hline(yintercept=30,col="red",linetype=2)+
  geom_vline(xintercept=2,col="blue",linetype=2)+
  facet_wrap(~Parameter,ncol=2,scale="free")
stop()




