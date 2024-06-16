mydata <- read.csv("mydata.csv",header = T) ##File path##

###NRI###
library(survIDINRI)


indat <- mydata[, c("time","status")]
x_old <- mydata[,c("age","albumin","edema","protime")]
x_new <- mydata[,c("age","albumin","edema","protime","bili")]
set.seed(1234)
x<-IDI.INF(indat,covs0 = as.matrix(x_old),
           covs1=as.matrix(x_new), 
           t0=median(mydata[,c("time")]),
           npert = 300)
IDI.INF.OUT(x)

###RCS plot###
library(rms)
library(ggplot2)

y<-c(2,3) ### time and status ###

rcs_var <- c(8) ### main variable ###

co_var <- c(4,5,7) ### continuous covariates ###

class_var <- c(6) ### categorical covariates ###

ymax=0.15 ###  maximum value of the Y-axis (right) ###

per=0.05 ### ratio of Y-axis (right) :  Y-axis (left) ###

knot=5 ### number of nodes ###

ref=5 ### reference value ###

rcs_plot <- function(mydata,y,rcs_var,co_var,class_var,ymax,per,ref,knot)
{
  
  
  for (c_col in class_var) 
  {
    mydata[,c_col] <- as.factor(mydata[,c_col])
  }
  
  dd <- datadist(mydata)
  dd$limits[2,rcs_var] <- ref
  
  options(datadist=dd)
  
  fullformula <- as.formula(paste0('Surv(',paste(colnames(mydata[y]),collapse=","),')~',paste(rep("rcs(",1), colnames(mydata[rcs_var]),collapse=") + ", sep = ""),
                                   ",c(",knot,"))", paste(rep("+",length(c(co_var,class_var))),colnames(mydata[c(co_var,class_var)]), collapse="")))
  
  fit <- cph(fullformula,data = mydata) 
  pr <- anova(fit)
  pr <- pr[2,3]
  
  HR <- Predict(fit,fun=exp,ref.zero = TRUE,np=200) 
  HR <- HR[HR$.predictor.==colnames(mydata[rcs_var]),]
  hr_p <- HR[HR$.predictor.==colnames(mydata[rcs_var]),c(1,(length(c(rcs_var,co_var,class_var))+1):(ncol(HR)-1))]

  a <- mydata$bili
  b <- HR$bili
  p<- ggplot()+
    stat_density(data=mydata,aes(x=a,fill="#ddeff6"),position='identity',size=1,adjust =4)+
    scale_fill_manual(values=c("#ddeff6")) +
    geom_line(data=HR, aes(x=b,yhat*per),linetype=1,size=1,alpha = 0.9,colour="red")+
    geom_ribbon(data=HR, aes(x=b,ymin = lower*per, ymax = upper*per),linetype=2,size=1,alpha = 0.3,colour="black",fill="#FFFFFF")+
    labs(x=NULL,y=NULL)+
    theme_classic()+
    geom_hline(yintercept=per, linetype=8,size=0.5)+
    theme(legend.position = 'none')+
    theme(axis.line.x=element_line(linetype=1,color="black",size=1))+
    scale_y_continuous(sec.axis = sec_axis(~./per),position = "right",limits = c(0, ymax))+
    theme(axis.line.y=element_line(linetype=1,color="black",size=1))+
    theme(axis.text.x = element_text(family="serif", face="bold", color="black", size=12))+
    theme(axis.text.y = element_text(family="serif", face="bold",color="black", size=12))+
    theme(axis.ticks.x=element_line(color="black",size=3,lineend = 1))+
    theme(axis.ticks.y=element_line(color="black",size=3,lineend = 1))+
    theme(axis.title.x=element_text(family="serif", vjust=1, size=18,face = "bold"))+
    theme(axis.title.y=element_text(family="serif", vjust=1, size=18,face = "bold"))+
    theme(text =element_text(family="serif"))+
    theme(title=element_text(family="serif", size=12,face="bold"))+
    theme(panel.grid.major =element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.margin = margin(1, 1, 1, 1, "cm"))
  
  return(list(plot=p,p=pr))
  
}

rcs_plot(mydata,y,rcs_var,co_var,class_var,ymax,per,ref,knot)
