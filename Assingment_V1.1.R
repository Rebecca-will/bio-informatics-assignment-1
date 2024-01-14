if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("dyplr")
library(dplyr)
library(tidyverse)
library(ggplot2)
assignment_data=read.csv("Assignment_dataset (2).csv")
attach(assignment_data)

colnames(assignment_data)
##Assign values
Treatment_Arm_traz<-(Treatment_Arm[49:88])
IL1B_pre_Tras<-(IL1B_pre[49:88])
IL1B_pre_lap<-(IL1B_pre[1:48])
CX3CL1_pre_Tras<-(CX3CL1_pre[49:88])
CX3CL1_pre_lap<-(CX3CL1_pre[1:48])
TNFA_pre_Tras<-(TNFA_pre[49:88])
TNFA_pre_lap<-(TNFA_pre[1:48])
CCL20_pre_Tras<-(CCL20_pre[49:88])
CCL20_pre_lap<-(CCL20_pre[1:48])
##IL1B
var(IL1B_pre_lap)
var(IL1B_pre_Tras)
var.test(IL1B_pre_lap,IL1B_pre_Tras)
t.test(IL1B_pre_lap,IL1B_pre_Tras,var.equal=TRUE)
##CX3CL1
var(CX3CL1_pre_Tras)
var(CX3CL1_pre_lap)
var.test(CX3CL1_pre_lap,CX3CL1_pre_Tras)
t.test(CX3CL1_pre_lap,CX3CL1_pre_Tras,var.equal=TRUE)
##TNFA
var(TNFA_pre_Tras)
var(TNFA_pre_lap)
var.test(TNFA_pre_lap,TNFA_pre_Tras)
t.test(TNFA_pre_lap,TNFA_pre_Tras,var.equal=TRUE)
##CCL20
var(CCL20_pre_Tras)
var(CCL20_pre_lap)
var.test(CCL20_pre_lap,CCL20_pre_Tras)
t.test(CCL20_pre_lap,CCL20_pre_Tras,var.equal=TRUE)
##Boxplots
boxplot(IL1B_pre_lap,IL1B_pre_Tras,main='Pre-Treatment IL1B',names=c('lapatinib','trastuzumab'))
boxplot(CX3CL1_pre_lap,CX3CL1_pre_Tras,main='Pre-Treatment CX3CL1',names=c('lapatinib','trastuzumab'))
boxplot(TNFA_pre_lap,TNFA_pre_Tras,main='Pre-Treatment TNFA',names=c('lapatinib','trastuzumab'))
boxplot(CCL20_pre_lap,CCL20_pre_Tras,main='Pre-Treatment CCL20',names=c('lapatinib','trastuzumab'))
##Histo and QQplot
hist(IL1B_pre_lap , prob=T)
curve(dnorm(x,mean=mean(IL1B_pre_lap), sd=sd(IL1B_pre_lap)), add=T, col="red")
qqnorm(IL1B_pre_lap)
qqline(IL1B_pre_lap, col='red')
shapiro.test(IL1B_pre_lap)

hist(IL1B_pre_Tras , prob=T)
curve(dnorm(x,mean=mean(IL1B_pre_Tras), sd=sd(IL1B_pre_Tras)), add=T, col="red")
qqnorm(IL1B_pre_lap, main = 'IL1B_pre_Lap')
qqline(IL1B_pre_lap, col='red')
shapiro.test(IL1B_pre_Tras)
sd(IL1B)

hist(CX3CL1_pre_lap , prob=T)
curve(dnorm(x,mean=mean(CX3CL1_pre_lap), sd=sd(CX3CL1_pre_lap)), add=T, col="red")
qqnorm(CX3CL1_pre_lap, main='CX3CL1_pre_Lap')
qqline(CX3CL1_pre_lap, col='red')
shapiro.test(CX3CL1_pre_lap)



hist(log(TNFA_pre_Tras) , prob=T)
curve(dnorm(x,mean=mean(TNFA_pre_Tras), sd=sd(TNFA_pre_Tras)), add=T, col="red")
qqnorm(TNFA_pre_Tras, main='TNFA_pre_Tras')
qqline(TNFA_pre_Tras, col='red')
shapiro.test(TNFA_pre_Tras)

wilcox.test(TNFA_pre_lap,TNFA_pre_Tras)


hist(CCL20_pre_lap , prob=T)
curve(dnorm(x,mean=mean(CCL20_pre_lap), sd=sd(CCL20_pre_lap)), add=T, col="red")
qqnorm(CCL20_pre_lap, main='CCL20_pre_Lap')
qqline(CCL20_pre_lap, col='red')
shapiro.test(CCL20_pre_lap)

wilcox.test(CCL20_pre_lap,CCL20_pre_Tras,exact = FALSE)




sd(IL1B_pre_lap)
hist(IL1B_pre_Tras , prob=T)
curve(dnorm(x,mean=mean(IL1B_pre_Tras), sd=sd(IL1B_pre_Tras)), add=T, col="red")
hist(IL1B_pre , prob=T)
curve(dnorm(x,mean=mean(IL1B_pre), sd=sd(IL1B_pre)), add=T, col="red")
hist(IL1B_pre , prob=T)
curve(dnorm(x,mean=mean(IL1B_pre), sd=sd(IL1B_pre)), add=T, col="red")

###Q2
count_respon<-assignment_data %>%
  count(Treatment_Response,ER_status) 
count_respon

fisher.test(count_respon)
ERvsRespon<-matrix(c(39,19,11,19),nr=2,dimnames=list('Response'=c('Nonresponders','Responders'),'ER status'=c('Positive','Negative')))
ERvsRespon
fisher.test(ERvsRespon)                        
chisq.test(ERvsRespon)

library(datasets)
datasets::
##Q3
trastuzumab<-(Treatment_Arm=='Trastuzumab')
Kru_tras<-c(IL1B_pre_Tras,CX3CL1_pre_Tras,TNFA_pre_Tras,CCL20_pre_Tras)
Kru_tras%>%
kruskal.test(Treatment_Arm~)
count_tes<-assignment_data %>%
    count(Treatment_Arm) 
 count_tes

data<-transmute(Treatment_Arm=='Trastuzumab')

tras_data=read.csv("tras.csv")
attach(tras_data)
colnames(tras_data)
dim(tras_data)
tras_data<-tras_data%>%
  select(c(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre))
  

  stack_tras<-cbind(tras_data[1:2], stack(tras_data[3:4]))
stack_tras%>%

dunnTest(values~Ind,method = 'bh')















as.data.frame(tras_data,c(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre))
Comp_table<-table(tras_data$IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre,Treatment_Arm)
 Comp_table    
                  
                  
                  
                               
                  ?table
kruskal.test(IL1B_pre~CX3CL1_pre)
kruskal.test(Treatment_Arm~$IL1B_pre)

Tras_comp<-tras_data%>%
  select(c(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre,Treatment_Arm))


kruskal.test(IL1B_pre~CX3CL1_pre~CCL20_pre~TNFA_pre,data=Tras_comp)

kruskal.test(Tras_comp)
dunnTest(Tras_comp,data=Tras_comp,method='bh')

dunnTest(IL1B_pre,CX3CL1_pre,CCL20_pre,TNFA_pre~as.factor(Treatment_Arm),method = 'bh')


boxplot(Tras_comp,main='Boxplot of expression levels'),

library(FSA)


Markers<-stack(tras_comp,select(c(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre))
markers
               
?stack

dunnTest(Tras_comp$IL1B_pre,Tras_comp$CX3CL1_pre,Tras_comp$TNFA_pre,Tras_comp$CCL20_pre,method='bh')

library(dunn.test)

?dunn.test
dunn.test(g=Tras_comp)

k<-kruskal.test(Tras_comp)



dunnTest(IL1B_pre~Treatment_Arm,method = 'bh')








x[i<-colnames()
y<-cbind(x,as.matrix(p.adjust(x[,2],method="BH")))
colnames(y)<-c("Marker","p-value","adjusted p-value")
y

m<-dunnTest(IL1B_pre~Treatment_Arm,method='bh',data=assignment_data)

  
  dunnTest(g=IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre,method='bh')
dunn.test(,method='bh')
dunn.test(Tras_comp,method = 'bh',list=TRUE)
dunnTest(Tras_comp,data(Tras_comp),method = 'bh',list=TRUE)

?dunnTest

dunnTest(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre,method='bh')



stack(tras_comp,select(c(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre))

z<-data.frame(stack(tras_comp,select(c(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre))


data_mod <- stack(Tras_comp[1:2:3:4],colnames=c('value','marker'))
print(data_mod)
dunnTest('column 1'~'column2,method = bh')



Traz_data<-assignment_data%>%
select(c(Treatment_Arm_traz,IL1B_pre_Tras,CX3CL1_pre_Tras,TNFA_pre_Tras,CCL20_pre_Tras))


Traz_data<-Traz_data%>%
  filter(Treatment_Arm=='Trastuzumab')


Traz_data<-Traz_data%>%
  stack(2:3:4:5)


colnames(Traz_data)

colnames(Traz_data)[2] = "Marker"


dunnTest(Traz_data$Expression~Traz_data$Marker,method='bh')

kruskal.test(Traz_data$Expression~Traz_data$Marker)
