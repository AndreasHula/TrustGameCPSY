library(nnet)
library(MASS)
library(party)
library(class)
library(ggplot2)
library(cdata)
library(dplyr)
library(plot.matrix)
library(pracma)
library(tiff)
library(png)
library(raster)
library('magick')
library(gridGraphics)
library(cowplot)

#Investments, Demographics and Model Parameters
#Investment & Repayment Data
BaseInvestorI = readRDS("InvestmentData.rds")
SumInvestorI = rowSums(BaseInvestorI)

BaseTrusteeI = readRDS("RepaymentData.rds")
SumTrusteeI = rowSums(BaseTrusteeI)

#Earnings & Zero-Investments
InvestorEarningsI = 200-SumInvestorI + SumTrusteeI
Null_invest = as.numeric(BaseInvestorI==0)
Null_invest = matrix(Null_invest, length(BaseInvestorI[,1]), 10)
Null_invest = rowSums(Null_invest)

#Demographic Data
SexI = readRDS("SexData.rds")
AgeI = readRDS("AgeData.rds")
IQI =readRDS("IQData.rds")
SESI = readRDS("SESData.rds")

# Estimated Parameters
NSPNInvestorAversionI =readRDS("InvestorAversioNData.rds")
NSPNInvestorIrritabilityI = readRDS("InvestorIrrData.rds")
NSPNInvestorIrrBeliefI = readRDS("InvestorIrrBeliefData.rds")
NSPNInvestorPlanI = readRDS("InvestorPlanData.rds")
NSPNInvestorTempI = readRDS("InvestorTempData.rds")
NSPNInvestorToMI = readRDS("InvestorToMData.rds")
NSPNInvestorNLLI = readRDS("InvestorNLLData.rds")
NSPNInvestorPositiveInequalityAversionI = readRDS("InvestorGuiltData.rds")
BacktestInvestorAversionI = readRDS("InvestorAversionBacktestData.rds")

### Age Average - page 3
Average_Age=mean(AgeI)
SD_Age = sd(AgeI)



###################################### Figure 2 Block - page 10 ##################



RiskAversionConfusion = vector( "double" , 8*8)
k=0
Original_Risk_Aversion = {}
Recovered_Risk_Aversion = {}
for(i in 0:7){
  for(j in 0:7){
    k=k+1
    RiskAversionConfusion[k]=length(which(BacktestInvestorAversionI[which(NSPNInvestorAversionI==i)]==j) )/length(which(NSPNInvestorAversionI==i))
    Original_Risk_Aversion = c(Original_Risk_Aversion, 0.4+ 0.2*i)
    Recovered_Risk_Aversion = c(Recovered_Risk_Aversion, 0.4+ 0.2*j)
  }
}
C_Matrix = as.data.frame(RiskAversionConfusion)
confusion_matrix <- as.data.frame(table(BacktestInvestorAversionI, NSPNInvestorAversionI))
names(C_Matrix) = "Density"

pr=ggplot(data = C_Matrix,
          mapping = aes(x = Recovered_Risk_Aversion,
                        y = Original_Risk_Aversion))+
  geom_raster(aes(fill=Density)) +
  scale_x_continuous("Recovered Risk Aversion", 
                     breaks=c(0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8),
                     labels=c("0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8"))+
  scale_y_continuous("Original Risk Aversion",
                     breaks=c(0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8),
                     labels=c("0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8"))+
  labs( title="A) Risk Aversion Confusion Matrix ") +
   theme(axis.text.x=element_text(size=11, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=11),
                     plot.title=element_text(size=17, hjust = 0.5))+
          theme(panel.border = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))



Risk_Split = matrix(0.0, 2, 8)
for(i in 1:8){
  Risk_Split[1,i]=length(which(NSPNInvestorAversionI==(i-1) & SexI==0))/length(which(SexI==0))
  Risk_Split[2,i]=length(which(NSPNInvestorAversionI==(i-1) & SexI==1))/length(which(SexI==1))
}


p1<- ~{
  barplot(Risk_Split, 
        main = "B) Distribution of Risk Aversion by Sex" ,
        beside = TRUE, col = c("green", "orange"), 
        xlab = "Risk Aversion", 
        ylab = "Empirical Frequency", 
        axes = FALSE , ylim = c(0,0.5) )
axis(1, at = c(2,5,8,11,14,17,20,23), 0.4+ 0.2*(0:7) )
axis(2, at = c(0,0.25,0.5) )
legend("topright", col = c("green", "orange"), legend = c("Male", "Female"), bty = 'n', lty = 1)
}

Earning_Quintiles = matrix(0.0, 2, 5)
for(i in 1:5){
  Earning_Quintiles[1,i]=length(which(InvestorEarningsI < quantile(InvestorEarningsI, 0.2*i ) & SexI ==0) )
  Earning_Quintiles[2,i]=length(which(InvestorEarningsI < quantile(InvestorEarningsI, 0.2*i ) & SexI ==1) )
}
for(i in 5:2){
  Earning_Quintiles[1,i]= Earning_Quintiles[1,i]- Earning_Quintiles[1,(i-1) ]
  Earning_Quintiles[2,i]= Earning_Quintiles[2,i]- Earning_Quintiles[2,(i-1) ]
}
Earning_Quintiles[1,] = Earning_Quintiles[1,]/length(which(SexI == 0) )
Earning_Quintiles[2,] = Earning_Quintiles[2,]/length(which(SexI == 1) )


p2<- ~{ barplot(Earning_Quintiles, 
        main = "C) Distribution of Earnings by Sex" ,
        beside = TRUE, col = c("green", "orange"), 
        xlab = "Earning Quintile", 
        ylab = "Empirical Frequency", 
        axes = FALSE , ylim = c(0,0.5) )
axis(1, at = c(2,5,8,11,14), c("(0,20)", "(20,40)", 
                               "(40,60)", "(60,80)", "(80,100)" ) )
axis(2, at = c(0,0.25,0.5) )
legend("topright", col = c("green", "orange"), legend = c("Male", "Female"), bty = 'n', lty = 1)
mtext(side = 3, at = 6, line=-2 , cex = 1.0, text="Average Male Earnings: 228 (30.4)  \n Average Female Earnings: 216 (25.25) ")
}

Risk_Earnings = matrix(0.0, 1, 8)
Risk_SD = matrix(0.0, 1, 8)
for(i in 1:8){
  Risk_Earnings[1,i]=mean(InvestorEarningsI[which(NSPNInvestorAversionI==(i-1) )])
  Risk_SD[1,i] = sd(InvestorEarningsI[which(NSPNInvestorAversionI==(i-1) )])
}


p3<- ~{ barplot(Risk_Earnings, 
main = "D) Mean Earnings by Risk Aversion" ,
beside = TRUE,  col= colorRampPalette(c("black","blue"))(8), 
ylab = "Investor Earnings", 
xlab = "Risk Aversion", 
axes = FALSE , ylim = c(0,320) )
axis(1, at = c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5), 0.4+ 0.2*(0:7) )
axis(2, at = c(0,100,200,300) )
errorbar(c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5), 
         as.vector(Risk_Earnings),c(0,0,0,0,0,0,0,0), 
        as.vector(Risk_SD), bar.col = "black", add= TRUE)
}
#tiff(file="Fig2.tiff", width = 14, height=8 , units="in" , res=300, compression='lzw')
#par(mfrow=c(2,2))
#plot_grid(pr, p1, p2, p3)
#dev.off() # run code separately to see graph

###################################### End Figure 2 Block ##################


#Eq 7 p11
Investment_mdl = lm(SumInvestorI ~  NSPNInvestorToMI+NSPNInvestorPlanI+NSPNInvestorPositiveInequalityAversionI+
                   NSPNInvestorAversionI+NSPNInvestorTempI+NSPNInvestorIrritabilityI+NSPNInvestorIrrBeliefI  )
Investment_step=step(Investment_mdl, trace=0)
Investment_step_mdl=Investment_step$call
#Note on Risk Aversion p11
Investment_Risk_mdl = lm(SumInvestorI ~ NSPNInvestorAversionI )
#Z-Scoring - page 11

AgeZ = (AgeI-mean(AgeI))/sd(AgeI)
IQZ = (IQI-mean(IQI))/sd(IQI)
SESZ = (SESI-mean(SESI))/sd(SESI)

##########################         Table 2 & 3 - page 12 ##############################
Dem_Components = data.frame( AgeZ, IQZ, SESZ, SexI)
Dem_Correls=cor(Dem_Components, method = "kendall")
Dem_Correls_P = matrix(0.0,4,4)
for(i in 1:4){
  for(j in 1:4){
    K=cor.test(Dem_Components[,i], Dem_Components[,j], 
               method = "kendall")
    Dem_Correls_P[i,j] = K$p.value 
  }
}
########################## End Table 2 & 3 ##############################





######################## Aversion Block ##################

#Eq10 p12
Aversion_mdl = lm(NSPNInvestorAversionI ~  AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
Aversion_step=step(Aversion_mdl, trace=0)
Aversion_step_mdl=Aversion_step$call

######### Figure 3 panels ###########
IQ_Earnings = matrix(0.0, 1, 5)
IQ_Breaks = c(0, 90, 100, 110, 120, 140)
IQ_SD = matrix(0.0, 1, 5)
for(i in 1:5){
  IQ_Earnings[1,i]=mean(SumInvestorI[which( IQ_Breaks[i]  < IQI & 
                                              IQI < IQ_Breaks[i+1] ) ])
  IQ_SD[1,i] = sd(SumInvestorI[which( IQ_Breaks[i]  < IQI & 
                                        IQI < IQ_Breaks[i+1] ) ])
}

barplot(IQ_Earnings, 
        main = "A) Mean Investments by IQ Brackets" ,
        beside = TRUE,  col= colorRampPalette(c("black","blue"))(5), 
        ylab = "Investment Amount", 
        xlab = "IQ Bracket", 
        axes = FALSE , ylim = c(0,200) )
axis(1, at = c(1.5,3.5,5.5,7.5,9.5), c("<90",
                                       "(90,100)", "(100,110)", 
                                       "(110,120)", ">120" )  )
axis(2, at = c(0,50,100,150) )
errorbar(c(1.5,3.5,5.5,7.5,9.5), 
         as.vector(IQ_Earnings),c(0,0,0,0,0), 
         as.vector(IQ_SD), bar.col = "black", add= TRUE)
dev.off()  #run code separately to see graph

Age_Earnings = matrix(0.0, 1, 6)
SD_Age = matrix(0.0, 1, 6)
Age_Breaks = c(0, 15, 17, 19, 21, 23,25)
for(i in 1:6){
  Age_Earnings[1,i]=mean(SumInvestorI[which( Age_Breaks[i]  < AgeI & 
                                               AgeI < Age_Breaks[i+1] ) ])
  SD_Age[1,i] = sd(SumInvestorI[which( Age_Breaks[i]  < AgeI & 
                                         AgeI < Age_Breaks[i+1] ) ])
}

barplot(Age_Earnings, 
        main = "B) Mean Investments by Age Brackets" ,
        beside = TRUE,  col= colorRampPalette(c("black","blue"))(6), 
        ylab = "Investment Amount", 
        xlab = "Age Bracket", 
        axes = FALSE , ylim = c(0,200) )
axis(1, at = c(1.5,3.5,5.5,7.5,9.5, 11.5), c("<15",
                                             "(15,17)", "(17,19)", 
                                             "(19,21)", "(21,23)", ">23" )  )
axis(2, at = c(0,50,100,150) )
errorbar(c(1.5,3.5,5.5,7.5,9.5, 11.5), 
         as.vector(Age_Earnings),c(0,0,0,0,0,0), 
         as.vector(SD_Age), bar.col = "black", add= TRUE)
dev.off()  #run code separately to see graph


############# End Figure 3 Panels ###################

#################################### End Aversion Block #########################

################################### Inequality Aversion Block ###########################

#Eq 11 p13
Ineq_mdl=polr(factor(NSPNInvestorPositiveInequalityAversionI) ~ AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
Ineq_step_mdl = step(Ineq_mdl, trace=0)
(Ineq_table <- coef(summary(Ineq_step_mdl)))
p_Ineq <- pnorm(abs(Ineq_table[, "t value"]), lower.tail = FALSE) * 2

Null_Test=t.test(Null_invest[which(SexI==0)], Null_invest[which(SexI==1)], alternative = "two.sided")

################################### End Inequality Aversion Block ###########################


########################## Null Results, No Equation ###############
Irr_mdl = polr(as.factor(NSPNInvestorIrritabilityI) ~  AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
Irr_step_mdl=step(Irr_mdl, trace=0)
(Irr_table <- coef(summary(Irr_step_mdl)))
p_Irr <- pnorm(abs(Irr_table[, "t value"]), lower.tail = FALSE) * 2


ToM_mdl=polr(factor(NSPNInvestorToMI) ~ AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
ToM_step_mdl = step(ToM_mdl, trace=0)
(ToM_table <- coef(summary(ToM_step_mdl)))
p_ToM <- pnorm(abs(ToM_table[, "t value"]), lower.tail = FALSE) * 2


Plan_mdl=polr(factor(NSPNInvestorPlanI) ~ AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
Plan_step_mdl = step(Plan_mdl, trace=0)
(Plan_table <- coef(summary(Plan_step_mdl)))
p_Plan <- pnorm(abs(Plan_table[, "t value"]), lower.tail = FALSE) * 2



IrrBelief_mdl=polr(factor(NSPNInvestorIrrBeliefI) ~ AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
IrrBelief_step_mdl = step(IrrBelief_mdl, trace=0)
(IrrBelief_table <- coef(summary(IrrBelief_step_mdl)))
p_IrrBelief <- pnorm(abs(IrrBelief_table[, "t value"]), lower.tail = FALSE) * 2

########## End Null Results


#Eq12, p14
Temp_mdl = lm(NSPNInvestorTempI ~  AgeZ+IQZ+SESZ+SexI+I(AgeZ^2)+I(IQZ^2)+I(SESZ^2))
Temp_step=step(Temp_mdl, trace=0)
Temp_step_mdl=Temp_step$call

########################## Table 4 & 5, p14 ###########################


Parameter_Components = data.frame( NSPNInvestorToMI, 
                                   NSPNInvestorPlanI, NSPNInvestorAversionI, 
                                   NSPNInvestorTempI, NSPNInvestorPositiveInequalityAversionI, NSPNInvestorIrritabilityI, 
                                   NSPNInvestorIrrBeliefI)
Correls=cor(Parameter_Components, method = "kendall")
Correls_P = matrix(0.0,7,7)
for(i in 1:7){
  for(j in 1:7){
    K=cor.test(Parameter_Components[,i], Parameter_Components[,j], 
               method = "kendall")
    Correls_P[i,j] = K$p.value 
  }
}
######################## End Table 4 & 5 #####################