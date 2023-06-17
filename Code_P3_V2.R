#**Jorge Eduardo Durán Vásquez
#**Case Studies SS 2022 
#*** Project 3: Nowcasting US GDP Growth Unrestricted MIDAS

###########***Upload and processing data***####################################

# Upload the data set
MData <- read.csv(file = "2022-02.csv", header = TRUE, sep = ',')
QData <- read.csv(file = "Q2022-02.csv", header = TRUE, sep = ',')

# Pre-processing the data
NewMData <- data.frame(MData$sasdate,MData$CUMFNS,MData$UNRATE,MData$CPIAUCSL,MData$FEDFUNDS,MData$M2REAL,MData$S.P.500)
NewQData <- data.frame(QData$sasdate,QData$GDPC1)

#Save the data without the first two rows
MData_V2 <- NewMData[2:758,]
QData_V2 <- NewQData[3:254,]

# Transforming Growth Gross Domestic Product (GDP) 
GDPgrowth = (diff(QData_V2$QData.GDPC1,1)/QData_V2$QData.GDPC1[1:251])*100

#Transforming Consumer Price Index (CPIAUCSL)
CPIAUCSLgrowth = (diff(MData_V2$MData.CPIAUCSL,1)/MData_V2$MData.CPIAUCSL[1:756])*100

# Transforming M1 money stock (M2REAL)
M2REALgrowth = (diff(MData_V2$MData.M2REAL,1)/MData_V2$MData.M2REAL[1:756])*100

#Transforming to Growth S&P 500 (S.P.500)
S.P.500growth = (diff(MData_V2$MData.S.P.500,1)/MData_V2$MData.S.P.500[1:756])*100

# We start from the fourth and third observation because, 
# the first observations was used in the transformation and
# then the first quarterly data is not complete then we start
# from the second quarterly data point that is 1/06/1959, the first
# month of this quarterly is 1/04/1959

AnalysisMdata <- data.frame(MData_V2$MData.CUMFNS[4:757],
                           MData_V2$MData.UNRATE[4:757],
                           CPIAUCSLgrowth[3:756],
                           MData_V2$MData.FEDFUNDS[4:757],
                           M2REALgrowth[3:756],
                           S.P.500growth[3:756]) 

colnames(AnalysisMdata) <- c('CUMFNS',
                            'UNRATE',
                            'CPIAUCSL',
                            'FEDFUNDS',
                            'M2REAL_growth',
                            'S.P.500_growth')

AnalysisQdata <- data.frame(GDPgrowth)

colnames(AnalysisQdata) <- c('GDPgrowth')

###########***Part a Ploting Time Series***############################################


# Change the format of the data to get time series data

tis_GDPC1 <- ts(AnalysisQdata$GDPgrowth, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_CUMFNS<- ts(AnalysisMdata$CUMFNS, start= c(1959,2), frequency = 12, end=c(2021,12))
tis_UNRATE <- ts(AnalysisMdata$UNRATE, start= c(1959,2), frequency = 12, end=c(2021,12))
tis_CPIAUCSL <- ts(AnalysisMdata$CPIAUCSL, start= c(1959,2), frequency = 12, end=c(2021,12))
tis_FEDFUNDS <- ts(AnalysisMdata$FEDFUNDS, start= c(1959,2), frequency = 12, end=c(2021,12))
tis_M2REAL <- ts(AnalysisMdata$M2REAL_growth, start= c(1959,2), frequency = 12, end=c(2021,12))
tis_S.P.500 <- ts(AnalysisMdata$S.P.500_growth, start= c(1959,2), frequency = 12, end=c(2021,12))


# With transformation 

pdf(file = "fig/Time_Series.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6) # The height of the plot in inches

par(mfrow=c(4,2),mar=c(3, 5, 1.1, 1),mgp=c(2,1,0))
ts.plot(tis_GDPC1, ylab="GDP Growth [%]", xlab="Years")
ts.plot(tis_CUMFNS, ylab="CUMFNS [%]", xlab="Years")
ts.plot(tis_UNRATE, ylab="UNRATE [%]", xlab="Years")
ts.plot(tis_CPIAUCSL, ylab="Inflation [%]", xlab="Years")
ts.plot(tis_FEDFUNDS, ylab="FEDFUNDS [%]", xlab="Years")
ts.plot(tis_M2REAL, ylab="M2REAL Growth [%]", xlab="Years")
ts.plot(tis_S.P.500, ylab="S&P 500 Growth [%]", xlab="Years")

## Run dev.off() to create the PDF file
dev.off()


###########***Part a BIC Model 1***#########################################
# The Nowcast error for Unrestricted MIDAS 
#BIC For k=1

Data_K1 = data.frame(matrix(ncol = 12, nrow = 0))

# we start from 2 because it is the value of the quarterly s that
# we can forecast

for(i in 2:14){ 
  Data_K1<- rbind(Data_K1,
                c(AnalysisMdata[3*i-1,],
                  AnalysisMdata[3*i-2,]))
}

Forecasting_MIDASBIC_K1= c()

for (i in 15:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K1 = rep(1,(i-1))
  K1_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K1 <- rbind(Data_K1,
                   c(AnalysisMdata[3*i-1,],
                     AnalysisMdata[3*i-2,]))
  
  z_BIC_K1 = rbind(ones_BIC_K1,
                   K1_L2_GDP,
                   t(Data_K1))
  
  Y_BIC_K1 = rbind(GDPgrowth[2:(i)])
  
  C_BIC_K1 = solve(z_BIC_K1%*%t(z_BIC_K1))
  B_BIC_K1 = Y_BIC_K1%*%t(z_BIC_K1)
  A_BIC_K1 = B_BIC_K1%*%C_BIC_K1
  
  pred_BIC_K1= A_BIC_K1[1,1] +
    (A_BIC_K1[1,2]*GDPgrowth[i])+
    (A_BIC_K1[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K1[1,4]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K1[1,5]*AnalysisMdata$CPIAUCSL[(3*(i+1))-1])+
    (A_BIC_K1[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))-1])+
    (A_BIC_K1[1,7]*AnalysisMdata$M2REAL_growth[(3*(i+1))-1])+
    (A_BIC_K1[1,8]*AnalysisMdata$S.P.500_growth[(3*(i+1))-1])+
    (A_BIC_K1[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K1[1,10]*AnalysisMdata$UNRATE[(3*(i+1))-2])+
    (A_BIC_K1[1,11]*AnalysisMdata$CPIAUCSL[(3*(i+1))-2])+
    (A_BIC_K1[1,12]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K1[1,13]*AnalysisMdata$M2REAL_growth[(3*(i+1))-2])+
    (A_BIC_K1[1,14]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])
    
              
  Forecasting_MIDASBIC_K1 <-  append(Forecasting_MIDASBIC_K1,pred_BIC_K1)
  
}

colnames(Data_K1) <- c('K1_L1_CUMFNS',
                       'K1_L1_UNRATE',
                       'K1_L1_CPIAUCSL',
                       'K1_L1_FEDFUNDS',
                       'K1_L1_M2REAL_growth',
                       'K1_L1_S.P.500_growth',
                       'K1_L2_CUMFNS',
                       'K1_L2_UNRATE',
                       'K1_L2_CPIAUCSL',
                       'K1_L2_FEDFUNDS',
                       'K1_L2_M2REAL_growth',
                       'K1_L2_S.P.500_growth')

Resd_1_BIC_K1 = Y_BIC_K1- (A_BIC_K1%*%z_BIC_K1)
Cov_resM1_T_BIC_K1 = (Resd_1_BIC_K1%*%t(Resd_1_BIC_K1))/250
BIC_K1= log(det(Cov_resM1_T_BIC_K1)) + ((1/2)*(log(250))*(14))

BIC_K1

# BIC_K1 = 38.45889

# # Writing ranger_importance_permutation data
# write.table(Resd_1_BIC_K1, file = "Resd_1_BIC_K1", sep = "\t",
#             row.names = TRUE, col.names = NA)




#BIC For k=2

Data_K2 = data.frame(matrix(ncol = 18, nrow = 0))

for(i in 2:20){
  Data_K2 <- rbind(Data_K2,
                  c(AnalysisMdata[(3*i)-1,],
                    AnalysisMdata[(3*i)-2,],
                    AnalysisMdata[(3*i)-3,]))
  colnames(Data_K2) <- c()
  }

Forecasting_MIDASBIC_K2= c()

for (i in 21:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K2 = rep(1,(i-1))
  K2_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K2 <- rbind(Data_K2,
                   c(AnalysisMdata[3*i-1,],
                     AnalysisMdata[3*i-2,],
                     AnalysisMdata[3*i-3,]))
  
  z_BIC_K2 = rbind(ones_BIC_K2,
                   K2_L2_GDP,
                   t(Data_K2))
  
  Y_BIC_K2 = rbind(GDPgrowth[2:(i)])
  
  C_BIC_K2 = solve(z_BIC_K2%*%t(z_BIC_K2))
  B_BIC_K2 = Y_BIC_K2%*%t(z_BIC_K2)
  A_BIC_K2 = B_BIC_K2%*%C_BIC_K2
  
  pred_BIC_K2= A_BIC_K2[1,1] +
    (A_BIC_K2[1,2]*GDPgrowth[i])+
    (A_BIC_K2[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K2[1,4]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K2[1,5]*AnalysisMdata$CPIAUCSL[(3*(i+1))-1])+
    (A_BIC_K2[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))-1])+
    (A_BIC_K2[1,7]*AnalysisMdata$M2REAL_growth[(3*(i+1))-1])+
    (A_BIC_K2[1,8]*AnalysisMdata$S.P.500_growth[(3*(i+1))-1])+
    (A_BIC_K2[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K2[1,10]*AnalysisMdata$UNRATE[(3*(i+1))-2])+
    (A_BIC_K2[1,11]*AnalysisMdata$CPIAUCSL[(3*(i+1))-2])+
    (A_BIC_K2[1,12]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K2[1,13]*AnalysisMdata$M2REAL_growth[(3*(i+1))-2])+
    (A_BIC_K2[1,14]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])+
    (A_BIC_K2[1,15]*AnalysisMdata$CUMFNS[(3*(i+1))-3])+
    (A_BIC_K2[1,16]*AnalysisMdata$UNRATE[(3*(i+1))-3])+
    (A_BIC_K2[1,17]*AnalysisMdata$CPIAUCSL[(3*(i+1))-3])+
    (A_BIC_K2[1,18]*AnalysisMdata$FEDFUNDS[(3*(i+1))-3])+
    (A_BIC_K2[1,19]*AnalysisMdata$M2REAL_growth[(3*(i+1))-3])+
    (A_BIC_K2[1,20]*AnalysisMdata$S.P.500_growth[(3*(i+1))-3])
  
  
  Forecasting_MIDASBIC_K2 <-  append(Forecasting_MIDASBIC_K2,pred_BIC_K2)
  
}

colnames(Data_K2) <- c('K2_L1_CUMFNS',
                       'K2_L1_UNRATE',
                       'K2_L1_CPIAUCSL',
                       'K2_L1_FEDFUNDS',
                       'K2_L1_M2REAL_growth',
                       'K2_L1_S.P.500_growth',
                       'K2_L2_CUMFNS',
                       'K2_L2_UNRATE',
                       'K2_L2_CPIAUCSL',
                       'K2_L2_FEDFUNDS',
                       'K2_L2_M2REAL_growth',
                       'K2_L2_S.P.500_growth')

Resd_1_BIC_K2 = Y_BIC_K2- (A_BIC_K2%*%z_BIC_K2)
Cov_resM1_T_BIC_K2 = (Resd_1_BIC_K2%*%t(Resd_1_BIC_K2))/250
BIC_K2= log(det(Cov_resM1_T_BIC_K2)) + ((1/2)*(log(250))*(20))

BIC_K2

# BIC_K2 = 54.00145

#BIC For k=3

Data_K3 = data.frame(matrix(ncol = 24, nrow = 0))

# we start from 2 because it is the first value of the quarterly "s" that
# we can forecast

for(i in 2:26){
  Data_K3 <- rbind(Data_K3,
                   c(AnalysisMdata[(3*i)-1,],
                     AnalysisMdata[(3*i)-2,],
                     AnalysisMdata[(3*i)-3,],
                     AnalysisMdata[(3*i)-4,]))
  colnames(Data_K3) <- c()
}

Forecasting_MIDASBIC_K3= c()

for (i in 27:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K3 = rep(1,(i-1))
  K3_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K3 <- rbind(Data_K3,
                   c(AnalysisMdata[3*i-1,],
                     AnalysisMdata[3*i-2,],
                     AnalysisMdata[3*i-3,],
                     AnalysisMdata[3*i-4,]))
  
  z_BIC_K3 = rbind(ones_BIC_K3,
                   K3_L2_GDP,
                   t(Data_K3))
  
  Y_BIC_K3 = rbind(AnalysisQdata$GDPgrowth[2:(i)])
  
  C_BIC_K3 = solve(z_BIC_K3%*%t(z_BIC_K3))
  B_BIC_K3 = Y_BIC_K3%*%t(z_BIC_K3)
  A_BIC_K3 = B_BIC_K3%*%C_BIC_K3

  # As a reference we first predict the observation 28 
  
  pred_BIC_K3= A_BIC_K3[1,1] +
    (A_BIC_K3[1,2]*AnalysisQdata$GDPgrowth[i])+
    (A_BIC_K3[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K3[1,4]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K3[1,5]*AnalysisMdata$CPIAUCSL[(3*(i+1))-1])+
    (A_BIC_K3[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))-1])+
    (A_BIC_K3[1,7]*AnalysisMdata$M2REAL_growth[(3*(i+1))-1])+
    (A_BIC_K3[1,8]*AnalysisMdata$S.P.500_growth[(3*(i+1))-1])+
    (A_BIC_K3[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K3[1,10]*AnalysisMdata$UNRATE[(3*(i+1))-2])+
    (A_BIC_K3[1,11]*AnalysisMdata$CPIAUCSL[(3*(i+1))-2])+
    (A_BIC_K3[1,12]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K3[1,13]*AnalysisMdata$M2REAL_growth[(3*(i+1))-2])+
    (A_BIC_K3[1,14]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])+
    (A_BIC_K3[1,15]*AnalysisMdata$CUMFNS[(3*(i+1))-3])+
    (A_BIC_K3[1,16]*AnalysisMdata$UNRATE[(3*(i+1))-3])+
    (A_BIC_K3[1,17]*AnalysisMdata$CPIAUCSL[(3*(i+1))-3])+
    (A_BIC_K3[1,18]*AnalysisMdata$FEDFUNDS[(3*(i+1))-3])+
    (A_BIC_K3[1,19]*AnalysisMdata$M2REAL_growth[(3*(i+1))-3])+
    (A_BIC_K3[1,20]*AnalysisMdata$S.P.500_growth[(3*(i+1))-3])+
    (A_BIC_K3[1,21]*AnalysisMdata$CUMFNS[(3*(i+1))-4])+
    (A_BIC_K3[1,22]*AnalysisMdata$UNRATE[(3*(i+1))-4])+
    (A_BIC_K3[1,23]*AnalysisMdata$CPIAUCSL[(3*(i+1))-4])+
    (A_BIC_K3[1,24]*AnalysisMdata$FEDFUNDS[(3*(i+1))-4])+
    (A_BIC_K3[1,25]*AnalysisMdata$M2REAL_growth[(3*(i+1))-4])+
    (A_BIC_K3[1,26]*AnalysisMdata$S.P.500_growth[(3*(i+1))-4])
  
  
  Forecasting_MIDASBIC_K3 <-  append(Forecasting_MIDASBIC_K3,pred_BIC_K3)
  
}

colnames(Data_K3) <- c('K3_L1_CUMFNS',
                       'K3_L1_UNRATE',
                       'K3_L1_CPIAUCSL',
                       'K3_L1_FEDFUNDS',
                       'K3_L1_M2REAL_growth',
                       'K3_L1_S.P.500_growth',
                       'K3_L2_CUMFNS',
                       'K3_L2_UNRATE',
                       'K3_L2_CPIAUCSL',
                       'K3_L2_FEDFUNDS',
                       'K3_L2_M2REAL_growth',
                       'K3_L2_S.P.500_growth')

Resd_1_BIC_K3 = Y_BIC_K3- (A_BIC_K3%*%z_BIC_K3)
Cov_resM1_T_BIC_K3 = (Resd_1_BIC_K3%*%t(Resd_1_BIC_K3))/250
BIC_K3= log(det(Cov_resM1_T_BIC_K3)) + ((1/2)*(log(250))*(26))

BIC_K3

# BIC_K3 = 70.35812

# The Nowcast error for Unrestricted MIDAS 


GDPgrowth_graph_MIDASK3 <- ts(AnalysisQdata$GDPgrowth, start= c(1959,2), frequency = 4, end=c(2021,4))
Forecastingtis_MIDASK3 <- ts(Forecasting_MIDASBIC_K3, start= c(1966,1), frequency = 4, end=c(2021,4))

GDPgrowth_MIDASgraph_K3 <- AnalysisQdata$GDPgrowth[-c(1:27)]

pdf(file = "fig/MIDAS3_GDP.pdf",   # The directory you want to save the file in
    width = 19, # The width of the plot in inches
    height = 10) # The height of the plot in inches

par(mfrow=c(1,1),cex.lab = 1.5,cex.axis = 1.5)
ts.plot(GDPgrowth_graph_MIDASK3, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]", ylim=c(-10,10),lwd = 2)
points(Forecastingtis_MIDASK3, col= 'red', type = 'l', lty=1,lwd = 2)
legend("topleft", legend=c("GDP growth","MIDAS Nowcast GDP growth Model 1"),
       col=c("black", "red"), lty=c("solid","solid"),lwd = c(3,3), cex = 1.5)

## Run dev.off() to create the PDF file
dev.off()

forecasting_error_MIDASK3= (sqrt((1/length(GDPgrowth_MIDASgraph_K3))*sum((GDPgrowth_MIDASgraph_K3-Forecasting_MIDASBIC_K3)^2)))
forecasting_error_MIDASK3

# 2.003935

###########***Part b BIC Model 2***#########################################
# The Nowcast error for GDP growth using Unrestricted MIDAS and contemporaneos HF observations 
#BIC For k=1

Data_K1N = data.frame(matrix(ncol = 18, nrow = 0))

for(i in 2:20){
  Data_K1N<- rbind(Data_K1N,
                  c(AnalysisMdata[3*i,],
                    AnalysisMdata[3*i-1,],
                    AnalysisMdata[3*i-2,]))
  colnames(Data_K1N) <- c()
}

Forecasting_MIDASBIC_K1N= c()

for (i in 21:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K1N = rep(1,(i-1))
  K1N_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K1N <- rbind(Data_K1N,
                   c(AnalysisMdata[3*i,],
                     AnalysisMdata[3*i-1,],
                     AnalysisMdata[3*i-2,]))
  
  z_BIC_K1N = rbind(ones_BIC_K1N,
                   K1N_L2_GDP,
                   t(Data_K1N))
  
  Y_BIC_K1N = rbind(GDPgrowth[2:(i)])
  
  C_BIC_K1N = solve(z_BIC_K1N%*%t(z_BIC_K1N))
  B_BIC_K1N = Y_BIC_K1N%*%t(z_BIC_K1N)
  A_BIC_K1N = B_BIC_K1N%*%C_BIC_K1N
  
  pred_BIC_K1N= A_BIC_K1N[1,1] +
    (A_BIC_K1N[1,2]*GDPgrowth[i])+
    (A_BIC_K1N[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))])+
    (A_BIC_K1N[1,4]*AnalysisMdata$UNRATE[(3*(i+1))])+
    (A_BIC_K1N[1,5]*AnalysisMdata$CPIAUCSL[(3*(i+1))])+
    (A_BIC_K1N[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))])+
    (A_BIC_K1N[1,7]*AnalysisMdata$M2REAL_growth[(3*(i+1))])+
    (A_BIC_K1N[1,8]*AnalysisMdata$S.P.500_growth[(3*(i+1))])+
    (A_BIC_K1N[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K1N[1,10]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K1N[1,11]*AnalysisMdata$CPIAUCSL[(3*(i+1))-1])+
    (A_BIC_K1N[1,12]*AnalysisMdata$FEDFUNDS[(3*(i+1))-1])+
    (A_BIC_K1N[1,13]*AnalysisMdata$M2REAL_growth[(3*(i+1))-1])+
    (A_BIC_K1N[1,14]*AnalysisMdata$S.P.500_growth[(3*(i+1))-1])+
    (A_BIC_K1N[1,15]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K1N[1,16]*AnalysisMdata$UNRATE[(3*(i+1))-2])+
    (A_BIC_K1N[1,17]*AnalysisMdata$CPIAUCSL[(3*(i+1))-2])+
    (A_BIC_K1N[1,18]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K1N[1,19]*AnalysisMdata$M2REAL_growth[(3*(i+1))-2])+
    (A_BIC_K1N[1,20]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])
  
  
  Forecasting_MIDASBIC_K1N <-  append(Forecasting_MIDASBIC_K1N,pred_BIC_K1N)
  
}

colnames(Data_K1N) <- c('K1N_L1_CUMFNS',
                       'K1N_L1_UNRATE',
                       'K1N_L1_CPIAUCSL',
                       'K1N_L1_FEDFUNDS',
                       'K1N_L1_M2REAL_growth',
                       'K1N_L1_S.P.500_growth',
                       'K1N_L2_CUMFNS',
                       'K1N_L2_UNRATE',
                       'K1N_L2_CPIAUCSL',
                       'K1N_L2_FEDFUNDS',
                       'K1N_L2_M2REAL_growth',
                       'K1N_L2_S.P.500_growth')

Resd_1_BIC_K1N = Y_BIC_K1N- (A_BIC_K1N%*%z_BIC_K1N)
Cov_resM1_T_BIC_K1N = (Resd_1_BIC_K1N%*%t(Resd_1_BIC_K1N))/250
BIC_K1N= log(det(Cov_resM1_T_BIC_K1N)) + ((1/2)*(log(250))*(20))

BIC_K1N

# BIC_K1N = 55.00177


#BIC For k=2

Data_K2N = data.frame(matrix(ncol = 24, nrow = 0))

for(i in 2:26){
  Data_K2N <- rbind(Data_K2N,
                   c(AnalysisMdata[(3*i),],
                     AnalysisMdata[(3*i)-1,],
                     AnalysisMdata[(3*i)-2,],
                     AnalysisMdata[(3*i)-3,]))
  colnames(Data_K2N) <- c()
}

Forecasting_MIDASBIC_K2N= c()

for (i in 27:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K2N = rep(1,(i-1))
  K2N_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K2N <- rbind(Data_K2N,
                   c(AnalysisMdata[3*i,],
                     AnalysisMdata[3*i-1,],
                     AnalysisMdata[3*i-2,],
                     AnalysisMdata[3*i-3,]))
  
  z_BIC_K2N = rbind(ones_BIC_K2N,
                   K2N_L2_GDP,
                   t(Data_K2N))
  
  Y_BIC_K2N = rbind(GDPgrowth[2:(i)])
  
  C_BIC_K2N = solve(z_BIC_K2N%*%t(z_BIC_K2N))
  B_BIC_K2N = Y_BIC_K2N%*%t(z_BIC_K2N)
  A_BIC_K2N = B_BIC_K2N%*%C_BIC_K2N
  
  pred_BIC_K2N= A_BIC_K2N[1,1] +
    (A_BIC_K2N[1,2]*GDPgrowth[i])+
    (A_BIC_K2N[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))])+
    (A_BIC_K2N[1,4]*AnalysisMdata$UNRATE[(3*(i+1))])+
    (A_BIC_K2N[1,5]*AnalysisMdata$CPIAUCSL[(3*(i+1))])+
    (A_BIC_K2N[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))])+
    (A_BIC_K2N[1,7]*AnalysisMdata$M2REAL_growth[(3*(i+1))])+
    (A_BIC_K2N[1,8]*AnalysisMdata$S.P.500_growth[(3*(i+1))])+
    (A_BIC_K2N[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K2N[1,10]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K2N[1,11]*AnalysisMdata$CPIAUCSL[(3*(i+1))-1])+
    (A_BIC_K2N[1,12]*AnalysisMdata$FEDFUNDS[(3*(i+1))-1])+
    (A_BIC_K2N[1,13]*AnalysisMdata$M2REAL_growth[(3*(i+1))-1])+
    (A_BIC_K2N[1,14]*AnalysisMdata$S.P.500_growth[(3*(i+1))-1])+
    (A_BIC_K2N[1,15]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K2N[1,16]*AnalysisMdata$UNRATE[(3*(i+1))-2])+
    (A_BIC_K2N[1,17]*AnalysisMdata$CPIAUCSL[(3*(i+1))-2])+
    (A_BIC_K2N[1,18]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K2N[1,19]*AnalysisMdata$M2REAL_growth[(3*(i+1))-2])+
    (A_BIC_K2N[1,20]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])+
    (A_BIC_K2N[1,21]*AnalysisMdata$CUMFNS[(3*(i+1))-3])+
    (A_BIC_K2N[1,22]*AnalysisMdata$UNRATE[(3*(i+1))-3])+
    (A_BIC_K2N[1,23]*AnalysisMdata$CPIAUCSL[(3*(i+1))-3])+
    (A_BIC_K2N[1,24]*AnalysisMdata$FEDFUNDS[(3*(i+1))-3])+
    (A_BIC_K2N[1,25]*AnalysisMdata$M2REAL_growth[(3*(i+1))-3])+
    (A_BIC_K2N[1,26]*AnalysisMdata$S.P.500_growth[(3*(i+1))-3])
  
  
  Forecasting_MIDASBIC_K2N <-  append(Forecasting_MIDASBIC_K2N,pred_BIC_K2N)
  
}

colnames(Data_K2N) <- c('K2N_L1_CUMFNS',
                       'K2N_L1_UNRATE',
                       'K2N_L1_CPIAUCSL',
                       'K2N_L1_FEDFUNDS',
                       'K2N_L1_M2REAL_growth',
                       'K2N_L1_S.P.500_growth',
                       'K2N_L2_CUMFNS',
                       'K2N_L2_UNRATE',
                       'K2N_L2_CPIAUCSL',
                       'K2N_L2_FEDFUNDS',
                       'K2N_L2_M2REAL_growth',
                       'K2N_L2_S.P.500_growth')

Resd_1_BIC_K2N = Y_BIC_K2N- (A_BIC_K2N%*%z_BIC_K2N)
Cov_resM1_T_BIC_K2N = (Resd_1_BIC_K2N%*%t(Resd_1_BIC_K2N))/250
BIC_K2N= log(det(Cov_resM1_T_BIC_K2N)) + ((1/2)*(log(250))*(26))

BIC_K2N

# BIC_K2N = 70.48164

#BIC For k=3

Data_K3N = data.frame(matrix(ncol = 30, nrow = 0))

for(i in 2:32){
  Data_K3N <- rbind(Data_K3N,
                   c(AnalysisMdata[(3*i),],
                     AnalysisMdata[(3*i)-1,],
                     AnalysisMdata[(3*i)-2,],
                     AnalysisMdata[(3*i)-3,],
                     AnalysisMdata[(3*i)-4,]))
  colnames(Data_K3N) <- c()
}


Forecasting_MIDASBIC_K3N= c()

for (i in 33:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K3N = rep(1,(i-1))
  K3N_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K3N <- rbind(Data_K3N,
                   c(AnalysisMdata[3*i,],
                     AnalysisMdata[3*i-1,],
                     AnalysisMdata[3*i-2,],
                     AnalysisMdata[3*i-3,],
                     AnalysisMdata[3*i-4,]))
  
  z_BIC_K3N = rbind(ones_BIC_K3N,
                   K3N_L2_GDP,
                   t(Data_K3N))
  
  Y_BIC_K3N = rbind(AnalysisQdata$GDPgrowth[2:(i)])
  
  C_BIC_K3N = solve(z_BIC_K3N%*%t(z_BIC_K3N))
  B_BIC_K3N = Y_BIC_K3N%*%t(z_BIC_K3N)
  A_BIC_K3N = B_BIC_K3N%*%C_BIC_K3N
  
  pred_BIC_K3N= A_BIC_K3N[1,1] +
    (A_BIC_K3N[1,2]*AnalysisQdata$GDPgrowth[i])+
    (A_BIC_K3N[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))])+
    (A_BIC_K3N[1,4]*AnalysisMdata$UNRATE[(3*(i+1))])+
    (A_BIC_K3N[1,5]*AnalysisMdata$CPIAUCSL[(3*(i+1))])+
    (A_BIC_K3N[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))])+
    (A_BIC_K3N[1,7]*AnalysisMdata$M2REAL_growth[(3*(i+1))])+
    (A_BIC_K3N[1,8]*AnalysisMdata$S.P.500_growth[(3*(i+1))])+
    (A_BIC_K3N[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K3N[1,10]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K3N[1,11]*AnalysisMdata$CPIAUCSL[(3*(i+1))-1])+
    (A_BIC_K3N[1,12]*AnalysisMdata$FEDFUNDS[(3*(i+1))-1])+
    (A_BIC_K3N[1,13]*AnalysisMdata$M2REAL_growth[(3*(i+1))-1])+
    (A_BIC_K3N[1,14]*AnalysisMdata$S.P.500_growth[(3*(i+1))-1])+
    (A_BIC_K3N[1,15]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K3N[1,16]*AnalysisMdata$UNRATE[(3*(i+1))-2])+
    (A_BIC_K3N[1,17]*AnalysisMdata$CPIAUCSL[(3*(i+1))-2])+
    (A_BIC_K3N[1,18]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K3N[1,19]*AnalysisMdata$M2REAL_growth[(3*(i+1))-2])+
    (A_BIC_K3N[1,20]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])+
    (A_BIC_K3N[1,21]*AnalysisMdata$CUMFNS[(3*(i+1))-3])+
    (A_BIC_K3N[1,22]*AnalysisMdata$UNRATE[(3*(i+1))-3])+
    (A_BIC_K3N[1,23]*AnalysisMdata$CPIAUCSL[(3*(i+1))-3])+
    (A_BIC_K3N[1,24]*AnalysisMdata$FEDFUNDS[(3*(i+1))-3])+
    (A_BIC_K3N[1,25]*AnalysisMdata$M2REAL_growth[(3*(i+1))-3])+
    (A_BIC_K3N[1,26]*AnalysisMdata$S.P.500_growth[(3*(i+1))-3])+
    (A_BIC_K3N[1,27]*AnalysisMdata$CUMFNS[(3*(i+1))-4])+
    (A_BIC_K3N[1,28]*AnalysisMdata$UNRATE[(3*(i+1))-4])+
    (A_BIC_K3N[1,29]*AnalysisMdata$CPIAUCSL[(3*(i+1))-4])+
    (A_BIC_K3N[1,30]*AnalysisMdata$FEDFUNDS[(3*(i+1))-4])+
    (A_BIC_K3N[1,31]*AnalysisMdata$M2REAL_growth[(3*(i+1))-4])+
    (A_BIC_K3N[1,32]*AnalysisMdata$S.P.500_growth[(3*(i+1))-4])
  
  
  Forecasting_MIDASBIC_K3N <-  append(Forecasting_MIDASBIC_K3N,pred_BIC_K3N)
  
}

colnames(z_BIC_K3N) <- c('K3N_Intercept',
                        'K3N_GDP',
                        'K3N_CUMFNS',
                        'K3N_UNRATE',
                        'K3N_CPIAUCSL',
                        'K3N_FEDFUNDS',
                        'K3N_M2REAL_growth',
                        'K3N_S.P.500_growth',
                       'K3N_L1_CUMFNS',
                       'K3N_L1_UNRATE',
                       'K3N_L1_CPIAUCSL',
                       'K3N_L1_FEDFUNDS',
                       'K3N_L1_M2REAL_growth',
                       'K3N_L1_S.P.500_growth',
                       'K3N_L2_CUMFNS',
                       'K3N_L2_UNRATE',
                       'K3N_L2_CPIAUCSL',
                       'K3N_L2_FEDFUNDS',
                       'K3N_L2_M2REAL_growth',
                       'K3N_L2_S.P.500_growth',
                       'K3N_L3_CUMFNS',
                       'K3N_L3_UNRATE',
                       'K3N_L3_CPIAUCSL',
                       'K3N_L3_FEDFUNDS',
                       'K3N_L3_M2REAL_growth',
                       'K3N_L3_S.P.500_growth',
                       'K3N_L4_CUMFNS',
                       'K3N_L4_UNRATE',
                       'K3N_L4_CPIAUCSL',
                       'K3N_L4_FEDFUNDS',
                       'K3N_L4_M2REAL_growth',
                       'K3N_L4_S.P.500_growth')

# # Writing ranger_importance_permutation data
# write.table(z_BIC_K3N, file = "First_Input_Data_model_2", sep = "\t",
#             row.names = TRUE, col.names = NA)


Resd_1_BIC_K3N = Y_BIC_K3N- (A_BIC_K3N%*%z_BIC_K3N)
Cov_resM1_T_BIC_K3N = (Resd_1_BIC_K3N%*%t(Resd_1_BIC_K3N))/250
BIC_K3N= log(det(Cov_resM1_T_BIC_K3N)) + ((1/2)*(log(250))*(32))

BIC_K3N

# BIC_K3N = 86.84903

# The Nowcast error for Unrestricted MIDAS 

GDPgrowth_graph_MIDASK3N <- ts(AnalysisQdata$GDPgrowth, start= c(1959,2), frequency = 4, end=c(2021,4))
Forecastingtis_MIDASK3N <- ts(Forecasting_MIDASBIC_K3N, start= c(1967,3), frequency = 4, end=c(2021,4))

GDPgrowth_MIDASgraph_K3N <- AnalysisQdata$GDPgrowth[-c(1:33)]

pdf(file = "fig/MIDASK3N_GDP_V2.pdf",   # The directory you want to save the file in
    width = 17, # The width of the plot in inches
    height = 10) # The height of the plot in inches

par(mfrow=c(1,1))
ts.plot(GDPgrowth_graph_MIDASK3N, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]", ylim=c(-10,10),lwd = 2)
points(Forecastingtis_MIDASK3N, col= 'red', type = 'l', lty=1,lwd = 2)
legend("bottomleft", legend=c("GDP growth","MIDAS Nowcast GDP growth Model 2"),
       col=c("black", "red"), lty=c("solid","solid"),lwd = c(3,3), cex = 1.5)

## Run dev.off() to create the PDF file
dev.off()

forecasting_error_MIDASK3N= (sqrt((1/length(GDPgrowth_MIDASgraph_K3N))*sum((GDPgrowth_MIDASgraph_K3N-Forecasting_MIDASBIC_K3N)^2)))
forecasting_error_MIDASK3N

# 2.828703



###########***Part c Significance of the variables K=3 Model 1***##############

#we estimate the variance of the error 
# T-test statistic model 1 BIC K3

Resd_1_K3 = Y_BIC_K3- (A_BIC_K3%*%z_BIC_K3)

var_BIC_1= solve(z_BIC_K3%*%t(z_BIC_K3))
# We are using 250 observations from 1/06/1959 to 1/09/2021   
var_BIC_2= ((Resd_1_K3%*%t(Resd_1_K3))/(250-(25*1)-1))
var_BIC_K3 = kronecker(var_BIC_1,var_BIC_2)


t_K3 <- c()

for (i in 1:length(A_BIC_K3)){
  t_K3 = append(t_K3, (A_BIC_K3[1,i]/sqrt(var_BIC_K3[i,i])))
}

names(t_K3) <- c('K3_Intercept',
                 'K3_GDPgrowth_L1',
                 'K3_L1_CUMFNS',
                'K3_L1_UNRATE',
                'K3_L1_CPIAUCSL',
                'K3_L1_FEDFUNDS',
                'K3_L1_M2REAL_growth',
                'K3_L1_S.P.500_growth',
                'K3_L2_CUMFNS',
                'K3_L2_UNRATE',
                'K3_L2_CPIAUCSL',
                'K3_L2_FEDFUNDS',
                'K3_L2_M2REAL_growth',
                'K3_L2_S.P.500_growth',
                'K3_L3_CUMFNS',
                'K3_L3_UNRATE',
                'K3_L3_CPIAUCSL',
                'K3_L3_FEDFUNDS',
                'K3_L3_M2REAL_growth',
                'K3_L3_S.P.500_growth',
                'K3_L4_CUMFNS',
                'K3_L4_UNRATE',
                'K3_L4_CPIAUCSL',
                'K3_L4_FEDFUNDS',
                'K3_L4_M2REAL_growth',
                'K3_L4_S.P.500_growth')

not_significance_K3 <- (t_K3 < 1.96) & (t_K3>-1.96)
not_significance_K3
# For a large sample size it follows a normal distribution
# at the level of significance alpha = 0.05 we have that for a 
# 2 sided test the critical value is (+/-) 1.96
# now we check which values from t_K3 fulfill X < 1.96 
# which means that we do not reject the null hypothesis, which indicates
# that the parameter estimation for the corresponding variable could be 0.

#BIC For k=3 using only significant variables

Data_K3SIG = data.frame(matrix(ncol = 24, nrow = 0))

# we start from 2 because it is the first value of the quarterly "s" that
# we can forecast

for(i in 2:12){
  Data_K3SIG <- rbind(Data_K3SIG,
                   c(AnalysisMdata[(3*i)-1,-c(3,4,5,6)],
                     AnalysisMdata[(3*i)-2,-c(2,3,5)],
                     AnalysisMdata[(3*i)-3,-c(1,3,5,6)],
                     AnalysisMdata[(3*i)-4,-c(2,3,6)]))
  colnames(Data_K3SIG) <- c()
}

Forecasting_MIDASBIC_K3SIG= c()
vec_Forecasting_error_MIDASBIC_K3SIG = c()

for (i in 13:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K3SIG = rep(1,(i-1))
  K3SIG_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K3SIG <- rbind(Data_K3SIG,
                   c(AnalysisMdata[(3*i)-1,-c(3,4,5,6)],
                     AnalysisMdata[(3*i)-2,-c(2,3,5)],
                     AnalysisMdata[(3*i)-3,-c(1,3,5,6)],
                     AnalysisMdata[(3*i)-4,-c(2,3,6)]))
  
  z_BIC_K3SIG = rbind(ones_BIC_K3SIG,
                   K3SIG_L2_GDP,
                   t(Data_K3SIG))
  
  Y_BIC_K3SIG = rbind(AnalysisQdata$GDPgrowth[2:(i)])
  
  C_BIC_K3SIG = solve(z_BIC_K3SIG%*%t(z_BIC_K3SIG))
  B_BIC_K3SIG = Y_BIC_K3SIG%*%t(z_BIC_K3SIG)
  A_BIC_K3SIG = B_BIC_K3SIG%*%C_BIC_K3SIG
  
  # As a reference we first predict the observation 28 
  
  pred_BIC_K3SIG= A_BIC_K3SIG[1,1] +
    (A_BIC_K3SIG[1,2]*AnalysisQdata$GDPgrowth[i])+
    (A_BIC_K3SIG[1,3]*AnalysisMdata$CUMFNS[(3*(i+1))-1])+
    (A_BIC_K3SIG[1,4]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K3SIG[1,5]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K3SIG[1,6]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K3SIG[1,7]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])+
    (A_BIC_K3SIG[1,8]*AnalysisMdata$UNRATE[(3*(i+1))-3])+
    (A_BIC_K3SIG[1,9]*AnalysisMdata$FEDFUNDS[(3*(i+1))-3])+
    (A_BIC_K3SIG[1,10]*AnalysisMdata$CUMFNS[(3*(i+1))-4])+
    (A_BIC_K3SIG[1,11]*AnalysisMdata$FEDFUNDS[(3*(i+1))-4])+
    (A_BIC_K3SIG[1,12]*AnalysisMdata$M2REAL_growth[(3*(i+1))-4])
    
  
  Forecasting_MIDASBIC_K3SIG <-  append(Forecasting_MIDASBIC_K3SIG,pred_BIC_K3SIG)
  
  vec_GDPgrowth_MIDASgraph_K3SIG <- AnalysisQdata$GDPgrowth[14:(i+1)]
  vec_Forecasting_error_MIDASBIC_K3SIG <- append(vec_Forecasting_error_MIDASBIC_K3SIG,(sqrt((1/length(vec_GDPgrowth_MIDASgraph_K3SIG))*sum((vec_GDPgrowth_MIDASgraph_K3SIG-Forecasting_MIDASBIC_K3SIG)^2))))
  
  
}


# The Nowcast error for Unrestricted MIDAS 


GDPgrowth_graph_MIDASK3SIG <- ts(AnalysisQdata$GDPgrowth, start= c(1959,2), frequency = 4, end=c(2021,4))
Forecastingtis_MIDASK3SIG <- ts(Forecasting_MIDASBIC_K3SIG, start= c(1962,3), frequency = 4, end=c(2021,4))

GDPgrowth_MIDASgraph_K3SIG <- AnalysisQdata$GDPgrowth[-c(1:13)]

pdf(file = "fig/MIDAS3sig_GDP.pdf",   # The directory you want to save the file in
    width = 17, # The width of the plot in inches
    height = 10) # The height of the plot in inches

par(mfrow=c(1,1))
ts.plot(GDPgrowth_graph_MIDASK3SIG, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]", ylim=c(-10,10),lwd = 2)
points(Forecastingtis_MIDASK3SIG, col= 'red', type = 'l', lty=1,lwd = 2)
legend("bottomleft", legend=c("GDP growth","MIDAS Nowcast GDP growth"),
       col=c("black", "red"), lty=c("solid","solid"),lwd = c(3,3), cex = 1.5)

## Run dev.off() to create the PDF file
dev.off()

vec_tis_Forecasting_error_MIDASBIC_K3SIG <- ts(vec_Forecasting_error_MIDASBIC_K3SIG, start= c(1962,3), frequency = 4, end=c(2021,4))

ts.plot(vec_tis_Forecasting_error_MIDASBIC_K3SIG, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]", ylim=c(0,2),lwd = 2)

forecasting_error_MIDASK3SIG= (sqrt((1/length(GDPgrowth_MIDASgraph_K3SIG))*sum((GDPgrowth_MIDASgraph_K3SIG-Forecasting_MIDASBIC_K3SIG)^2)))
forecasting_error_MIDASK3SIG

# 0.6692301


###########***Part d Significance of the variables K=3 Model 2***##############

# we estimate the variance of the error 

# T-test statistic model 2 BIC K3N

Resd_1_K3N = Y_BIC_K3N - (A_BIC_K3N%*%z_BIC_K3N)

var_BIC_1N= solve(z_BIC_K3N%*%t(z_BIC_K3N))
var_BIC_2N= ((Resd_1_K3N%*%t(Resd_1_K3N))/(250-(31*1)-1))
var_BIC_K3N = kronecker(var_BIC_1N,var_BIC_2N)


t_K3N <- c()

for (i in 1:length(A_BIC_K3N)){
  t_K3N = append(t_K3N, (A_BIC_K3N[1,i]/sqrt(var_BIC_K3N[i,i])))
}

names(t_K3N) <- c('K3N_Intercept',
                 'K3N_GDPgrowth_L1',
                 'K3N_CUMFNS',
                 'K3N_UNRATE',
                 'K3N_CPIAUCSL',
                 'K3N_FEDFUNDS',
                 'K3N_M2REAL_growth',
                 'K3N_S.P.500_growth',
                 'K3N_L1_CUMFNS',
                 'K3N_L1_UNRATE',
                 'K3N_L1_CPIAUCSL',
                 'K3N_L1_FEDFUNDS',
                 'K3N_L1_M2REAL_growth',
                 'K3N_L1_S.P.500_growth',
                 'K3N_L2_CUMFNS',
                 'K3N_L2_UNRATE',
                 'K3N_L2_CPIAUCSL',
                 'K3N_L2_FEDFUNDS',
                 'K3N_L2_M2REAL_growth',
                 'K3N_L2_S.P.500_growth',
                 'K3N_L3_CUMFNS',
                 'K3N_L3_UNRATE',
                 'K3N_L3_CPIAUCSL',
                 'K3N_L3_FEDFUNDS',
                 'K3N_L3_M2REAL_growth',
                 'K3N_L3_S.P.500_growth',
                 'K3N_L4_CUMFNS',
                 'K3N_L4_UNRATE',
                 'K3N_L4_CPIAUCSL',
                 'K3N_L4_FEDFUNDS',
                 'K3N_L4_M2REAL_growth',
                 'K3N_L4_S.P.500_growth')

not_significance_K3N <- (t_K3N < 1.96) & (t_K3N >-1.96)
not_significance_K3N

#BIC For k=3

Data_K3NSIG = data.frame(matrix(ncol = 30, nrow = 0))

for(i in 2:11){
  Data_K3NSIG <- rbind(Data_K3NSIG,
                    c(AnalysisMdata[(3*i),-c(1,2,3,4,5,6)],
                      AnalysisMdata[(3*i)-1,-c(1,3,4,5,6)],
                      AnalysisMdata[(3*i)-2,-c(2,3,5)],
                      AnalysisMdata[(3*i)-3,-c(1,3,5,6)],
                      AnalysisMdata[(3*i)-4,-c(2,3,6)]))
  colnames(Data_K3NSIG) <- c()
}

Forecasting_MIDASBIC_K3NSIG= c()
vec_Forecasting_error_MIDASBIC_K3NSIG= c()

for (i in 12:(length(AnalysisQdata$GDPgrowth)-1)){
  
  ones_BIC_K3NSIG = rep(1,(i-1))
  K3NSIG_L2_GDP = AnalysisQdata$GDPgrowth[1:(i-1)]
  Data_K3NSIG <- rbind(Data_K3NSIG,
                    c(AnalysisMdata[(3*i),-c(1,2,3,4,5,6)],
                      AnalysisMdata[(3*i)-1,-c(1,3,4,5,6)],
                      AnalysisMdata[(3*i)-2,-c(2,3,5)],
                      AnalysisMdata[(3*i)-3,-c(1,3,5,6)],
                      AnalysisMdata[(3*i)-4,-c(2,3,6)]))
  
  z_BIC_K3NSIG = rbind(ones_BIC_K3NSIG,
                    K3NSIG_L2_GDP,
                    t(Data_K3NSIG))
  
  Y_BIC_K3NSIG = rbind(AnalysisQdata$GDPgrowth[2:(i)])
  
  C_BIC_K3NSIG = solve(z_BIC_K3NSIG%*%t(z_BIC_K3NSIG))
  B_BIC_K3NSIG = Y_BIC_K3NSIG%*%t(z_BIC_K3NSIG)
  A_BIC_K3NSIG = B_BIC_K3NSIG%*%C_BIC_K3NSIG
  
  pred_BIC_K3NSIG= A_BIC_K3NSIG[1,1] +
    (A_BIC_K3NSIG[1,2]*AnalysisQdata$GDPgrowth[i])+
    (A_BIC_K3NSIG[1,3]*AnalysisMdata$UNRATE[(3*(i+1))-1])+
    (A_BIC_K3NSIG[1,4]*AnalysisMdata$CUMFNS[(3*(i+1))-2])+
    (A_BIC_K3NSIG[1,5]*AnalysisMdata$FEDFUNDS[(3*(i+1))-2])+
    (A_BIC_K3NSIG[1,6]*AnalysisMdata$S.P.500_growth[(3*(i+1))-2])+
    (A_BIC_K3NSIG[1,7]*AnalysisMdata$UNRATE[(3*(i+1))-3])+
    (A_BIC_K3NSIG[1,8]*AnalysisMdata$FEDFUNDS[(3*(i+1))-3])+
    (A_BIC_K3NSIG[1,9]*AnalysisMdata$CUMFNS[(3*(i+1))-4])+
    (A_BIC_K3NSIG[1,10]*AnalysisMdata$FEDFUNDS[(3*(i+1))-4])+
    (A_BIC_K3NSIG[1,11]*AnalysisMdata$M2REAL_growth[(3*(i+1))-4])
    
  
  Forecasting_MIDASBIC_K3NSIG <-  append(Forecasting_MIDASBIC_K3NSIG,pred_BIC_K3NSIG)
  
  vec_GDPgrowth_MIDASgraph_K3NSIG <- AnalysisQdata$GDPgrowth[13:(i+1)]
  vec_Forecasting_error_MIDASBIC_K3NSIG <- append(vec_Forecasting_error_MIDASBIC_K3NSIG,(sqrt((1/length(vec_GDPgrowth_MIDASgraph_K3NSIG))*sum((vec_GDPgrowth_MIDASgraph_K3NSIG-Forecasting_MIDASBIC_K3NSIG)^2))))
  
}

# The Nowcast error for Unrestricted MIDAS 

GDPgrowth_graph_MIDASK3NSIG <- ts(AnalysisQdata$GDPgrowth, start= c(1959,2), frequency = 4, end=c(2021,4))
Forecastingtis_MIDASK3NSIG <- ts(Forecasting_MIDASBIC_K3NSIG, start= c(1962,2), frequency = 4, end=c(2021,4))

GDPgrowth_MIDASgraph_K3NSIG <- AnalysisQdata$GDPgrowth[-c(1:12)]

pdf(file = "fig/MIDASK3NSIG_GDP.pdf",   # The directory you want to save the file in
    width = 17, # The width of the plot in inches
    height = 10) # The height of the plot in inches

par(mfrow=c(1,1))
ts.plot(GDPgrowth_graph_MIDASK3NSIG, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]", ylim=c(-10,10),lwd = 2)
points(Forecastingtis_MIDASK3NSIG, col= 'red', type = 'l', lty=1,lwd = 2)
legend("bottomleft", legend=c("GDP growth","MIDAS Nowcast GDP growth Model 2"),
       col=c("black", "red"), lty=c("solid","solid"),lwd = c(3,3), cex = 1.5)

## Run dev.off() to create the PDF file
dev.off()

forecasting_error_MIDASK3NSIG= (sqrt((1/length(GDPgrowth_MIDASgraph_K3NSIG))*sum((GDPgrowth_MIDASgraph_K3NSIG-Forecasting_MIDASBIC_K3NSIG)^2)))
forecasting_error_MIDASK3NSIG

# 0.7150504

#Error Plot 

vec_tis_Forecasting_error_MIDASBIC_K3NSIG <- ts(vec_Forecasting_error_MIDASBIC_K3NSIG, start= c(1962,2), frequency = 4, end=c(2021,4))

ts.plot(vec_tis_Forecasting_error_MIDASBIC_K3NSIG, type = 'l', xlab="Time [Years]", ylab="RMSFE", ylim=c(0,1.5),lwd = 2)

###########***Part e Random Forest with hyper parameter tuning***##############
#####* Loading the Random Forest Library
library(ranger)

## Now we create the data that will be used in the Random Forest

randomforest_data <- data.frame()

for (i in 2:length(AnalysisQdata$GDPgrowth)) {
  
  randomforest_data <- rbind(randomforest_data, 
                            c(AnalysisQdata$GDPgrowth[(i-1):i],
                              AnalysisMdata$CUMFNS[((3*(i))-4):(3*(i))],
                              AnalysisMdata$UNRATE[((3*(i))-4):(3*(i))],
                              AnalysisMdata$CPIAUCSL[((3*(i))-4):(3*(i))],
                              AnalysisMdata$FEDFUNDS[((3*(i))-4):(3*(i))],
                              AnalysisMdata$M2REAL_growth[((3*(i))-4):(3*(i))],
                              AnalysisMdata$S.P.500_growth[((3*(i))-4):(3*(i))]))
  
}

randomforest_data

colnames(randomforest_data) <- c('GDPgrowth_pred',
                  'GDPgrowth_L1',
                  'CUMFNS',
                  'CUMFNS_L1',
                  'CUMFNS_L2',
                  'CUMFNS_L3',
                  'CUMFNS_L4',
                  'UNRATE',
                  'UNRATE_L1',
                  'UNRATE_L2',
                  'UNRATE_L3',
                  'UNRATE_L4',
                  'CPIAUCSL',
                  'CPIAUCSL_L1',
                  'CPIAUCSL_L2',
                  'CPIAUCSL_L3',
                  'CPIAUCSL_L4',
                  'FEDFUNDS',
                  'FEDFUNDS_L1',
                  'FEDFUNDS_L2',
                  'FEDFUNDS_L3',
                  'FEDFUNDS_L4',
                  'M2REAL_growth',
                  'M2REAL_growth_L1',
                  'M2REAL_growth_L2',
                  'M2REAL_growth_L3',
                  'M2REAL_growth_L4',
                  'S.P.500_growth',
                  'S.P.500_growth_L1',
                  'S.P.500_growth_L2',
                  'S.P.500_growth_L3',
                  'S.P.500_growth_L4')

opt_par <- array(0, dim=c(3, 3, 3, 3),
                 dimnames = list(c("Tiefe 8", "Tiefe 9", "Tiefe 10"),
                                 c("500 trees", "750 trees", "1000 trees"),
                                 c("2 Variablen pro Splitt", "3 Variablen pro Splitt", "4 Variablen pro Splitt"),
                                 c("Min_node_size_3", "Min_node_size_4", "Min_node_size_5")))
opt_par


