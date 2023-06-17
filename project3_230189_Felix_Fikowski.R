# Case Studies - Report 3 (Felix Fikowski, 280189)

################################################################################
#Laden benötigter Pakete
{
  library(ggplot2)
  library(ggpubr)
  library(ranger)
  library(xtable)
}
################################################################################

#Setzen des Dateipfades

#Import of the data
{
  setwd("C:\\Users\\Felix\\Documents\\TU Dortmund\\Case Studies\\project 3")
  
  df_quarterly <- read.csv("2022-02.csv")
  df_quarterly <- as.data.frame(df_quarterly)
  head(df_quarterly)
  dim(df_quarterly)
  #[1] 256 247
  
  #remove uninformative rows:
  df_quarterly <- df_quarterly[-c(1:2, 255:256), ] 
  
  colnames(df_quarterly)
  df_quarterly <- df_quarterly[, c("sasdate","GDPC1")]
  
  head(df_quarterly)
  
  dim(df_quarterly)
  #[1] 256   7
}

#xtable(con_table)


################################################################################
#Transform the data as mentioned in the project description
{
  matrix <- data.matrix(df_quarterly, rownames.force = NA)
  
  #transform GDP (GDP1)
  GDP <- matrix[,2]
  GDP_per <- diff(GDP,lag=1, differences = 1)/GDP[1:251]*100
  
  per_matrix <- cbind(GDP_per)
  head(per_matrix)
  
  dates <- as.Date(df_quarterly[2:252,1], "%m/%d/%Y")
  
  data_quarterly <- data.frame(dates, GDP_per)
  colnames(data_quarterly) <- c("dates GDP growth", "GDP_per")
  head(data_quarterly)
  dim(data_quarterly)
}

################################################################################
#Data preperation
{
  df_monthly <- read.csv("2022-02_mon.csv")
  df_monthly <- as.data.frame(df_monthly)
  head(df_monthly)
  tail(df_monthly)
  
  colnames(df_monthly)
  df_monthly <- df_monthly[2:759, c("sasdate","CUMFNS","UNRATE","CPIAUCSL","FEDFUNDS","M2REAL","S.P.500")]
  
  head(df_monthly)
  
  dim(df_monthly)
  
  #transform consumerprice index (CPIAUCSL  )
  CPIAUCSL <- df_monthly[,4]
  CPIAUCSL_per <- diff(CPIAUCSL,lag=1, differences = 1)/CPIAUCSL[1:757]*100
  head(CPIAUCSL_per)
  
  #transform M2 money stock (M2REAL)
  M2REAL <- df_monthly[,6]
  M2REAL_per <- diff(M2REAL,lag=1, differences = 1)/M2REAL[1:757]*100
  head(M2REAL_per)
  
  #transform S.P.500 (S.P.500)
  S.P.500 <- df_monthly[,7]
  S.P.500_per <- diff(S.P.500,lag=1, differences = 1)/S.P.500[1:757]*100
  head(S.P.500_per)
  dates <- as.Date(df_monthly[2:756,1], "%m/%d/%Y")
  
  df_monthly1 <- data.frame(dates, df_monthly[2:756,-c(1,4,6,7)], CPIAUCSL_per[1:755], M2REAL_per[1:755], S.P.500_per[1:755])
  colnames(df_monthly1) <- c("dates", "CUMFNS","UNRATE","FEDFUNDS","CPIAUCSL_per","M2REAL_per","S.P.500_per")
  
  dim(df_monthly1)
  df_monthly1[1:10,]
  tail(df_monthly1)
  
}

{
  #5-Number summary of the data:
  
  CUMFNS<-summary(df_monthly1$CUMFNS)
  UNRATE<-summary(df_monthly1$UNRATE)
  CPIAUCSL_per<-summary(df_monthly1$CPIAUCSL_per)
  FEDFUNDS<-summary(df_monthly1$FEDFUNDS)
  M2REAL_per<-summary(df_monthly1$M2REAL_per)
  S.P.500_per<-summary(df_monthly1$S.P.500_per)
  
  
  compare1<-cbind(CUMFNS, UNRATE, CPIAUCSL_per, FEDFUNDS, M2REAL_per, S.P.500_per)
  con_table <- round(compare1,2)
  xtable(con_table)
}

#plot generation of the 6 monthly time series
{
  series_CUMFNS <-ggplot(data = df_monthly1, aes(x=dates, y=CUMFNS)) +
    geom_line() + 
    ylab("CUMFNS [%]") +
    xlab("")
  
  series_UNRATE <-ggplot(df_monthly1, aes(x=dates, y=UNRATE)) +
    geom_line() + 
    ylab("UNRATE [%]") +
    xlab("")
  
  series_CPIAUCSL_per <-ggplot(df_monthly1, aes(x=dates, y=CPIAUCSL_per)) +
    geom_line() + 
    ylab("Inflation [%]") +
    xlab("")
  
  series_FEDFUNDS <-ggplot(df_monthly1, aes(x=dates, y=FEDFUNDS)) +
    geom_line() + 
    ylab("FEDFUNDS [%]") +
    xlab("")
  
  series_M2REAL_per <-ggplot(df_monthly1, aes(x=dates, y=M2REAL_per)) +
    geom_line() + 
    ylab("M2REAL gr. [%]") +
    xlab("year")
  
  series_S.P.500_per <-ggplot(df_monthly1, aes(x=dates, y=S.P.500_per)) +
    geom_line() +
    ylab("S.P.500 gr. [%]") +
    xlab("year")
  
  figure <- ggarrange(series_CUMFNS, series_UNRATE, series_CPIAUCSL_per, 
                      series_FEDFUNDS, series_M2REAL_per, series_S.P.500_per,
                      ncol = 2, nrow = 3)
  figure <- figure +theme(plot.margin = margin(0.5,0,0,0, "cm"))
  
  ggsave("series.pdf",plot =figure)
  figure
}
#preparing the data
{
  
  intercept <- array(1,250)
  
  data1 <- data.frame(data_quarterly[2:251,], intercept)
  data2 <- data.frame(data_quarterly[2:251,], intercept)
  data3 <- data.frame(data_quarterly[2:251,], intercept)
  data4 <- data.frame(data_quarterly[2:251,], intercept)
  data5 <- data.frame(data_quarterly[2:251,], intercept)
  data6 <- data.frame(data_quarterly[2:251,], intercept)
}

{
  M2REAL_per <- c()
  for (i in 1:250) {
    M2REAL_per_position <- df_monthly1$M2REAL_per[5+3*i]
    M2REAL_per <- c(M2REAL_per, M2REAL_per_position)    
  }
  M2REAL_per
  length(M2REAL_per)
  
  M2REAL_per1 <- c()
  for (i in 1:250) {
    M2REAL_per1_position <- df_monthly1$M2REAL_per[4+3*i]
    M2REAL_per1 <- c(M2REAL_per1, M2REAL_per1_position)    
  }
  M2REAL_per1
  length(M2REAL_per1)
  
  M2REAL_per2 <- c()
  for (i in 1:250) {
    M2REAL_per2_position <- df_monthly1$M2REAL_per[3+3*i]
    M2REAL_per2 <- c(M2REAL_per2, M2REAL_per2_position)    
  }
  M2REAL_per2
  length(M2REAL_per2)
  
  M2REAL_per3 <- c()
  for (i in 1:250) {
    M2REAL_per3_position <- df_monthly1$M2REAL_per[2+3*i]
    M2REAL_per3 <- c(M2REAL_per3, M2REAL_per3_position)    
  }
  M2REAL_per3
  length(M2REAL_per3)
  
  M2REAL_per4 <- c()
  for (i in 1:250) {
    M2REAL_per4_position <- df_monthly1$M2REAL_per[1+3*i]
    M2REAL_per4 <- c(M2REAL_per4, M2REAL_per4_position)    
  }
  M2REAL_per4
  length(M2REAL_per4)
  
  GDP_per1 <- c()
  for (i in 1:250) {
    GDP_per1_position <- data_quarterly$GDP_per[i]
    GDP_per1 <- c(GDP_per1, GDP_per1_position)    
  }
  GDP_per1
  length(GDP_per1)
  
  data1 <- data.frame(data1, GDP_per1, M2REAL_per1, M2REAL_per2)
  data2 <- data.frame(data2, GDP_per1, M2REAL_per1, M2REAL_per2, M2REAL_per3)
  data3 <- data.frame(data3, GDP_per1, M2REAL_per1, M2REAL_per2,M2REAL_per3, M2REAL_per4)
  data4 <- data.frame(data4, GDP_per1, M2REAL_per, M2REAL_per1, M2REAL_per2)
  data5 <- data.frame(data5, GDP_per1, M2REAL_per, M2REAL_per1, M2REAL_per2, M2REAL_per3)
  data6 <- data.frame(data6, GDP_per1, M2REAL_per, M2REAL_per1, M2REAL_per2,M2REAL_per3, M2REAL_per4)
  
  CUMFNS <- c()
  for (i in 1:250) {
    CUMFNS_position <- df_monthly1$CUMFNS[5+3*i]
    CUMFNS <- c(CUMFNS, CUMFNS_position)    
  }
  CUMFNS
  length(CUMFNS)
  
  CUMFNS1 <- c()
  for (i in 1:250) {
    CUMFNS1_position <- df_monthly1$CUMFNS[4+3*i]
    CUMFNS1 <- c(CUMFNS1, CUMFNS1_position)    
  }
  CUMFNS1
  length(CUMFNS1)
  
  CUMFNS2 <- c()
  for (i in 1:250) {
    CUMFNS2_position <- df_monthly1$CUMFNS[3+3*i]
    CUMFNS2 <- c(CUMFNS2, CUMFNS2_position)    
  }
  CUMFNS2
  length(CUMFNS2)
  
  CUMFNS3 <- c()
  for (i in 1:250) {
    CUMFNS3_position <- df_monthly1$CUMFNS[2+3*i]
    CUMFNS3 <- c(CUMFNS3, CUMFNS3_position)    
  }
  CUMFNS3
  length(CUMFNS3)
  
  CUMFNS4 <- c()
  for (i in 1:250) {
    CUMFNS4_position <- df_monthly1$CUMFNS[1+3*i]
    CUMFNS4 <- c(CUMFNS4, CUMFNS4_position)    
  }
  CUMFNS4
  length(CUMFNS4)
  
  data1 <- data.frame(data1, CUMFNS1, CUMFNS2)
  data2 <- data.frame(data2, CUMFNS1, CUMFNS2, CUMFNS3)
  data3 <- data.frame(data3, CUMFNS1, CUMFNS2,CUMFNS3, CUMFNS4)
  data4 <- data.frame(data4, CUMFNS, CUMFNS1, CUMFNS2)
  data5 <- data.frame(data5, CUMFNS, CUMFNS1, CUMFNS2, CUMFNS3)
  data6 <- data.frame(data6, CUMFNS, CUMFNS1, CUMFNS2,CUMFNS3, CUMFNS4)
  
  UNRATE <- c()
  for (i in 1:250) {
    UNRATE_position <- df_monthly1$UNRATE[5+3*i]
    UNRATE <- c(UNRATE, UNRATE_position)    
  }
  UNRATE
  length(UNRATE)
  
  UNRATE1 <- c()
  for (i in 1:250) {
    UNRATE1_position <- df_monthly1$UNRATE[4+3*i]
    UNRATE1 <- c(UNRATE1, UNRATE1_position)    
  }
  UNRATE1
  length(UNRATE1)
  
  UNRATE2 <- c()
  for (i in 1:250) {
    UNRATE2_position <- df_monthly1$UNRATE[3+3*i]
    UNRATE2 <- c(UNRATE2, UNRATE2_position)    
  }
  UNRATE2
  length(UNRATE2)
  
  UNRATE3 <- c()
  for (i in 1:250) {
    UNRATE3_position <- df_monthly1$UNRATE[2+3*i]
    UNRATE3 <- c(UNRATE3, UNRATE3_position)    
  }
  UNRATE3
  length(UNRATE3)
  
  UNRATE4 <- c()
  for (i in 1:250) {
    UNRATE4_position <- df_monthly1$UNRATE[1+3*i]
    UNRATE4 <- c(UNRATE4, UNRATE4_position)    
  }
  UNRATE4
  length(UNRATE4)
  
  data1 <- data.frame(data1, UNRATE1, UNRATE2)
  data2 <- data.frame(data2, UNRATE1, UNRATE2, UNRATE3)
  data3 <- data.frame(data3, UNRATE1, UNRATE2,UNRATE3, UNRATE4)
  data4 <- data.frame(data4, UNRATE, UNRATE1, UNRATE2)
  data5 <- data.frame(data5, UNRATE, UNRATE1, UNRATE2, UNRATE3)
  data6 <- data.frame(data6, UNRATE, UNRATE1, UNRATE2,UNRATE3, UNRATE4)
  
  FEDFUNDS <- c()
  for (i in 1:250) {
    FEDFUNDS_position <- df_monthly1$FEDFUNDS[5+3*i]
    FEDFUNDS <- c(FEDFUNDS, FEDFUNDS_position)    
  }
  FEDFUNDS
  length(FEDFUNDS)
  
  FEDFUNDS1 <- c()
  for (i in 1:250) {
    FEDFUNDS1_position <- df_monthly1$FEDFUNDS[4+3*i]
    FEDFUNDS1 <- c(FEDFUNDS1, FEDFUNDS1_position)    
  }
  FEDFUNDS1
  length(FEDFUNDS1)
  
  FEDFUNDS2 <- c()
  for (i in 1:250) {
    FEDFUNDS2_position <- df_monthly1$FEDFUNDS[3+3*i]
    FEDFUNDS2 <- c(FEDFUNDS2, FEDFUNDS2_position)    
  }
  FEDFUNDS2
  length(FEDFUNDS2)
  
  FEDFUNDS3 <- c()
  for (i in 1:250) {
    FEDFUNDS3_position <- df_monthly1$FEDFUNDS[2+3*i]
    FEDFUNDS3 <- c(FEDFUNDS3, FEDFUNDS3_position)    
  }
  FEDFUNDS3
  length(FEDFUNDS3)
  
  FEDFUNDS4 <- c()
  for (i in 1:250) {
    FEDFUNDS4_position <- df_monthly1$FEDFUNDS[1+3*i]
    FEDFUNDS4 <- c(FEDFUNDS4, FEDFUNDS4_position)    
  }
  FEDFUNDS4
  length(FEDFUNDS4)
  
  data1 <- data.frame(data1, FEDFUNDS1, FEDFUNDS2)
  data2 <- data.frame(data2, FEDFUNDS1, FEDFUNDS2, FEDFUNDS3)
  data3 <- data.frame(data3, FEDFUNDS1, FEDFUNDS2,FEDFUNDS3, FEDFUNDS4)
  data4 <- data.frame(data4, FEDFUNDS, FEDFUNDS1, FEDFUNDS2)
  data5 <- data.frame(data5, FEDFUNDS, FEDFUNDS1, FEDFUNDS2, FEDFUNDS3)
  data6 <- data.frame(data6, FEDFUNDS, FEDFUNDS1, FEDFUNDS2,FEDFUNDS3, FEDFUNDS4)
  
  CPIAUCSL_per <- c()
  for (i in 1:250) {
    CPIAUCSL_per_position <- df_monthly1$CPIAUCSL_per[5+3*i]
    CPIAUCSL_per <- c(CPIAUCSL_per, CPIAUCSL_per_position)    
  }
  CPIAUCSL_per
  length(CPIAUCSL_per)
  
  CPIAUCSL_per1 <- c()
  for (i in 1:250) {
    CPIAUCSL_per1_position <- df_monthly1$CPIAUCSL_per[4+3*i]
    CPIAUCSL_per1 <- c(CPIAUCSL_per1, CPIAUCSL_per1_position)    
  }
  CPIAUCSL_per1
  length(CPIAUCSL_per1)
  
  CPIAUCSL_per2 <- c()
  for (i in 1:250) {
    CPIAUCSL_per2_position <- df_monthly1$CPIAUCSL_per[3+3*i]
    CPIAUCSL_per2 <- c(CPIAUCSL_per2, CPIAUCSL_per2_position)    
  }
  CPIAUCSL_per2
  length(CPIAUCSL_per2)
  
  CPIAUCSL_per3 <- c()
  for (i in 1:250) {
    CPIAUCSL_per3_position <- df_monthly1$CPIAUCSL_per[2+3*i]
    CPIAUCSL_per3 <- c(CPIAUCSL_per3, CPIAUCSL_per3_position)    
  }
  CPIAUCSL_per3
  length(CPIAUCSL_per3)
  
  CPIAUCSL_per4 <- c()
  for (i in 1:250) {
    CPIAUCSL_per4_position <- df_monthly1$CPIAUCSL_per[1+3*i]
    CPIAUCSL_per4 <- c(CPIAUCSL_per4, CPIAUCSL_per4_position)    
  }
  CPIAUCSL_per4
  length(CPIAUCSL_per4)
  
  data1 <- data.frame(data1, CPIAUCSL_per1, CPIAUCSL_per2)
  data2 <- data.frame(data2, CPIAUCSL_per1, CPIAUCSL_per2, CPIAUCSL_per3)
  data3 <- data.frame(data3, CPIAUCSL_per1, CPIAUCSL_per2,CPIAUCSL_per3, CPIAUCSL_per4)
  data4 <- data.frame(data4, CPIAUCSL_per, CPIAUCSL_per1, CPIAUCSL_per2)
  data5 <- data.frame(data5, CPIAUCSL_per, CPIAUCSL_per1, CPIAUCSL_per2, CPIAUCSL_per3)
  data6 <- data.frame(data6, CPIAUCSL_per, CPIAUCSL_per1, CPIAUCSL_per2,CPIAUCSL_per3, CPIAUCSL_per4)
  
  S.P.500_per <- c()
  for (i in 1:250) {
    S.P.500_per_position <- df_monthly1$S.P.500_per[5+3*i]
    S.P.500_per <- c(S.P.500_per, S.P.500_per_position)    
  }
  S.P.500_per
  length(S.P.500_per)
  
  S.P.500_per1 <- c()
  for (i in 1:250) {
    S.P.500_per1_position <- df_monthly1$S.P.500_per[4+3*i]
    S.P.500_per1 <- c(S.P.500_per1, S.P.500_per1_position)    
  }
  S.P.500_per1
  length(S.P.500_per1)
  
  S.P.500_per2 <- c()
  for (i in 1:250) {
    S.P.500_per2_position <- df_monthly1$S.P.500_per[3+3*i]
    S.P.500_per2 <- c(S.P.500_per2, S.P.500_per2_position)    
  }
  S.P.500_per2
  length(S.P.500_per2)
  
  S.P.500_per3 <- c()
  for (i in 1:250) {
    S.P.500_per3_position <- df_monthly1$S.P.500_per[2+3*i]
    S.P.500_per3 <- c(S.P.500_per3, S.P.500_per3_position)    
  }
  S.P.500_per3
  length(S.P.500_per3)
  
  S.P.500_per4 <- c()
  for (i in 1:250) {
    S.P.500_per4_position <- df_monthly1$S.P.500_per[1+3*i]
    S.P.500_per4 <- c(S.P.500_per4, S.P.500_per4_position)    
  }
  S.P.500_per4
  length(S.P.500_per4)
  
  
  data1 <- data.frame(data1, S.P.500_per1, S.P.500_per2)
  data2 <- data.frame(data2, S.P.500_per1, S.P.500_per2, S.P.500_per3)
  data3 <- data.frame(data3, S.P.500_per1, S.P.500_per2,S.P.500_per3, S.P.500_per4)
  data4 <- data.frame(data4, S.P.500_per, S.P.500_per1, S.P.500_per2)
  data5 <- data.frame(data5, S.P.500_per, S.P.500_per1, S.P.500_per2, S.P.500_per3)
  data6 <- data.frame(data6, S.P.500_per, S.P.500_per1, S.P.500_per2,S.P.500_per3, S.P.500_per4)
  
  data1 <- data.frame(data1[4:250,])
  data2 <- data.frame(data2[4:250,])
  data3 <- data.frame(data3[4:250,])
  data4 <- data.frame(data4[4:250,])
  data5 <- data.frame(data5[4:250,])
  data6 <- data.frame(data6[4:250,])
}

{
  matrix1 <- data.matrix(data1, rownames.force = NA)
  matrix2 <- data.matrix(data2, rownames.force = NA)
  matrix3 <- data.matrix(data3, rownames.force = NA)
  matrix4 <- data.matrix(data4, rownames.force = NA)
  matrix5 <- data.matrix(data5, rownames.force = NA)
  matrix6 <- data.matrix(data6, rownames.force = NA)
}

################################################################################
#a)
#Generation of the one-step-ahead forecast with calculation of the RMSFE for MIDAS 1 v=1
forecast_uMIDASmatrix1= c()
rmsfe_matrix1 = c()
for (i in 1:(dim(matrix1[,-c(1,2)])[1]-dim(matrix1[,-c(1,2)])[2])){
  #dim(matrix1[,-c(1,2)])[1] = 250; dim(matrix1[,-c(1,2)])[2]=14

  z = matrix1[1:((dim(matrix1[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix1[1:((dim(matrix1[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  
  pred= matrix1[(dim(matrix1[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix1 <-  append(forecast_uMIDASmatrix1,pred)
  rmsfe <- sqrt(mean((matrix1[((dim(matrix1[,-c(1,2)])[2]+1):(dim(matrix1[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix1)^2))
  rmsfe_matrix1 <-  append(rmsfe_matrix1,rmsfe)
}
forecast_uMIDASmatrix1
rmsfe_matrix1

#store the forecast in a data frame
na <- rep(NA,14)
forecast<-append(na, forecast_uMIDASmatrix1)
forecasts1 <- data.frame(data6$dates,data6$GDP_per, forecast)
colnames(forecasts1) <- c("dates","GDP_per", "mod1_1")


#overall RMSFE
RMSE_umidas1_oneahead <- sqrt(mean((forecasts1$GDP_per[15:247] - forecasts1$mod1_1[15:247])^2))
RMSE_umidas1_oneahead
#1.400675

#BIC calculation
Y1 = matrix1[1:(13+234),2]
z1 = matrix1[1:(13+234),-c(1,2)]
C1 = solve(t(z1)%*%z1)
B1 = t(z1)%*%Y1
A1 = C1%*%B1
res1 = Y1 - (z1%*%A1)


sigma_hat1 <- sum(t(res1)%*%res1)/length(Y1)

BIC1= log((sigma_hat1)) + length(A1)/length(Y1)*log(length(Y1))
BIC1
#0.1271827

#Generation of the one-step-ahead forecast with calculation of the RMSFE for MIDAS 1 v=2
rmsfe_matrix2 = c()
forecast_uMIDASmatrix2= c()
for (i in 1:(dim(matrix2[,-c(1,2)])[1]-dim(matrix2[,-c(1,2)])[2])){
  
  z = matrix2[1:((dim(matrix2[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix2[1:((dim(matrix2[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  i
  pred= matrix2[(dim(matrix2[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix2 <-  append(forecast_uMIDASmatrix2,pred)
  rmsfe <- sqrt(mean((matrix2[((dim(matrix2[,-c(1,2)])[2]+1):(dim(matrix2[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix2)^2))
  rmsfe_matrix2 <-  append(rmsfe_matrix2,rmsfe)
}
forecast_uMIDASmatrix2
rmsfe_matrix2

#store the forecast in a data frame
na <- rep(NA,20)
forecast<-append(na, forecast_uMIDASmatrix2)
forecasts2 <- data.frame(forecasts1, forecast)
colnames(forecasts2) <- c("dates","GDP_per", "mod1_1", "mod1_2")

umidas2_oneahead_plot <- ggplot(forecasts2, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, colour = "actual GDP growth"), linetype = "dashed") + 
  geom_line(aes(y = mod1_2, colour = "GDP growth forecast")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-10, 10)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")
umidas2_oneahead_plot

ggsave("umidas2_oneahead_plot.pdf",plot = umidas2_oneahead_plot)


RMSE_umidas2_oneahead <- sqrt(mean((forecasts2$GDP_per[21:247] - forecasts2$mod1_2[21:247])^2))
RMSE_umidas2_oneahead
#0.858512

#BIC calculation
Y2 = matrix2[,2]
z2 = matrix2[,-c(1,2)]
C2 = solve(t(z2)%*%z2)
B2 = t(z2)%*%Y2
A2 = C2%*%B2
res2 = Y2 - (z2%*%A2)

sigma_hat2 <- sum(t(res2)%*%res2)/length(Y2)


BIC2= log(sigma_hat2) + length(A2)/length(Y2)*log(length(Y2))
BIC2
#-0.7756817

#Generation of the one-step-ahead forecast with calculation of the RMSFE for MIDAS 1 v=3
rmsfe_matrix3 = c()
forecast_uMIDASmatrix3= c()
#(dim(matrix3[,-c(1,2)])[1]-dim(matrix3[,-c(1,2)])[2])
for (i in 1:(dim(matrix3[,-c(1,2)])[1]-dim(matrix3[,-c(1,2)])[2])){
  
  z = matrix3[1:((dim(matrix3[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix3[1:((dim(matrix3[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  i
  pred= matrix3[(dim(matrix3[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix3 <-  append(forecast_uMIDASmatrix3,pred)
  rmsfe <- sqrt(mean((matrix3[((dim(matrix3[,-c(1,2)])[2]+1):(dim(matrix3[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix3)^2))
  rmsfe_matrix3 <-  append(rmsfe_matrix3,rmsfe)
}

forecast_uMIDASmatrix3
rmsfe_matrix3

#store the forecast in a data frame
na <- rep(NA,26)
forecast<-append(na, forecast_uMIDASmatrix3)
forecasts3 <- data.frame(forecasts2, forecast)
colnames(forecasts3) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3")

#plot of the forecasts for the U-MIDAS 1
umidas3_oneahead_plot <- ggplot(forecasts3, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, linetype = "actual GDP growth", colour = "actual GDP growth")) + 
  geom_line(aes(y = mod1_1, linetype = "v=1", colour = "v=1")) + 
  geom_line(aes(y = mod1_2, linetype = "v=2", colour = "v=2")) + 
  geom_line(aes(y = mod1_3, linetype = "v=3", colour = "v=3")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(-10, 8)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "dashed", "dashed", "dashed"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green","orange"))
umidas3_oneahead_plot

ggsave("umidas3_oneahead_plot.pdf",plot = umidas3_oneahead_plot)
length(forecasts3$GDP_per)

RMSE_umidas3_oneahead <- sqrt(mean((forecasts3$GDP_per[27:247] - forecasts3$mod1_3[27:247])^2))
RMSE_umidas3_oneahead
#19.66029

#RMSFE without large value for the first forcast
sqrt(mean((forecasts3$GDP_per[28:247] - forecasts3$mod1_3[28:247])^2))


tail(forecasts3)

#store coefficients for later significance analysis
Ysig3 = Y
Asig3 = A
zsig3 = z

#BIC calculation
Y3 = matrix3[,2]
z3 = matrix3[,-c(1,2)]
C3 = solve(t(z3)%*%z3)
B3 = t(z3)%*%Y3
A3 = C3%*%B3

res3 = Y3 - (z3%*%A3)

sigma_hat3 <- sum(t(res3)%*%res3)/length(Y3)

BIC3= log(sigma_hat3) + length(A3)/length(Y3)*log(length(Y3))
BIC3
#-0.8505984

#store the RMSFE in data frame
rmsfe_df1 <- data.frame(data6$dates.GDP.growth[(249-length(forecast_uMIDASmatrix3)):248], rmsfe_matrix1[13:233], rmsfe_matrix2[7:227], rmsfe_matrix3)
colnames(rmsfe_df1) <- c("dates","rmsfe_matrix1","rmsfe_matrix2", "rmsfe_matrix3")

#plot rmsfe values
rmsfe_plot <- ggplot(rmsfe_df1, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = rmsfe_matrix1, colour = "RMSFE Model 1")) + 
  geom_line(aes(y = rmsfe_matrix2, colour = "RMSFE Model 2")) + 
  geom_line(aes(y = rmsfe_matrix3, colour = "RMSFE Model 3")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_plot

ggsave("rmsfe_plot.pdf",plot = rmsfe_plot)

#table for BIC values
BIC <- rbind(BIC1,BIC2,BIC3)
BIC_frame <- data.frame(BIC)
colnames(BIC_frame) <- c("BIC")
rownames(BIC_frame) <- c("v=1","v=2","v=3")
xtable(BIC_frame,digits=4)

#table for RMSFE values
RMSFE <- rbind(RMSE_umidas1_oneahead,RMSE_umidas2_oneahead,RMSE_umidas3_oneahead)
RMSFE_frame <- data.frame(RMSFE)
colnames(RMSFE_frame) <- c("BIC")
rownames(RMSFE_frame) <- c("v=1","v=2","v=3")
xtable(RMSFE_frame,digits=4)

################################################################################
#b)

#Generation of the one-step-ahead forecast with calculation of the RMSFE for MIDAS 2 v=1
rmsfe_matrix4 = c()
forecast_uMIDASmatrix4= c()
for (i in 1:(dim(matrix4[,-c(1,2)])[1]-dim(matrix4[,-c(1,2)])[2])){
  
  z = matrix4[1:((dim(matrix4[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix4[1:((dim(matrix4[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  i
  pred= matrix4[(dim(matrix4[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix4 <-  append(forecast_uMIDASmatrix4,pred)
  rmsfe <- sqrt(mean((matrix4[((dim(matrix4[,-c(1,2)])[2]+1):(dim(matrix4[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix4)^2))
  rmsfe_matrix4 <-  append(rmsfe_matrix4,rmsfe)
}
forecast_uMIDASmatrix4
rmsfe_matrix4

#store the forecast in a data frame
na <- rep(NA,20)
forecast<-append(na, forecast_uMIDASmatrix4)
forecasts4 <- data.frame(forecasts3, forecast)
colnames(forecasts4) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3", "mod2_1")

RMSE_umidas4_oneahead <- sqrt(mean((forecasts4$GDP_per[21:247] - forecasts4$mod2_1[21:247])^2))
RMSE_umidas4_oneahead
#1.790362

#BIC calculation
Y4 = matrix4[,2]
z4 = matrix4[,-c(1,2)]
C4 = solve(t(z4)%*%z4)
B4 = t(z4)%*%Y4
A4 = C4%*%B4

res4 = Y4 - (z4%*%A4)
sigma_hat4 <- sum(t(res4)%*%res4)/length(Y4)

BIC4= log(sigma_hat4) + length(A4)/length(Y4)*log(length(Y4))
BIC4
#0.2381839

#Generation of the one-step-ahead forecast with calculation of the RMSFE for MIDAS 2 v=2
rmsfe_matrix5 = c()
forecast_uMIDASmatrix5= c()
for (i in 1:(dim(matrix5[,-c(1,2)])[1]-dim(matrix5[,-c(1,2)])[2])){
  
  z = matrix5[1:((dim(matrix5[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix5[1:((dim(matrix5[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  i
  pred= matrix5[(dim(matrix5[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix5 <-  append(forecast_uMIDASmatrix5,pred)
  rmsfe <- sqrt(mean((matrix5[((dim(matrix5[,-c(1,2)])[2]+1):(dim(matrix5[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix5)^2))
  rmsfe_matrix5 <-  append(rmsfe_matrix5,rmsfe)
}
forecast_uMIDASmatrix5
rmsfe_matrix5

#store the forecast in a data frame
na <- rep(NA,26)
forecast<-append(na, forecast_uMIDASmatrix5)
forecasts5 <- data.frame(forecasts4, forecast)
colnames(forecasts5) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3", "mod2_1","mod2_2")


RMSE_umidas5_oneahead <- sqrt(mean((forecasts5$GDP_per[27:247] - forecasts5$mod2_2[27:247])^2))
RMSE_umidas5_oneahead
#0.9714103

#BIC calculation
Y5 = matrix5[,2]
z5 = matrix5[,-c(1,2)]
C5 = solve(t(z5)%*%z5)
B5 = t(z5)%*%Y5
A5 = C5%*%B5

res5 = Y5 - (z5%*%A5)

sigma_hat5 <- sum(t(res5)%*%res5)/length(Y5)

BIC5= log(sigma_hat5) + length(A5)/length(Y5)*log(length(Y5))
BIC5
#-0.7328974

#Generation of the one-step-ahead forecast with calculation of the RMSFE for MIDAS 2 v=3
rmsfe_matrix6 = c()
forecast_uMIDASmatrix6= c()
for (i in 1:(dim(matrix6[,-c(1,2)])[1]-dim(matrix6[,-c(1,2)])[2])){
  
  z = matrix6[1:((dim(matrix6[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix6[1:((dim(matrix6[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  i
  pred= matrix6[(dim(matrix6[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix6 <-  append(forecast_uMIDASmatrix6,pred)
  rmsfe <- sqrt(mean((matrix6[((dim(matrix6[,-c(1,2)])[2]+1):(dim(matrix6[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix6)^2))
  rmsfe_matrix6 <-  append(rmsfe_matrix6,rmsfe)
}
forecast_uMIDASmatrix6
rmsfe_matrix6

#store the forecast in a data frame
na <- rep(NA,32)
forecast<-append(na, forecast_uMIDASmatrix6)
forecasts6 <- data.frame(forecasts5, forecast)
colnames(forecasts6) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3", "mod2_1","mod2_2", "mod2_3")

#plot of the forecasts of the U-MIDAS 2
umidas6_oneahead_plot <- ggplot(forecasts6, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, linetype = "actual GDP growth", colour = "actual GDP growth")) + 
  geom_line(aes(y = mod2_1, linetype = "v=1", colour = "v=1")) + 
  geom_line(aes(y = mod2_2, linetype = "v=2", colour = "v=2")) + 
  geom_line(aes(y = mod2_3, linetype = "v=3", colour = "v=3")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(-10, 8)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "dashed", "dashed", "dashed"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green","orange"))
umidas6_oneahead_plot

ggsave("umidas6_oneahead_plot.pdf",plot = umidas6_oneahead_plot)

#calculate the RMSFE
RMSE_umidas6_oneahead <- sqrt(mean((forecasts6$GDP_per[33:247] - forecasts6$mod2_3[33:247])^2))
RMSE_umidas6_oneahead
#0.8529666

Ysig6 = Y
Asig6 = A
zsig6 = z

#BIC calculation
Y6 = matrix6[,2]
z6 = matrix6[,-c(1,2)]
C6 = solve(t(z6)%*%z6)
B6 = t(z6)%*%Y6
A6 = C6%*%B6

res6 = Y6 - (z6%*%A6)
sigma_hat6 <- sum(t(res6)%*%res6)/length(Y6)

BIC6= log(sigma_hat6) + length(A6)/length(Y6)*log(length(Y6))
BIC6
#-0.7895031

length(rmsfe_matrix4)
length(rmsfe_matrix5)
length(rmsfe_matrix6)

#store rmsfe
rmsfe_df1 <- data.frame(data6$dates.GDP.growth[(249-length(forecast_uMIDASmatrix6)):248], rmsfe_matrix4[13:228], rmsfe_matrix5[7:222], rmsfe_matrix6)
colnames(rmsfe_df1) <- c("dates","rmsfe_matrix4","rmsfe_matrix5", "rmsfe_matrix6")

#plot rmfse
rmsfe_plot1 <- ggplot(rmsfe_df1, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = rmsfe_matrix4, colour = "RMSFE Model 1")) + 
  geom_line(aes(y = rmsfe_matrix5, colour = "RMSFE Model 2")) + 
  geom_line(aes(y = rmsfe_matrix6, colour = "RMSFE Model 3")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_plot1

ggsave("rmsfe_plot1.pdf",plot = rmsfe_plot1)
{
  #table for BIC values
  BIC <- rbind(BIC4,BIC5,BIC6)
  BIC_frame1 <- data.frame(BIC)
  colnames(BIC_frame1) <- c("BIC")
  rownames(BIC_frame1) <- c("v=1","v=2","v=3")
  xtable(BIC_frame1,digits=4)
}
{
  #table for RMSFE values
  RMSFE <- rbind(RMSE_umidas4_oneahead,RMSE_umidas5_oneahead,RMSE_umidas6_oneahead)
  RMSFE_frame <- data.frame(RMSFE)
  colnames(RMSFE_frame) <- c("BIC")
  rownames(RMSFE_frame) <- c("v=1","v=2","v=3")
  xtable(RMSFE_frame,digits=4)
}
################################################################################
#c)

#we estimate the variance of the error 
# T-test statistic model 1 BIC K3
ressig3 = Ysig3 - (zsig3%*%Asig3)

csig3= solve(t(zsig3)%*%zsig3)
# We are using 248 observations from 1/06/1959 to 1/09/2021   
sigma_hatsig3= (t(ressig3)%*%ressig3)/(length(Ysig3))
var_beta3 <- sigma_hatsig3[1,1]*csig3

#get test statistic
t_K3 <- c()
for (i in 1:length(Asig3)){
  t_K3 = append(t_K3, (Asig3[i]/sqrt(var_beta3[i,i])))
}

#perform significance test
not_significance_K3 <- (t_K3 < 1.96) & (t_K3>-1.96)
not_significance_K3

#get corresponding columns to sig. variables
columns <- c()
for (i in 2:length(not_significance_K3)) {
  if (not_significance_K3[i] == TRUE){
    columns <- append(columns, (i+2))
  }
}
columns


names(not_significance_K3) <- colnames(data3)[3:28]

#exclude insignificant variables
matrix7 <- matrix3[,-columns]
{
  #table of test statistic values
  tests1 <- data.frame(t(t_K3[2:26]))
  tests1 <- data.matrix(tests1)
  colnames(tests1) <- colnames(matrix3)[4:28]
  dim(tests1)
  tests1 <- rbind(round(tests1[,1:4],2),colnames(tests1)[5:8],
                  round(tests1[1,5:8],2),colnames(tests1)[9:12],
                  round(tests1[1,9:12],2),colnames(tests1)[13:16],
                  round(tests1[1,13:16],2),colnames(tests1)[17:20],
                  round(tests1[1,17:20],2),colnames(tests1)[21:24],
                  round(tests1[1,21:24],2),colnames(tests1)[25],
                  round(tests1[1,25],2))
  
  xtable(tests1, digits=2)
}
head(matrix7)

#perform one-step-ahead forecast with U-MIDAS 1 sig. incl. RMSFE cal.
rmsfe_matrix7 = c()
forecast_uMIDASmatrix7= c()
for (i in 1:(dim(matrix7[,-c(1,2)])[1]-dim(matrix7[,-c(1,2)])[2])){
  
  z = matrix7[1:((dim(matrix7[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix7[1:((dim(matrix7[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  
  pred= matrix7[(dim(matrix7[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix7 <-  append(forecast_uMIDASmatrix7,pred)
  rmsfe <- sqrt(mean((matrix7[((dim(matrix7[,-c(1,2)])[2]+1):(dim(matrix7[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix7)^2))
  rmsfe_matrix7 <-  append(rmsfe_matrix7,rmsfe)
}
forecast_uMIDASmatrix7
rmsfe_matrix7

#store the forecast in a data frame
na <- rep(NA,(dim(matrix7)[2]-2))
forecast<-append(na, forecast_uMIDASmatrix7)
forecasts7 <- data.frame(forecasts6, forecast)
colnames(forecasts7) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3", "mod2_1","mod2_2", "mod2_3","sig_mod1")

RMSE_umidas7_oneahead <- sqrt(mean((forecasts7$GDP_per[(dim(matrix7)[2]-1):247] - forecasts7$sig_mod1[(dim(matrix7)[2]-1):247])^2))
RMSE_umidas7_oneahead
#0.8903242


################################################################################
#d)

#we estimate the variance of the error 
# T-test statistic model 1 BIC K3

ressig6 = Ysig6 - (zsig6%*%Asig6)

csig6= solve(t(zsig6)%*%zsig6)
# We are using 248 observations from 1/06/1959 to 1/09/2021   
sigma_hatsig6= (t(ressig6)%*%ressig6)/(247)
var_beta3 = sigma_hatsig6[1,1]*csig6


t_K3 <- c()


for (i in 1:length(Asig6)){
  t_K3 = append(t_K3, (Asig6[i]/sqrt(var_beta3[i,i])))
}



not_significance_K3 <- (t_K3 < 1.96) & (t_K3>-1.96)
not_significance_K3

columns <- c()
for (i in 2:length(not_significance_K3)) {
  if (not_significance_K3[i] == TRUE){
    columns <- append(columns, (i+2))
  }
}
columns


# For a large sample size it follows a normal distribution
# at the level of significance alpha = 0.05 we have that for a 
# 2 sided test the critical value is (+/-) 1.96
# now we check which values from t_K3 fulfill X < 1.96 
# which means that we do not reject the null hypothesis, which indicates
# that the parameter estimation for the corresponding variable could be 0.
matrix8 <- matrix6[,-columns]

#table of test statistic values
tests2 <- data.frame(t(t_K3[2:32]))
colnames(tests2) <- colnames(matrix6)[4:34]

xtable(tests2)

{
  #table of test statistic values
  tests2 <- data.frame(t(t_K3[2:32]))
  tests2 <- data.matrix(tests2)
  colnames(tests2) <- colnames(matrix6)[4:34]
  dim(tests2)
  tests2 <- rbind(tests2[,1:4],colnames(tests2)[5:8],
                  round(tests2[1,5:8],2),colnames(tests2)[9:12],
                  round(tests2[1,9:12],2),colnames(tests2)[13:16],
                  round(tests2[1,13:16],2),colnames(tests2)[17:20],
                  round(tests2[1,17:20],2),colnames(tests2)[21:24],
                  round(tests2[1,21:24],2),colnames(tests2)[25:28],
                  round(tests2[1,25:28],2),colnames(tests2)[29:31],
                  round(tests2[1,29:31],2))
  
  xtable(tests2, digits=2)
}

rmsfe_matrix8 = c()
forecast_uMIDASmatrix8= c()
for (i in 1:(dim(matrix8[,-c(1,2)])[1]-dim(matrix8[,-c(1,2)])[2])){
  
  z = matrix8[1:((dim(matrix8[,-c(1,2)])[2]-1)+i),-c(1,2)]
  Y = matrix8[1:((dim(matrix8[,-c(1,2)])[2]-1)+i),2]
  
  C = solve(t(z)%*%z)
  B = t(z)%*%Y
  A = C%*%B
  i
  pred= matrix8[(dim(matrix8[,-c(1,2)])[2]+i),-c(1,2)]%*%A
  forecast_uMIDASmatrix8 <-  append(forecast_uMIDASmatrix8,pred)
  rmsfe <- sqrt(mean((matrix8[((dim(matrix8[,-c(1,2)])[2]+1):(dim(matrix8[,-c(1,2)])[2]+i)),2] - forecast_uMIDASmatrix8)^2))
  rmsfe_matrix8 <-  append(rmsfe_matrix8,rmsfe)
}
forecast_uMIDASmatrix8
rmsfe_matrix8

#store the forecast in a data frame
na <- rep(NA,(dim(matrix8)[2]-2))
forecast<-append(na, forecast_uMIDASmatrix8)
forecasts8 <- data.frame(forecasts7, forecast)
colnames(forecasts8) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3", "mod2_1","mod2_2", "mod2_3","sig_mod1","sig_mod2")

umidas8_oneahead_plot <- ggplot(forecasts8, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, linetype = "actual GDP growth", colour = "actual GDP growth")) + 
  geom_line(aes(y = sig_mod1, linetype = "U-MIDAS 1 sig.", colour = "U-MIDAS 1 sig.")) + 
  geom_line(aes(y = sig_mod2, linetype = "U-MIDAS 2 sig.", colour = "U-MIDAS 2 sig.")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(-10, 10)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "dashed", "dashed"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green"))
umidas8_oneahead_plot

ggsave("umidas8_oneahead_plot.pdf",plot = umidas8_oneahead_plot)


RMSE_umidas8_oneahead <- sqrt(mean((forecasts8$GDP_per[(dim(matrix8)[2]-1):247] - forecasts8$sig_mod2[(dim(matrix8)[2]-1):247])^2))
RMSE_umidas8_oneahead
#0.6313064

#plot of the rmsfe of the models based on the significant coefficients
na3 <- rep(NA,(dim(matrix3)[2]-2))
rmsfe3<-append(na3, rmsfe_matrix3)
na6 <- rep(NA,(dim(matrix6)[2]-2))
rmsfe6<-append(na6, rmsfe_matrix6)
na7 <- rep(NA,(dim(matrix7)[2]-2))
rmsfe7<-append(na7, rmsfe_matrix7)
na8 <- rep(NA,(dim(matrix8)[2]-2))
rmsfe8<-append(na8, rmsfe_matrix8)
rmsfe_df2 <- data.frame(data6$dates,rmsfe3,rmsfe6, rmsfe7, rmsfe8)
colnames(rmsfe_df2) <- c("dates","Model1_3","Model2_3","Model1_sig","Model2_sif")

#plot of the rmsfe
rmsfe_plot2 <- ggplot(rmsfe_df2, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = Model1_sig, linetype = "U-MIDAS 1 sig.", colour = "U-MIDAS 1 sig.")) + 
  geom_line(aes(y = Model2_sif, linetype = "U-MIDAS 2 sig.", colour = "U-MIDAS 2 sig.")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "solid"))+
  scale_colour_manual(name="Legend",values=c("red", "blue"))
rmsfe_plot2

ggsave("rmsfe_plot2.pdf",plot = rmsfe_plot2)
################################################################################
#e)
#random forest oqa forecast midas 1
rf_mod1 <- c()
rmsfe_rf_mod1 <- c()
for(i in 1:246) {
  forest<-ranger(GDP_per ~ ., data=data3[(1:i),-c(1,3)],seed = 420)
  tree_forecast <- predict(forest,data3[1+i,-c(1,3)])
  rf_mod1 <- c(rf_mod1, tree_forecast$predictions) 
  rmsfe <- sqrt(mean((data3[2:(1+i),2] - rf_mod1)^2))
  rmsfe_rf_mod1 <-  append(rmsfe_rf_mod1,rmsfe)
  
}
na_rf_mod1 <- rep(NA,1)
rf_mod1 <-  append(na_rf_mod1,rf_mod1)
na_rmsfe_rf_mod1 <- rep(NA,1)
rmsfe_rf_mod1 <-  append(na_rmsfe_rf_mod1,rmsfe_rf_mod1)
length(rmsfe_rf_mod1)

#random forest oqa forecast midas 2
rf_mod2 <- c()
rmsfe_rf_mod2 <- c()
for(i in 1:246) {
  forest<-ranger(GDP_per ~ ., data=data6[(1:i),-c(1,3)],seed = 420)
  tree_forecast <- predict(forest,data6[1+i,-c(1,3)])
  rf_mod2 <- c(rf_mod2, tree_forecast$predictions) 
  rmsfe <- sqrt(mean((data3[2:(1+i),2] - rf_mod2)^2))
  rmsfe_rf_mod2 <-  append(rmsfe_rf_mod2,rmsfe)
  
}
na_rf_mod2 <- rep(NA,1)
rf_mod2 <-  append(na_rf_mod2,rf_mod2)
na_rmsfe_rf_mod2 <- rep(NA,1)
rmsfe_rf_mod2 <-  append(na_rmsfe_rf_mod2,rmsfe_rf_mod2)
length(rmsfe_rf_mod2)

#random forest oqa forecast midas 1
rf_mod1 <- c()
for(i in 1:246) {
  forest<-ranger(GDP_per ~ ., data=data3[(1:i),-c(1,3)],seed = 420)
  tree_forecast <- predict(forest,data3[1+i,-c(1,3)])
  rf_mod1 <- c(rf_mod1, tree_forecast$predictions)    
}
na_rf_mod1 <- rep(NA,1)
rf_mod1 <-  append(na_rf_mod1,rf_mod1)
length(rf_mod1)

#random forest oqa forecast hyperparameter tuned
opt_par_test <- array(0, dim=c(5,5,5),
                      dimnames = list(c("300 trees","400 trees", "500 trees", "600 trees", "700 trees"),
                                      c("3 Variablen pro Splitt", "4 Variablen pro Splitt","5 Variablen pro Splitt","6 Variablen pro Splitt","7 Variablen pro Splitt"),
                                      c("Min_node_size_3","Min_node_size_4", "Min_node_size_5", "Min_node_size_6", "Min_node_size_7")))
opt_par_test
(nrow(data3)-2)
forecasting_RF_oqa <- data.frame()
mod_paramDS2  <- data.frame()
vec_Forecasting_error_RF= c()

for(m in 1:(nrow(data3)-2)){
  for(j in 1:5) {
    for(k in 1:5) {
      for (l in 1:5){
        temp.model <- ranger(GDP_per ~ ., data=data3[(1:m),-c(1,3)], 
                             mtry=(k+2), min.node.size = (l+2), 
                             num.trees=(j)*250,
                             seed = 826)
        pred <- predict(temp.model,data3[(m+1),])
        pred_val <- pred$predictions
        opt_par_test[j,k,l] <- opt_par_test[j,k,l] + (data3$GDP_per[(m+1)]-pred_val)^2
        
      }
    }
  }
  
  mod_param <- which(opt_par_test == min(opt_par_test), arr.ind = TRUE)
  mod_paramDS2 = rbind(mod_paramDS2, mod_param)
  RF_oqa <- ranger(GDP_per ~ .,data=data3[(1:m),-c(1,3)], 
                   mtry=(mod_param[1,2]+2), min.node.size = (mod_param[1,3]+1), 
                   num.trees= ((mod_param[1,1]+1)*250),
                   # max.depth = mod_param[1,1],
                   seed = 826)
  forecasting_RF_oqa = rbind(forecasting_RF_oqa, predict(RF_oqa,data3[(m+1),]))
  
  vec_GDPgrowth_RF <- data3$GDP_per[3:(m+1)]
  vec_Forecasting_error_RF <- append(vec_Forecasting_error_RF,(sqrt((1/length(vec_GDPgrowth_RF))*sum((vec_GDPgrowth_RF-forecasting_RF_oqa$predictions)^2))))
  
  print(mod_paramDS2)
  print(opt_par_test[1,1,1])
  print(m)
}
forecasting_RF_oqa
mod_paramDS2
vec_Forecasting_error_RF

write.csv(matrix(forecasting_RF_oqa, nrow=1), file ="rf_tuning_mod1_forecast.csv", row.names=FALSE)

################################################################################
#f)

opt_par_test2 <- array(0, dim=c(5,5,5),
                      dimnames = list(c("300 trees","400 trees", "500 trees", "600 trees", "700 trees"),
                                      c("3 Variablen pro Splitt", "4 Variablen pro Splitt","5 Variablen pro Splitt","6 Variablen pro Splitt","7 Variablen pro Splitt"),
                                      c("Min_node_size_3","Min_node_size_4", "Min_node_size_5", "Min_node_size_6", "Min_node_size_7")))
opt_par_test2

forecasting_RF_oqa2 <- data.frame()
mod_paramDS2  <- data.frame()
vec_Forecasting_error_RF2= c()
(nrow(data6)-2)
for(m in 1:(nrow(data6)-2)){
  for(j in 1:5) {
    for(k in 1:5) {
      for (l in 1:5){
        temp.model <- ranger(GDP_per ~ ., data=data6[(1:m),-c(1,3)], 
                             mtry=(k+2), min.node.size = (l+2), 
                             num.trees=(j+2)*100,
                             seed = 826)
        pred <- predict(temp.model,data6[(m+1),])
        pred_val <- pred$predictions
        opt_par_test2[j,k,l] <- opt_par_test2[j,k,l] + (data6$GDP_per[(m+1)]-pred_val)^2
        
      }
    }
  }
  
  mod_param <- which(opt_par_test2 == min(opt_par_test2), arr.ind = TRUE)
  mod_paramDS2 = rbind(mod_paramDS2, mod_param[1,])
  RF_oqa <- ranger(GDP_per ~ .,data=data6[(1:m),-c(1,3)], 
                   mtry=(mod_param[1,2]+2), min.node.size = (mod_param[1,3]+1), 
                   num.trees= ((mod_param[1,1]+2)*100),
                   # max.depth = mod_param[1,1],
                   seed = 826)
  forecasting_RF_oqa2 = rbind(forecasting_RF_oqa2, predict(RF_oqa,data6[(m+1),]))
  
  vec_GDPgrowth_RF <- data6$GDP_per[3:(m+1)]
  vec_Forecasting_error_RF2 <- append(vec_Forecasting_error_RF2,(sqrt((1/length(vec_GDPgrowth_RF))*sum((vec_GDPgrowth_RF-forecasting_RF_oqa2$predictions)^2))))
  
  print(mod_paramDS2)
  print(opt_par_test2[1,1,1])
  print(m)
}
warnings()
forecasting_RF_oqa2
mod_paramDS2
vec_Forecasting_error_RF2

write.csv(matrix(forecasting_RF_oqa2, nrow=1), file ="rf_tuning_mod2_forecast.csv", row.names=FALSE)


#plot hyperparameters
mod_paramDS2_plot <- data.frame(data6$dates.GDP.growth[3:247],mod_paramDS2[,2:3]+2,mod_paramDS2[,1]*100+200)
colnames(mod_paramDS2_plot) <- c("dates","m", "n_min", "B")

scale = 100

hyperparameters_plot <- ggplot(mod_paramDS2_plot, aes(x = dates, y = m)) +
  geom_line(aes(color = "number of considered Variables")) +
  geom_line(aes(y = (B/scale), color = "Number of Trees")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="number of considered Variables")) +
  labs(x = "Time", y = "Number of Trees", color = "") +
  scale_color_manual(values = c("orange2", "gray30")) +
  theme(legend.position="bottom")
hyperparameters_plot

ggsave("hyperparameters_plot.pdf",plot = hyperparameters_plot)

rf_na <- rep(NA,2)
rf_tuning_forecast1<-append(rf_na, forecasting_RF_oqa$predictions)
length(rf_tuning_forecast1)

rf_tuning_forecast2<-append(rf_na, forecasting_RF_oqa2$predictions)
length(rf_tuning_forecast2)

#store the forecast in a data frame
forecasts_rf <- data.frame(data6$dates.GDP.growth[1:247],data6$GDP_per[1:247], rf_tuning_forecast1, rf_tuning_forecast2, rf_mod1, rf_mod2)
colnames(forecasts_rf) <- c("dates","GDP_per", "rf_tuning_forecast1", "rf_tuning_forecast2", "rf_forecast1", "rf_forecast2")

rf_plot <- ggplot(forecasts_rf, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, linetype = "actual GDP growth", colour = "actual GDP growth")) + 
  geom_line(aes(y = rf_forecast2, linetype = "RF U-MIDAS 2", colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = rf_tuning_forecast2, linetype = "RF U-MIDAS 2 tuned", colour = "RF U-MIDAS 2 tuned")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "dashed", "dashed"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green"))
rf_plot

ggsave("rf_plot.pdf",plot = rf_plot)

rf_rmsfe_na <- rep(NA,2)

rf_rmsfe_tuning_forecast1<-append(rf_rmsfe_na, vec_Forecasting_error_RF)
length(rf_rmsfe_tuning_forecast1)

rf_rmfse_tuning_forecast2<-append(rf_rmsfe_na, vec_Forecasting_error_RF2)
length(rf_rmfse_tuning_forecast2)

rmsfe_df3 <- data.frame(rmsfe_df2,rf_rmsfe_tuning_forecast1, rf_rmfse_tuning_forecast2, rmsfe_rf_mod1, rmsfe_rf_mod2)
colnames(rmsfe_df3) <- c("dates","Model1_3","Model2_3",
                         "Model1_sig","Model2_sif","RF1_tuned", 
                         "RF2_tuned","RF1", "RF2")

rf_rmsfe_plot <- ggplot(rmsfe_df3, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = RF1, linetype = "RF U-MIDAS 1", colour = "RF U-MIDAS 1")) + 
  geom_line(aes(y = RF2, linetype = "RF U-MIDAS 2", colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = RF1_tuned, linetype = "RF U-MIDAS 1 tuned", colour = "RF U-MIDAS 1 tuned")) + 
  geom_line(aes(y = RF2_tuned, linetype = "RF U-MIDAS 2 tuned", colour = "RF U-MIDAS 2 tuned")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "solid", "solid", "solid"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green","orange"))
rf_rmsfe_plot

ggsave("rf_rmsfe_plot.pdf",plot = rf_rmsfe_plot)



rmsfe_rf_frame1 <- data.frame(rmsfe_df3[245, c(6:9)])
colnames(rmsfe_rf_frame1) <- c("RF1_tuned", "RF2_tuned","RF1", "RF2")
rownames(rmsfe_rf_frame1) <- c("RMSFE")
xtable(rmsfe_rf_frame1,digits = 4)
################################################################################
#g)

forecasts8 <- data.frame(forecasts8, forecasts_rf[,3:6])
colnames(forecasts8) <- c("dates","GDP_per", "mod1_1", "mod1_2", "mod1_3", "mod2_1","mod2_2", "mod2_3","sig_mod1","sig_mod2", "rf_tuning_forecast1", "rf_tuning_forecast2", "rf_forecast1", "rf_forecast2")

rmsfe_rf2_ma <- c()
rmsfe_rf2_ma <- rep(NA,(2+10))
for (i in 1:(245-10)) {
  RMSE_rf2_10 <- sqrt(mean((forecasts_rf$GDP_per[(2+i):(2+9+i)] - forecasts_rf$rf_forecast2[(2+i):(2+9+i)])^2))
  rmsfe_rf2_ma <-  append(rmsfe_rf2_ma,RMSE_rf2_10)
}
length(rmsfe_rf2_ma)

rmsfe_rf2_tuned_ma <- c()
rmsfe_rf2_tuned_ma <- rep(NA,(2+10))
for (i in 1:(245-10)) {
  RMSE_rf2_tuned_10 <- sqrt(mean((forecasts_rf$GDP_per[(2+i):(2+9+i)] - forecasts_rf$rf_tuning_forecast2[(2+i):(2+9+i)])^2))
  rmsfe_rf2_tuned_ma <-  append(rmsfe_rf2_tuned_ma,RMSE_rf2_tuned_10)
}
length(rmsfe_rf2_tuned_ma)

rmsfe_matrix8_ma <- c()
rmsfe_matrix8_ma <- rep(NA,(dim(matrix8)[2]-2+10))
for (i in 1:(dim(matrix8)[1]-dim(matrix8)[2]-8)) {
  RMSE_umidas8_10 <- sqrt(mean((forecasts8$GDP_per[(dim(matrix8)[2]-2+i):(dim(matrix8)[2]-2+9+i)] - forecasts8$sig_mod2[(dim(matrix8)[2]-2+i):(dim(matrix8)[2]-2+9+i)])^2))
  rmsfe_matrix8_ma <-  append(rmsfe_matrix8_ma,RMSE_umidas8_10)
}
rmsfe_matrix8_ma

rmsfe_matrix7_ma <- c()
rmsfe_matrix7_ma <- rep(NA,(dim(matrix7)[2]-2+10))
for (i in 1:(dim(matrix7)[1]-dim(matrix7)[2]-8)) {
  RMSE_umidas7_10 <- sqrt(mean((forecasts7$GDP_per[(dim(matrix7)[2]-2+i):(dim(matrix7)[2]-2+9+i)] - forecasts8$sig_mod1[(dim(matrix7)[2]-2+i):(dim(matrix7)[2]-2+9+i)])^2))
  rmsfe_matrix7_ma <-  append(rmsfe_matrix7_ma,RMSE_umidas7_10)
}
rmsfe_matrix7_ma

rmsfe_matrix6_ma <- c()
rmsfe_matrix6_ma <- rep(NA,(dim(matrix6)[2]-2+10))
for (i in 1:(dim(matrix6)[1]-dim(matrix6)[2]-8)) {
  RMSE_umidas6_10 <- sqrt(mean((forecasts6$GDP_per[(dim(matrix6)[2]-2+i):(dim(matrix6)[2]-2+9+i)] - forecasts8$mod2_3[(dim(matrix6)[2]-2+i):(dim(matrix6)[2]-2+9+i)])^2))
  rmsfe_matrix6_ma <-  append(rmsfe_matrix6_ma,RMSE_umidas6_10)
}
rmsfe_matrix6_ma

rmsfe_matrix3_ma <- c()
rmsfe_matrix3_ma <- rep(NA,(dim(matrix3)[2]-2+10))
for (i in 1:(dim(matrix3)[1]-dim(matrix3)[2]-8)) {
  RMSE_umidas3_10 <- sqrt(mean((forecasts3$GDP_per[(dim(matrix3)[2]-2+i):(dim(matrix3)[2]-2+9+i)] - forecasts8$mod1_3[(dim(matrix3)[2]-2+i):(dim(matrix3)[2]-2+9+i)])^2))
  rmsfe_matrix3_ma <-  append(rmsfe_matrix3_ma,RMSE_umidas3_10)
}
rmsfe_matrix3_ma

rmsfe_ma <- data.frame(data6$dates, rmsfe_matrix3_ma, rmsfe_matrix6_ma, rmsfe_matrix7_ma, rmsfe_matrix8_ma, rmsfe_rf2_ma, rmsfe_rf2_tuned_ma)
colnames(rmsfe_ma) <- c("dates", "mod1_3", "mod2_3", "sig_mod1_3", "sig_mod2_3", "rf_forecast2","rf_tuning_forecast2")

rmsfe_ma_plot <- ggplot(rmsfe_ma, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = mod1_3, colour = "U-MIDAS 1 v=3")) + 
  geom_line(aes(y = mod2_3, colour = "U-MIDAS 2 v=3")) + 
  geom_line(aes(y = sig_mod1_3, colour = "U-MIDAS 1 sign.")) + 
  geom_line(aes(y = sig_mod2_3, colour = "U-MIDAS 2 sign.")) + 
  geom_line(aes(y = rf_forecast2, colour = "RF 2")) + 
  geom_line(aes(y = rf_tuning_forecast2, colour = "RF 2 tuned")) + 
  coord_cartesian(ylim=c(0, 5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_ma_plot

ggsave("rmsfe_ma_plot.pdf",plot = rmsfe_ma_plot)

rmsfe_whole <- c()
for (i in 3:(dim(forecasts8)[2])) {
  rmsfe <- sqrt(mean((forecasts3$GDP_per[33:247] - forecasts8[33:247,i])^2))
  rmsfe_whole <- append(rmsfe_whole, rmsfe)
}

{
  #calculate comparable RMSFEs report 3

  rmsfe_whole <- c()
  for (i in c(5,8,9,10,12,14)) {
    rmsfe <- sqrt(mean((forecasts8$GDP_per[33:247] - forecasts8[33:247,i])^2))
    rmsfe_whole <- append(rmsfe_whole, rmsfe)
  }  
    rmsfe_whole_frame1 <- data.frame(t(rmsfe_whole))
    colnames(rmsfe_whole_frame1) <- c("mod1_3","mod2_3",
                                      "sig_mod1","sig_mod2",
                                      "rf_tuning_forecast2","rf_forecast2")
    rownames(rmsfe_whole_frame1) <- c("RMSFE")
    
    xtable(rmsfe_whole_frame1, digits = 4)
  
}
head(rmsfe_df3)

################################################################################
#h)

df_ar <- read.csv("data_forecast.csv")
RMSE_AR1 <- sqrt(mean((df_ar$GDP_per[1:248] - df_ar$forecast_ar[1:248])^2))
RMSE_AR1
#1.196388

ar <- df_ar$forecast_ar[2:248]
length(ar)

df_var1 <- read.csv("data_var_forecast.csv")
RMSE_VAR1 <- sqrt(mean((df_var1$GDP_per[1:242] - df_var1$var_forecast[1:242])^2))
RMSE_VAR1
#1.974371

na_var1 <- rep(NA,5)
var1 <- append(na_var1, df_var1$var_forecast)

rmsfe_var1_ma <- rep(NA,5)
for (i in 1:(242)) {
  RMSE_var1_s <- sqrt(mean((forecasts_rf$GDP_per[(6):(5+i)] - var1[(6):(5+i)])^2))
  rmsfe_rf2_tuned_ma <-  append(rmsfe_rf2_tuned_ma,RMSE_var1_s)
}
length(rmsfe_rf2_tuned_ma)

df_varp <- read.csv("data_varp_forecast.csv")
RMSE_VAR1 <- sqrt(mean((df_varp$GDP_per[1:226] - df_varp$varp_forecast[1:226])^2))
RMSE_VAR1
#1.974371

na_var3 <- rep(NA,(247-226))
var3 <- append(na_var3, df_varp$varp_forecast)
length(var3)

rmsfe_var3_ma <- rep(NA,(247-226))
for (i in 1:(247-(247-226))) {
  RMSE_var1_s <- sqrt(mean((forecasts_rf$GDP_per[(6):(5+i)] - var1[(6):(5+i)])^2))
  rmsfe_rf2_tuned_ma <-  append(rmsfe_rf2_tuned_ma,RMSE_var1_s)
}
length(rmsfe_rf2_tuned_ma)

df_rf <- read.csv("rf_forecasts.csv")
RMSE_rf <- sqrt(mean((df_rf$GDP_per[1:240] - df_rf$forecast_rf_ranger[1:240])^2))
RMSE_rf
#1.214389

na_rf <- rep(NA,(247-240))
rf <- append(na_rf, df_rf$forecast_rf_ranger)
length(rf)

na_rt_big <- rep(NA,(247-240))
rt_big <- append(na_rt_big, df_rf$forecast_rt_big_1ahead)
length(rt_big)

forecasts9 <- data.frame(forecasts8, ar, var1, var3,rf,rt_big)
colnames(forecasts9) <- c("dates","GDP_per", "mod1_1", "mod1_2", 
                          "mod1_3", "mod2_1","mod2_2", "mod2_3",
                          "sig_mod1","sig_mod2", "rf_tuning_forecast1", 
                          "rf_tuning_forecast2", "rf_forecast1", 
                          "rf_forecast2", "ar", "var1", "var3",
                          "rf","rt_big")

{
  #calculate comparable RMSFEs for all models
  
  rmsfe_am_whole <- c()
  for (i in 3:dim(forecasts9)[2]) {
    rmsfe <- sqrt(mean((forecasts9$GDP_per[33:247] - forecasts9[33:247,i])^2))
    rmsfe_am_whole <- append(rmsfe_am_whole, rmsfe)
  }  
  rmsfe_whole_frame1 <- data.frame(t(rmsfe_am_whole))
  colnames(rmsfe_whole_frame1) <- c("mod1_1", "mod1_2", 
                                    "mod1_3", "mod2_1","mod2_2", "mod2_3",
                                    "sig_mod1","sig_mod2", "rf_tuning_forecast1", 
                                    "rf_tuning_forecast2", "rf_forecast1", 
                                    "rf_forecast2", "ar", "var1", "var3",
                                    "rf","rt_big")
  rownames(rmsfe_whole_frame1) <- c("RMSFE")
  
  xtable(rmsfe_whole_frame1, digits = 4)
  
}

ml_plot <- ggplot(forecasts9, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, linetype = "actual GDP growth", colour = "actual GDP growth")) + 
  geom_line(aes(y = rf_forecast2, linetype = "RF U-MIDAS 2", colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = rf, linetype = "RF 2. report", colour = "RF 2. report")) + 
  geom_line(aes(y = rt_big, linetype = "RT 2. report", colour = "RT 2. report")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "dashed", "dashed", "dashed"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green","orange"))
ml_plot

ggsave("ml_plot.pdf",plot = ml_plot)



ts_plot <- ggplot(forecasts9, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, linetype = "actual GDP growth", colour = "actual GDP growth")) + 
  geom_line(aes(y = ar, linetype = "AR(1)", colour = "AR(1)")) + 
  geom_line(aes(y = var1, linetype = "VAR(1)", colour = "VAR(1)")) + 
  geom_line(aes(y = sig_mod2, linetype = "U-MIDAS 2 sig.", colour = "U-MIDAS 2 sig.")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(-10, 10)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")+
  scale_linetype_manual(name="Legend",values=c("solid", "dashed", "dashed", "dashed"))+
  scale_colour_manual(name="Legend",values=c("red", "blue","green","orange"))
ts_plot

ggsave("ts_plot.pdf",plot = ts_plot)

{
  #plots RMSFEs for all models
  
  rmsfe_whole_plot_df <- matrix(NA, nrow = (247-32), ncol = dim(forecasts9)[2])
  for (i in 3:dim(forecasts9)[2]) {
    for (j in 1:(247-32)) {
      rmsfe <- sqrt(mean((forecasts9$GDP_per[33:(32+j)] - forecasts9[33:(32+j),i])^2))
      rmsfe_whole_plot_df[j,i] <- rmsfe
    }
  }  
  rmsfe_whole_frame1 <- data.frame(forecasts9[33:247,1],rmsfe_whole_plot_df[,3:dim(forecasts9)[2]])
  colnames(rmsfe_whole_frame1) <- c("dates","mod1_1", "mod1_2", 
                                    "mod1_3", "mod2_1","mod2_2", "mod2_3",
                                    "sig_mod1","sig_mod2", "rf_tuning_forecast1", 
                                    "rf_tuning_forecast2", "rf_forecast1", 
                                    "rf_forecast2", "ar", "var1", "var3",
                                    "rf","rt_big")

}

{
  #plots RMSFEs for all models
  
  rmsfe_am_whole_plot_df <- matrix(NA, nrow = (247-32-9), ncol = dim(forecasts9)[2])
  for (i in 3:dim(forecasts9)[2]) {
    for (j in 1:(247-32-9)) {
      rmsfe <- sqrt(mean((forecasts9$GDP_per[(32+j):(41+j)] - forecasts9[(32+j):(41+j),i])^2))
      rmsfe_am_whole_plot_df[j,i] <- rmsfe
    }
  }  
  rmsfe_whole_am_frame1 <- data.frame(forecasts9[42:247,1],rmsfe_am_whole_plot_df[,3:dim(forecasts9)[2]])
  colnames(rmsfe_whole_am_frame1) <- c("dates","mod1_1", "mod1_2", 
                                    "mod1_3", "mod2_1","mod2_2", "mod2_3",
                                    "sig_mod1","sig_mod2", "rf_tuning_forecast1", 
                                    "rf_tuning_forecast2", "rf_forecast1", 
                                    "rf_forecast2", "ar", "var1", "var3",
                                    "rf","rt_big")
  
}

rmsfe_total_p3_plot <- ggplot(rmsfe_whole_frame1, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = mod1_3, colour = "U-MIDAS 1")) + 
  geom_line(aes(y = mod2_3, colour = "U-MIDAS 2")) + 
  geom_line(aes(y = sig_mod1, colour = "U-MIDAS 1 sig.")) + 
  geom_line(aes(y = sig_mod2, colour = "U-MIDAS 2 sig.")) + 
  geom_line(aes(y = rf_forecast2, colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = rf_tuning_forecast2, colour = "RF U-MIDAS 2 tuned")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 3)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_total_p3_plot
ggsave("rmsfe_total_p3_plot.pdf",plot = rmsfe_total_p3_plot)

rmsfe_ma_p3_plot <- ggplot(rmsfe_whole_am_frame1, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = mod1_3, colour = "U-MIDAS 1")) + 
  geom_line(aes(y = mod2_3, colour = "U-MIDAS 2")) + 
  geom_line(aes(y = sig_mod1, colour = "U-MIDAS 1 sig.")) + 
  geom_line(aes(y = sig_mod2, colour = "U-MIDAS 2 sig.")) + 
  geom_line(aes(y = rf_forecast2, colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = rf_tuning_forecast2, colour = "RF U-MIDAS 2 tuned")) + 
  coord_cartesian(ylim=c(0, 5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_ma_p3_plot
ggsave("rmsfe_ma_p3_plot.pdf",plot = rmsfe_ma_p3_plot)

rmsfe_total_plot <- ggplot(rmsfe_whole_frame1, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = ar, colour = "AR(1)")) + 
  geom_line(aes(y = var1, colour = "VAR(1)")) + 
  geom_line(aes(y = rt_big, colour = "RT project 2")) + 
  geom_line(aes(y = sig_mod2, colour = "U-MIDAS 2 sig.")) + 
  geom_line(aes(y = rf_forecast2, colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = rf, colour = "RF project 2")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_total_plot
ggsave("rmsfe_total_plot.pdf",plot = rmsfe_total_plot)

rmsfe_ma_total_plot <- ggplot(rmsfe_whole_am_frame1, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = ar, colour = "AR(1)")) + 
  geom_line(aes(y = var1, colour = "VAR(1)")) + 
  geom_line(aes(y = rt_big, colour = "RT project 2")) + 
  geom_line(aes(y = sig_mod2, colour = "U-MIDAS 2 sig.")) + 
  geom_line(aes(y = rf_forecast2, colour = "RF U-MIDAS 2")) + 
  geom_line(aes(y = rf, colour = "RF project 2")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 3)) +
  xlab("Time") + ylab("RMSFE") +
  theme(legend.position="bottom")
rmsfe_ma_total_plot
ggsave("rmsfe_ma_total_plot.pdf",plot = rmsfe_ma_total_plot)

