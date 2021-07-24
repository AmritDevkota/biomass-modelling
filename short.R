install.packages('readxl')
library(readxl)
biodata = read.csv("biomass_data1.csv")

myVar <- c("biomass_g", "Diam_cm", "height_m")
for (i in 1: length(myVar)){
  variable <- myVar[[i]]
  minimum <- min(biodata[, myVar[[i]]], na.rm = T)
  maximum <- max(biodata[, myVar[[i]]], na.rm = T)
  mean_ <- mean(biodata[, myVar[[i]]], na.rm = T)
  median_ <- median(biodata[, myVar[[i]]], na.rm = T)
  stdDev <- sd(biodata[, myVar[[i]]], na.rm = T)
  Q1 <- quantile(biodata[, myVar[[i]]], 0.25, na.rm = T)
  Q3 <- quantile(biodata[, myVar[[i]]], 0.75, na.rm = T)
  IQR <- Q3 - Q1
  CV <- stdDev/mean_
  statistics <- cbind(
    variable, minimum, maximum, mean_, 
    median_, stdDev, Q1, Q3, IQR, CV
  )
  if (i == 1){
    write.table(
      statistics, "descriptive_statistics.csv",
      sep = ",",  col.names = T, row.names = F, append = F
    )
  } else {
    write.table(
      statistics, "descriptive_statistics.csv",
      sep = ",",  col.names = F, row.names = F, append = T
    )
  }
}

## Histogram:
# install.packages('ggplot2')
library(ggplot2)    # load package need to install first (install.package(''))
for(i in myVar){
  myPlot <- ggplot(biodata, aes_string(i)) + geom_histogram()
  print(myPlot)
  filename <- paste("Histogram_",i,sep="")
  dev.copy(tiff,filename, width = 7.1, height = 5.5, units = "in", bg = "white", res = 100)
  dev.off()
}

# Density plot
for (i in myVar) {
  plot(density(biodata[,i]), main = i)
  filename <- paste("Density_plot_",i,sep="")
  dev.copy(tiff,filename, width = 7.1, height = 5.5, units = "in", bg = "white", res = 100)
  dev.off()
}

# Boxplot
for (i in myVar) {
  boxplot(biodata[, i], main = i)
  filename <- paste("Box_plot_",i,sep="")
  dev.copy(tiff,filename, width = 7.1, height = 5.5, units = "in", bg = "white", res = 100)
  dev.off()
}

# Q-Q plot
for(i in myVar){
  qqnorm(biodata[,i], main=i); 
  qqline(biodata[,i],col = 2)
  filename <- paste("Q_Q_plot_",i,sep="")
  dev.copy(tiff,filename, width = 7.1, height = 5.5, units = "in", bg = "white", res = 100)
  dev.off()
}

# Shapiro_Wilk test for normality
for(i in myVar) {
  print(shapiro.test(biodata[, i]))
#  filename <- paste("Shapiro_Wilk_normality2_",i,".txt",sep="")
#  sink(filename)
#  sink()
}

#savehistory(file= "Shapiro_Wilk_normality1.txt")
sink("Shapiro_Wilk_normality.txt")
sink()

# Relationship between dep & indep variable require 'ggplot2' package
library(ggplot2) # launch package if not already also install before that if not
for ( i in 2: length(myVar)){         # Starts from 2 end at 3
  myPlot <- ggplot(biodata, aes_string(myVar[i], myVar[1])) + geom_point()
  print(myPlot)
}

# No data partition since we have small data
trainData = biodata
myVar
head(trainData)

B <- myVar[1]
D <- myVar[2]
H <- myVar[3]

# Equation and variable matching
colnames(trainData)[colnames(trainData)==myVar[1]] <- 'B'
colnames(trainData)[colnames(trainData)==myVar[2]] <- 'D'
colnames(trainData)[colnames(trainData)==myVar[3]] <- 'H'
head(trainData)
trainData$D
install.packages(minpack.lm)
library(minpack.lm)
model <- list(
       lm(B ~ D, data = trainData),
       lm(B ~ D + H, data = trainData),
       lm(B ~ D + D^2 + H, data = trainData),
       lm(B ~ D*H, data = trainData),
       lm(B ~ D^2*H, data = trainData),
       lm(B ~ D + D^2 + H^2, data = trainData),
       lm(B ~ D*H + D^2*H^2, data = trainData),
       nls(B ~ a*D * H^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236)),
       nls(B ~ exp(a*(D^2*H)^b), data = trainData, start = list(a=4.1580, b= 0.1380)),
       nls(B ~ a + b*log(D) + c*log(H), data = trainData, start = list(a=-2.4554, b= 1.9026, c= 0.8352)),
       nls(B ~ a + b*log(D^2*H), data = trainData, start = list(a=-23, b= 0.949)),
       nls(B ~ a*D^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236)),
       nls(B ~ a*(exp(b*D)), data = trainData, start=list(a=exp(0.1235), b=exp(-4.3729)))
)

summary(model[[8]])

## Model Evaluation
# Statistical evalutation
# Summarize models 
filename <- paste("model_summary.txt",sep="") # give file name before output function
sink(filename)
  for (i in 1: length(model)) {
    print(summary(model[[i]]))
  }

# Save model summary in .txt file #can also be saved as csv file
sink() # Close the sink

library(broom) # install if not already install

# Save coefficient
# filename <- paste("model_coefficient.csv",sep="") # give file name before output function
sink("model_coefficient.txt")
for (i in 1: length(model)){
  i = toString(i)
  name = paste("model",i,"_coefficient.csv", sep="")
  print(tidy(model[[i]]))
}
sink()

library(ggplot2)
# Save other statistics             
sink("model_statistics.txt") 
for (i in 1: length(model)){
  i = toString(i)
#  name = paste("model",i,"_statistis.csv", sep="")
#  write.csv(tidy(model[[i]])), name)
  print(glance(model[[i]]))         # glance
}
sink()


# Coefficient of determination
sink("model_coeff_of_determination.txt")
for (i in 1: length(model)){
  cor.test(trainData$D,trainData$H)
}
sink()

# Calculate AIC & AICC values (model with lowest AIC value is best)
#install.packages('AICcmodavg')
#install.packages('plyr')
#install.packages('stringr')
library(AICcmodavg)
library(plyr)
library(stringr)

AIC_values <- ldply(model, function(mod){
  data.frame(AICc = AICc(mod), model = deparse(formula(mod)))
})
write.csv(AIC_values, "AIC_values.csv")


-----------------------
# Graphical evaluation
# Residual Analysis: Using residual plots
# Short list the most closest candidate model that fit the most. Not more than 7!

# Observing 
  
for(i in 1:7){
  par(mfrow = c(2,2)) # plot 4 plots in 1 panel
  plot(model[[i]], main = paste("model",i,sep=""))
}
for(i in 5:13){
  par(mfrow = c(1,1)) # plot 1 plots in 1 panel
  plot(model[[i]], main = paste("model",i,sep=""))
  print(myPlot)
}

# Q-Q plots for remaining 3 models
for(i in 1:13){
  par(mfrow=c(1,1)) # plot 4 plot in 1 panel
  qqplot <- qqnorm(resid(model[[i]]), main = paste("model",i,sep="")); qqline(resid(model[[i]]), col = 2)
  jpeg(paste("QQPlot_model",i,".jpg", sep=""))
  qqplot
  dev.off()
}
for(i in 5:7){
  par(mfrow=c(1,1)) # plot 1 plot in 1 panel
  qqnorm(resid(model[[i]]), main = paste("model",i,sep="")); qqline(resid(model[[i]]), col = 2)
}

# Using Histograms
library(ggplot2)
for(i in 1:length(model)) {
  par(mfrow=c(2,2)) # Plot 2 plot in 2 panel.
  jpeg(paste("Histogram_model",i,".jpg", sep=""))
  myPlot <- ggplot(mapping = aes(resid(model[i]))) +
    geom_histogram() +
    labs(title = paste("model",i,sep="")
    )
  print(myPlot)
  dev.off()
}

# Density Plots
for(i in 1:length(model)){
  plot(density(resid(model[[i]])), main = paste("model",i,sep=""))
}

#Using predicted vs. observed values plots
for(i in 1:length(model)){
  plot(predict(model[[i]]), trainData$B, main = paste("model",i,sep=""), xlab="Predicted values", ylab="Observed vaues")
  abline(a=0,b=1)
  abline(lm(predict(model[[i]])~trainData$B), col="blue")
}

# Using model curves and data used for model fitting
library(ggplot2)            # If the packages don't support
remove.package("ggplot2")   # Remove package
install.package("ggplot2")  # Install Again
library(ggplot2)            # Launch the package

#Compute combined variables
head(trainData)     # Head of data

trainData$DH <- trainData$D * trainData$H
trainData$D2H <- trainData$D^2 * trainData$H
trainData$rDH <- trainData$rho_g_cm3 * trainData$x * trainData$height_m
trainData$rD2H <- trainData$rho_g_cm3 * trainData$x^2 * trainData$height_m

#Define variables
y <- trainData$B
#x <- c("x", "DH", "D2H", "rDH", "rD2H")
#x <- trainData$D
x <- c(1: 120)

ggplot(trainData, aes(x, y)) +
  geom_point(size = 2) +
  geom_line(aes(y=predict(model[[1]])), size = 1, colour = "red") +
  geom_line(aes(y=predict(model[[2]])), size = 1, colour = "green") +
  geom_line(aes(y=predict(model[[3]])), size = 1, colour = "blue") +
  geom_line(aes(y=predict(model[[4]])), size = 1, colour = "cyan") +
  geom_line(aes(y=predict(model[[5]])), size = 1, colour = "purple")

for(i in 8:length(model)){
  plot(model[[i]])
}



colour <- c("red", "green", "blue", "cyan", "brown", "yellow", "orange","red4", "green4", "blue4", "cyan4", "brown4", "yellow4")
dev.new(width=5, height=4) # plot size
ggplot(trainData, aes(x, y)) + geom_point(size = 2) +
  geom_line(aes(y = predict(model[[1]])), size = 1, colour = "red") +
  geom_line(aes(y = predict(model[[2]])), size = 1, colour = "green") +
  geom_line(aes(y = predict(model[[3]])), size = 1, colour = "blue") +
  geom_line(aes(y = predict(model[[4]])), size = 1, colour = "cyan") +
  geom_line(aes(y = predict(model[[5]])), size = 1, colour = "brown") +
  geom_line(aes(y = predict(model[[6]])), size = 1, colour = "yellow") +
  geom_line(aes(y = predict(model[[7]])), size = 1, colour = "orange") +
  geom_line(aes(y = predict(model[[8]])), size = 1, colour = "red4") +
  geom_line(aes(y = predict(model[[9]])), size = 1, colour = "green4") +
  geom_line(aes(y = predict(model[[10]])), size = 1, colour = "blue4") +
  geom_line(aes(y = predict(model[[11]])), size = 1, colour = "cyan4") +
  geom_line(aes(y = predict(model[[12]])), size = 1, colour = "brown4") +
  geom_line(aes(y = predict(model[[13]])), size = 1, colour = "yellow4")
legend(1, 95, legend=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13"),
       col=c("red", "green", "blue", "cyan", "brown", "yellow", "orange","red4", "green4", "blue4", "cyan4", "brown4", "yellow4"), lty=1:2, cex=0.8)


plot(1:10)
dev.new(width=5, height=4)
#plot(1:20)
# Using Legend
# Generate some data
x<-1:10; y1=x*x; y2=2*y1
plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y")
# Add a line
lines(x, y2, pch=18, col="blue", type="b", lty=2)
# Add a legend
#legend(1,95, legend=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13"),
#       col=c("red", "green", "blue", "cyan", "brown", "yellow", "orange","red4", "green4", "blue4", "cyan4", "brown4", "yellow4"), lty=1:2, cex=0.8)
legend("topleft", legend=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13"),
       col=c("red", "green", "blue", "cyan", "brown", "yellow", "orange","red4", "green4", "blue4", "cyan4", "brown4", "yellow4"), lty=1:2, cex=0.8)


#Save plot
dev.copy(tiff, filename="./Model_predictions.tif", width = 7.1, height = 5.5, units = "in", bg = "white", res = 300)
dev.off()

# example
jpeg('rplot.jpg')
plot(x,y)
dev.off()


# OR,
for(i in 1:5){ 
  myPlot <- ggplot(trainData, aes_string(x[i], y)) + geom_point(size = 2) +
    geom_line(aes(y = predict(model[[i]])), size = 1, colour = "red") +
    labs(x = x[i], y = "Biomass_g")
  print(myPlot)
  #Save plot
  dev.copy(tiff, filename=paste("BiomassVs", x[i], ".tif", sep = ""), width = 7.1, height = 5.5, units = "in", bg = "white", res = 300)
  dev.off()
}

#### 8. Model validation ####
#By comparing predictions of other established models with predictions of our selected model
trainData$B_D <- predict(model[[1]], trainData)
trainData$B_DH <- predict(model[[2]], trainData)
trainData$B_D2H <- predict(model[[3]], trainData)
trainData$B_rDH <- predict(model[[4]], trainData)
trainData$B_rD2H <- predict(model[[5]], trainData)


trainData$B_D_Cha <- 71.9320*trainData$x^(1.6991+0.0585*trainData$x)
trainData$B_D2H_Cha <- exp(4.1580 * trainData$D2H^0.1380)
trainData$B_rD2H_Cha <- exp(4.4790 * trainData$rD2H^0.1322)

#Plot two model predictions vs. predictor variable
ggplot() + 
  geom_line(aes(x=trainData$x,y=trainData$B_D),color='red') + 
  geom_line(aes(x=trainData$x,y=trainData$B_D_Cha),color='blue') + 
  labs(x = "D (cm)", y = "Predicted biomass (g)")

ggplot() + 
  geom_line(aes(x=trainData$D2H, y=trainData$B_D2H),color='red') + 
  geom_line(aes(x=trainData$D2H, y=trainData$B_D2H_Cha),color='blue') + 
  labs(x = expression(D^"2"~H*"("*cm^"2"~m*")"), y = "Predicted biomass (g)")

ggplot() + 
  geom_line(aes(x=trainData$rD2H, y=trainData$B_rD2H),color='red') + 
  geom_line(aes(x=trainData$rD2H, y=trainData$B_rD2H_Cha),color='blue') + 
  labs(x = expression(rho*D^"2"~H*"("*gcm^"-3"~cm^"2"~m*")"), y = "Predicted biomass (g)")










