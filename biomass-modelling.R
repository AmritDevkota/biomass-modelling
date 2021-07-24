# Biomass Modelling Assignment
# Assignment Datasets: Biomass Modelling
# Data file: biomass_data1.csv

# Objectives:
# Estimate tree biomass (dependent) 
# based on easily measurable variables (independent) 
# DBH, Height (and Density, if available)

# First import data
# Data in excel file
# 'readxl' library required to read excel/csv file

# Check list of library ensure the required library there
.libPaths() # Gives path to the library

getwd() # Gives current working directory

## If the library 'realxl' not in the list of libraries in library folder
# Install the library/package
install.packages('readxl')

## Now, once installed you need to load the package
library(readxl)

## save the data with an appropriate variable name
biodata = read.csv("biomass_data1.csv") # If the data in the working directory

# If the excell have multiple sheets
biodata <- as.data.frame(
    read_excel(
        "biomass_data1.xls", 
        sheet = "biomass_data1", 
        range = NULL, 
        col_names = TRUE
    )
)

class(biodata)      # class of data
dim(biodata)        # dimension of data
head(biodata)       # first few rows
str(biodata)        # structure of data
summary(biodata)    # summarize
colnames(biodata)   # column names

# Renaming column
colnames(biodata)[colnames(biodata)=="rho_g/cm3"] <- rho_g_cm3

# variable measured on site: "Diam_cm" "height_m"
# variable to be computed: "biomass_g" "rho_g_cm3"

# Compute volume 
r = biodata$Diam_cm/2
h = biodata$height_m
biodata$vol <- 1/2 * (pi(r/100)^2) * h
v = biodata$vol

# list of variables
myVar <- c("biomass_g", "Diam_cm", "height_m", "vol")

# Using for loop for descriptive statistic
# Test for [1] and later [2:5 or any num] depending on variable number
for (i in myVar[1]){        # Wrong only 1 file running; do either twice or add if else and replace i with number instead of string
    variable <- i
    minimum <- min(biodata[, i], na.rm = T)
    maximum <- max(biodata[, i], na.rm = T)
    mean_ <- mean(biodata[, i], na.rm = T)
    median_ <- median(biodata[, i], na.rm = T)
    stdDev <- sd(biodata[, i], na.rm = T)
    Q1 <- quantile(biodata[, i], 0.25, na.rm = T)
    Q3 <- quantile(biodata[, i], 0.75, na.rm = T)
    IQR <- Q3 - Q1
    CV <- stdDev/mean_
    statistics <- cbind(
        variable, minimum, maximum, mean_, 
        median_, stdDev, Q1, Q3, IQR, CV
    )
    write.table(
        statistics, "descriptive_statistics.csv",
        sep = ",",  col.names = T, row.names = F, append = F
    )
}

## Histogram: show distribution of dep/indep variable not for normality
install.package('ggplot2')
library(ggplot2)    # load package need to install first (install.package(''))
for(i in myVar){
    myPlot <- ggplot(biodata, aes_string(i)) + geom_histogram()
    print(myPlot)
}

# Density plot
for (i in myVar) {
    plot(density(biodata[,i]), main = i)
}

# Boxplot
for (i in myVar) {
    boxplot(biodata[, i], main = i)
}

# Q-Q plot
for(i in myVar){
    qqnorm(biodata[,i], main=i); 
    qqline(biodata[,i],col = 2)
}

# Shapiro_Wilk test for normality
for(i in myVar) {
    print(shapiro.test(biodata[, i]))
}

# Relationship between dep & indep variable require 'ggplot2' package
library(ggplot2) # launch package if not already also install before that if not
for ( i in 2: length(myVar)){
    myPlot <- ggplot(biodata, aes_string(myVar[i], myVar[1])) + geom_point()
    print(myPlot)
}

# Data partition: into Training dataset and Test dataset
# set the seed to make partition reproducible
set.seed(123)

# If 75% data for training purpose
trainIndex = sample(1:nrow(biodata), size = round(0.75 * nrow (biodata)), replace=FALSE)
trainData = biodata[trainIndex , ]
testData = biodata[-trainIndex , ]

trainData = biodata         # Since we have small data

## Select best suited models 
# different combination of predictor variables

# Equation and variable matching
colnames(trainData)[colnames(trainData)==myVar[1]] <- 'D'
colnames(trainData)[colnames(trainData)==myVar[2]] <- 'H'
colnames(trainData)[colnames(trainData)==myVar[3]] <- 'B'

# Candidate models
# 1.	B = a + b (D2)
# 2.	B = a + b (D2) + c (Wd)
# 3.	B = aDb
# 4.	B = a(D2H)b
# 5.	B = aDb Hc
# 6.	ln(B) = ln(a) + b* ln(D)
# 7.	ln(B) = ln(a) + b* ln(D2)
# 8.	ln(B) = ln(a) + b * ln(D2 * H)
# 9.	ln(B) = ln(a) + b * ln(D) + c * ln (H)

list(lm(y ~ x, data = trainData), #Linear form
model <- list(
    lm(B ~ D, data = trainData),
    nls(B ~ a * D^b, data = trainData, start = list(a=0, b=0)),
    nls(B ~ a * ((D^2)*H)^b, data = trainData, start = list(a=0, b=0)),
    nls(B ~ a * (D^b)*H^c, data = trainData, start = list(a=0, b=0)),
    lm(ln(B) ~ ln(a) + b * ln(D)),
    lm(ln(B) ~ ln(a) + b * ln(D^2)),
    lm(ln(B) ~ ln(a) + b * ln((D^2) * H)),
    lm(ln(B) ~ ln(a) + b * ln(D) + c * (H))
)

#Prepare list of candidate models i.e. models with different combination of variables
# model <- list(nls(y ~ a*x^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236)),
#               nls(y ~ a*DH^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236)),
#               nls(y ~ a*D2H^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236)),
#               nls(y ~ a*rDH^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236)),
#               nls(y ~ a*rD2H^b, data = trainData, start = list(a=exp(0.7903), b=-1.8236))
# )

## Model Evaluation
# Statistical evalutation
# Summarize models
for (i in 1: length(model)) {
    print(summary(model[[i]]))
}

# Save model summary in .txt file
sink("./results/model_summary1.txt")

# Save model summary as .csv files
# Install package 'broom'
install.packages('broom')   # If already install not required
library(broom)

# Save coefficient
for (i in 1: length(model)){
  i = toString(i)
  name = paste("model",i,"_coefficient.csv", sep="")
  write.csv(tidy(model[[i]]), name)
}

# Save other statistics
for (i in 1: length(model)){
  i = toString(i)
  name = paste("model",i,"_statistis.csv", sep="")
  write.csv(tidy(model[[i]]), name)
}

# Calculate AIC & AICC values (model with lowest AIC value is best)
install.packages('AICcmodavg')
install.packages('plyr')
install.packages('stringr')

AIC_values <- ldply(model, function(mod){
    data.frame(AICC = AICC(mod), model = deparse(formula(mod)))
})
write.csv(AIC_values, "AIC_values.csv")

# Graphical evaluation
# Residual Analysis: Using residual plots
for(i in 1:4){
    par(mfrow = c(2,2)) # plot 4 plots in 1 panel
    plot(model[[i]], main = paste("model",i,sep=""))
}
for(i in 5:8){
    par(mfrow = c(1,1)) # plot 1 plots in 1 panel
    plot(model[[i]], main = paste("model",i,sep=""))
    print(myPlot)
}

# Q-Q plots for remaining 3 models
for(i in 1:4){
  par(mfrow=c(2,2)) # plot 4 plot in 1 panel
  qqnorm(resid(model[[i]]), main = paste("model",i,sep="")); qqline(resid(model[[i]]), col = 2)
}
for(i in 5:7){
  par(mfrow=c(1,1)) # plot 1 plot in 1 panel
  qqnorm(resid(model[[i]]), main = paste("model",i,sep="")); qqline(resid(model[[i]]), col = 2)
}

# Using Histograms
library(ggplot2)
for(i in 1:length(model)) {
    myPlot <- ggplot(mapping = aes(resid(model[[i]]))) +
        geom_histogram() +
        labs(title = paste("model",i,sep="")
    )
    print(myPlot)
}

# Density Plots
for(i in 1:length(model)){
  plot(density(resid(model[[i]])), main = paste("model",i,sep=""))
}

#Using predicted vs. observed values plots
for(i in 1:length(model)){
  plot(predict(model[[i]]), trainData$y, main = paste("model",i,sep=""), xlab="Predicted values", ylab="Observed vaues")
  abline(a=0,b=1)
  abline(lm(predict(model[[i]])~trainData$y), col="blue")
}

# Using model curves and data used for model fitting
library(ggplot2)            # If the packages don't support
remove.package("ggplot2")   # Remove package
install.package("ggplot2")  # Install Again
library(ggplot2)            # Launch the package

#Compute combined variables
head(trainData)     # Head of data

trainData$DH <- trainData$x * trainData$height_m
trainData$D2H <- trainData$x^2 * trainData$height_m
trainData$rDH <- trainData$rho_g_cm3 * trainData$x * trainData$height_m
trainData$rD2H <- trainData$rho_g_cm3 * trainData$x^2 * trainData$height_m

#Define variables
y <- c("y")
x <- c("x", "DH", "D2H", "rDH", "rD2H")

colour <- c("red", "green", "blue", "cyan", "brown", "yellow", "orange", "black")
ggplot(trainData, aes(x, y)) + geom_point(size = 2) +
  geom_line(aes(y = predict(model[[1]])), size = 1, colour = colour[1]) +
  geom_line(aes(y = predict(model[[2]])), size = 1, colour = colour[2]) +
  geom_line(aes(y = predict(model[[3]])), size = 1, colour = colour[3]) +
  geom_line(aes(y = predict(model[[4]])), size = 1, colour = colour[4]) +
  geom_line(aes(y = predict(model[[5]])), size = 1, colour = colour[5]) +
  geom_line(aes(y = predict(model[[6]])), size = 1, colour = colour[6]) +
  geom_line(aes(y = predict(model[[7]])), size = 1, colour = colour[7]) +
  geom_line(aes(y = predict(model[[7]])), size = 1, colour = colour[8])

#Save plot
dev.copy(tiff, filename="./picture/Model_predictions_vs_data_used_for_model_fitting.tif", width = 7.1, height = 5.5, units = "in", bg = "white", res = 300)
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


























