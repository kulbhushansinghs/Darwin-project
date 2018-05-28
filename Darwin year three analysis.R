## Libraries
library(lme4)

#### Setting working directory
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Darwin-project/Data")

#### Importing individual level data
dat.ind <- read.csv("Individual level data.csv", header = T)

#### Cleaning Individual level data
summary(dat.ind)
names(dat.ind)
table(dat.ind$Community)
table(dat.ind$Age) # multiple symbols used so it will need to be cleaned up manually in the datasheet
table(dat.ind$Age_numeric)
hist(dat.ind$Age_numeric)
table(dat.ind$Gender)
table(dat.ind$education) # Needs manual cleaning up
table(dat.ind$X)
hist(dat.ind$X)
table(dat.ind$Profession)
table(dat.ind$Small_have)
hist(dat.ind$Small_have) 
table(dat.ind$Large_have)
hist(dat.ind$Large_have) 
hist(dat.ind$Small_loss)
hist(dat.ind$Large_loss)
hist(dat.ind$Large_loss)
table(dat.ind$Q1) #what do the resonses mean?
table(dat.ind$Q2) # Need to check what the response means
table(dat.ind$Q3)
table(dat.ind$Attitude)
hist(dat.ind$Attitude) # needs cleaning up in the excelsheet
table(dat.ind$Corral)
table(dat.ind$Vaccination)
table(dat.ind$Insurance)
table(dat.ind$SLE)
table(dat.ind$Number.of.Darwin.schemes)
table(dat.ind$Number.of.other.schemes)
table(dat.ind$number_scheme)
table(dat.ind$accuracy.of.the.overall.interview)
hist(dat.ind$Attitude)

### Some visualizations

boxplot(dat.ind$Attitude[which(dat.ind$Country == "China")],
        dat.ind$Attitude[which(dat.ind$Country == "India")], 
        dat.ind$Attitude[which(dat.ind$Country == "Kyrgyzstan")],
        dat.ind$Attitude[which(dat.ind$Country == "Mongolia")],
        dat.ind$Attitude[which(dat.ind$Country == "Pakistan")], 
        names = c("China", "India", "Kyrgyzstan","Mongolia", "Pakistan"), 
        ylab = "Attitude score")

plot(dat.ind$Attitude~dat.ind$Age, ylab = "Attitude", xlab = "Age")

boxplot(dat.ind$Attitude[which(dat.ind$Gender == "Male")],
        dat.ind$Attitude[which(dat.ind$Gender == "Female")],
        names = c("Male", "Female"), ylab = "Attitude score", col = "grey")

boxplot(dat.ind$Attitude~dat.ind$X, ylab = "Attitude", xlab = "Education categories")

plot(dat.ind$Attitude~dat.ind$Small_have)
plot(dat.ind$Attitude~dat.ind$Small_loss)
plot(dat.ind$Attitude~dat.ind$Large_have)
plot(dat.ind$Attitude~dat.ind$Large_loss)
boxplot(dat.ind$Attitude~dat.ind$Number.of.Darwin.schemes, xlab = "Number of Darwin schemes")
boxplot(dat.ind$Attitude~dat.ind$Number.of.all.schemes, xlab = "Number of all schemes")
boxplot(dat.ind$Attitude~dat.ind$number_scheme, xlab = "Number of schemes" )

par(mfrow = c(2,2))
par(mar = c(4,3,1,1))
boxplot(dat.ind$Attitude~dat.ind$SLE, ylab = "Attitude", xlab = "SLE", col = "grey")
boxplot(dat.ind$Attitude~dat.ind$Vaccination, ylab = "Attitude", xlab = "Vaccination", col = "grey")
boxplot(dat.ind$Attitude~dat.ind$Insurance , ylab = "Attitude", xlab = "Insurance", col = "grey")
boxplot(dat.ind$Attitude~dat.ind$Corral, ylab = "Attitude", xlab = "Corral", col = "grey")


############ Mixed effects models analysis
 mod1 <- lmer(Attitude ~ Age_numeric + X + Gender +
                Profession + Small_have + Small_loss + Large_have +
                Large_loss + Number.of.Darwin.schemes + (1|Country/Community), data = dat.ind)
summary(mod1) 
