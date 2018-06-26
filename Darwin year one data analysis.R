
#### Setting working directory
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Darwin-project/Data")

#### Reading data
dat<-read.csv("Year1data.csv")

#### Cleaning data
names(dat)
dat$Country<- as.character(dat$Country)
unique(dat$Country)
which(dat$Country == "Pakistan ") ## These are the entries with mistaken spellings for Pakistan 
dat$Country[which(dat$Country == "Pakistan ")]<- "Pakistan" #Correcting the spelling

dat$Community <- as.character(dat$Community)
(table(dat$Community))
dat$Community[which(dat$Community == "Tost ")] <- "Tost"
dat$Community[which(dat$Community == "Gegeet ")] <- "Gegeet"
dat$Community[which(dat$Community == "")]<- NA

table(dat$Age)
hist(dat$Age)
table(dat$Country)
unique(dat$Community[which(dat$Country == "Pakistan")])
unique(dat$Community[which(dat$Country == "Mongolia")])
unique(dat$Community[which(dat$Country == "Kyrgyzstan")])
table(dat$Gender, dat$Country)
dat$Gender[which(dat$Gender == "Fimale")] <- "Female"
dat$Gender[which(dat$Gender == "male")] <- "Male"

table(dat$Education) ## This is too complex to correct using code. I will do this mannually in the datasheet
table(dat$Education_ordered)
table(dat$Gender, dat$Education_ordered)
colnames(dat)[8] <- "Number.of.schemes"
table(dat$Number.of.schemes)

table(dat$Vaccination)
sum(table(dat$Vaccination))

table(dat$Corral.Protection)
sum(table(dat$Corral.Protection))

table(dat$Insurance)
sum(table(dat$Insurance))

table(dat$SLE)


table(dat$Ethics.Approval)
dat$Ethics.Approval[which(dat$Ethics.Approval == "y")]<- "Y"
dat$Ethics.Approval[which(dat$Ethics.Approval == "Yes")]<- "Y"
dat$Ethics.Approval[which(dat$Ethics.Approval == 0)]<- "N"

table(dat$Income)  ## I need to check what scale is this

table(dat$Number.of.family.members)
hist(dat$Number.of.family.members)

table(dat$No.female.family.members)
hist(dat$No.female.family.members)

table(dat$No.male.family.members)

table(dat$Info_SL.conservation) #Useful infomation
™table(dat$Type)
table(dat$Often)  # This variable will need to be scaled to a same unit before analysis
table(dat$Others_info_SL.conservation)
table(dat$Info_SL.conservation, dat$Others_info_SL.conservation)
table(dat$Preventive.measures_herding) #This variable will need a lot of cleaning up in the datasheet before use
table(dat$Preventive.measures_corral) #This variable will need a lot of cleaning up in the datasheet before use
table(dat$LS.loss.acceptable) #This variable will need a lot of cleaning up in the datasheet before use

table(dat$Small_loss)
hist(dat$Small_loss)

table(dat$Large_loss)
hist(dat$Large_loss)

table(dat$Small_have)
hist(dat$Small_have)

table(dat$Large_have)
hist(dat$Large_have)

table(dat$Total_Snow.leopard)
hist(dat$Total_Snow.leopard)

table(dat$Total_Wolf)
hist(dat$Total_Wolf)

table(dat$Total_Ibex)
hist(dat$Total_Ibex)

table(dat$Total_Markhor)
hist(dat$Total_Markhor)

table(dat$Total_Argali)
hist(dat$Total_Argali)

table(dat$Total_Lynx)
hist(dat$Total_Lynx)

table(dat$Total_Brown.bear)
hist(dat$Total_Brown.bear)

dat<-subset(dat, select = c(Country, Community, Age, Gender, Education_ordered, Small_loss, 
                            Large_loss, Small_have, Large_have, Total_Snow.leopard, Total_Wolf, 
                            Total_Lynx, Total_Ibex, Total_Brown.bear, Total_Argali, Total_Markhor, Total_Blue.sheep,
                            Number.of.schemes))
dat<-dat[complete.cases(dat[,3:5]),]
##################################################################
########## Analysis of the attitude data by country
par(mfrow = c(2,3))
par(mar = c(2,5,3,1))
boxplot(dat$Total_Snow.leopard[which(dat$Country == "Mongolia")],
        dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Snow.leopard[which(dat$Country == "Pakistan")], 
        #names= c("Mongolia", "Kyrgyzstan", "Pakistan"), 
        main = "Snow leopard", ylab = "Attitude score",
        ylim = c(0,40), col = c("white", "grey", "dark grey"))

boxplot(dat$Total_Wolf[which(dat$Country == "Mongolia")],
        dat$Total_Wolf[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Wolf[which(dat$Country == "Pakistan")], 
        #names= c("Mongolia", "Kyrgyzstan", "Pakistan"), 
        main = "Wolf", ylim = c(0,40), col = c("white", "light grey", "dark grey"))

boxplot(dat$Total_Lynx[which(dat$Country == "Mongolia")],
        dat$Total_Lynx[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Lynx[which(dat$Country == "Pakistan")], 
        #names= c("Mongolia", "Kyrgyzstan", "Pakistan"), 
        main = "Lynx", ylim = c(0,40), col = c("white", "grey", "dark grey"))

par(mar = c(6, 5, 2, 1))
boxplot(dat$Total_Brown.bear[which(dat$Country == "Mongolia")],
        dat$Total_Brown.bear[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Brown.bear[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), ylab = "Attitude score",
        main = "Brown bear", ylim = c(0,40), col = c("white", "grey", "dark grey"), las = 2)

boxplot(dat$Total_Ibex[which(dat$Country == "Mongolia")],
        dat$Total_Ibex[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Ibex[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), 
        main = "Ibex", ylim = c(0,40), col = c("white", "grey", "dark grey"), las = 2)

boxplot(dat$Total_Argali[which(dat$Country == "Mongolia")],
        dat$Total_Argali[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Argali[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), 
        main = "Argali", ylim = c(0,40), col = c("white", "grey", "dark grey"), las = 2)

par(mar = c(3,3,3,0))
boxplot(dat$Total_Markhor[which(dat$Country == "Mongolia")],
        dat$Total_Markhor[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Markhor[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Markhor", ylim = c(0,40))

hist(dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan")], main = "Snow leopard")
summary(lm(dat$Total_Snow.leopard~dat$Country))
#####################################################################
##### Analysis by Sl conservation information

boxplot(dat$Total_Snow.leopard[which(dat$Country == "Mongolia" & 
                                       dat$Info_SL.conservation == "Yes")],
        dat$Total_Snow.leopard[which(dat$Country == "Mongolia" & 
                                       dat$Info_SL.conservation == "No")],
        dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan" & 
                                       dat$Info_SL.conservation == "Yes")],
        dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan" & 
                                       dat$Info_SL.conservation == "No")],
        dat$Total_Snow.leopard[which(dat$Country == "Pakistan" & 
                                       dat$Info_SL.conservation == "Yes")],
        dat$Total_Snow.leopard[which(dat$Country == "Pakistan" & 
                                       dat$Info_SL.conservation == "No")],
        names= c("Mongolia_Yes", "Mongolia_No", "Kyrgyzstan _Yes", "Kyrgyzstan _No",
                 "Pakistan_Yes", "Pakistan_No"), main = "Snow leopard")

#####################################################################
##### Analysis by Gender
par(mfrow = c(2,3))
par(mar = c(1,3,3,0.3))
boxplot(dat$Total_Snow.leopard[which(dat$Country == "Mongolia" & 
                                       dat$Gender == "Female")],
        dat$Total_Snow.leopard[which(dat$Country == "Mongolia" & 
                                       dat$Gender == "Male")],
        dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan" & 
                                       dat$Gender == "Female")],
        dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan" & 
                                       dat$Gender == "Male")],
        dat$Total_Snow.leopard[which(dat$Country == "Pakistan" & 
                                       dat$Gender == "Female")],
        dat$Total_Snow.leopard[which(dat$Country == "Pakistan" & 
                                       dat$Gender == "Male")],
        #names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
        #         "Pakistan\nFemale", "Pakistan\nMale"), 
        main = "Snow leopard", las = 2, col = c("white", "grey"))

boxplot(dat$Total_Wolf[which(dat$Country == "Mongolia" & 
                                       dat$Gender == "Female")],
        dat$Total_Wolf[which(dat$Country == "Mongolia" & 
                                       dat$Gender == "Male")],
        dat$Total_Wolf[which(dat$Country == "Kyrgyzstan" & 
                                       dat$Gender == "Female")],
        dat$Total_Wolf[which(dat$Country == "Kyrgyzstan" & 
                                       dat$Gender == "Male")],
        dat$Total_Wolf[which(dat$Country == "Pakistan" & 
                                       dat$Gender == "Female")],
        dat$Total_Wolf[which(dat$Country == "Pakistan" & 
                                       dat$Gender == "Male")],
        #names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
        #         "Pakistan\nFemale", "Pakistan\nMale"), 
        main = "Wolf", las = 2, col = c("white", "grey"))

boxplot(dat$Total_Lynx[which(dat$Country == "Mongolia" & 
                               dat$Gender == "Female")],
        dat$Total_Lynx[which(dat$Country == "Mongolia" & 
                               dat$Gender == "Male")],
        dat$Total_Lynx[which(dat$Country == "Kyrgyzstan" & 
                               dat$Gender == "Female")],
        dat$Total_Lynx[which(dat$Country == "Kyrgyzstan" & 
                               dat$Gender == "Male")],
        dat$Total_Lynx[which(dat$Country == "Pakistan" & 
                               dat$Gender == "Female")],
        dat$Total_Lynx[which(dat$Country == "Pakistan" & 
                               dat$Gender == "Male")],
        #names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
        #         "Pakistan\nFemale", "Pakistan\nMale"), 
        main = "Lynx", las = 2, col = c("white", "grey"))
par(mar = c(6,3,2,0))
boxplot(dat$Total_Brown.bear[which(dat$Country == "Mongolia" & 
                               dat$Gender == "Female")],
        dat$Total_Brown.bear[which(dat$Country == "Mongolia" & 
                               dat$Gender == "Male")],
        dat$Total_Brown.bear[which(dat$Country == "Kyrgyzstan" & 
                               dat$Gender == "Female")],
        dat$Total_Brown.bear[which(dat$Country == "Kyrgyzstan" & 
                               dat$Gender == "Male")],
        dat$Total_Brown.bear[which(dat$Country == "Pakistan" & 
                               dat$Gender == "Female")],
        dat$Total_Brown.bear[which(dat$Country == "Pakistan" & 
                               dat$Gender == "Male")],
        names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
                 "Pakistan\nFemale", "Pakistan\nMale"), 
        main = "Brown bear", las = 2, col = c("white", "grey"))

boxplot(dat$Total_Ibex[which(dat$Country == "Mongolia" & 
                                     dat$Gender == "Female")],
        dat$Total_Ibex[which(dat$Country == "Mongolia" & 
                                     dat$Gender == "Male")],
        dat$Total_Ibex[which(dat$Country == "Kyrgyzstan" & 
                                     dat$Gender == "Female")],
        dat$Total_Ibex[which(dat$Country == "Kyrgyzstan" & 
                                     dat$Gender == "Male")],
        dat$Total_Ibex[which(dat$Country == "Pakistan" & 
                                     dat$Gender == "Female")],
        dat$Total_Ibex[which(dat$Country == "Pakistan" & 
                                     dat$Gender == "Male")],
        names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
                 "Pakistan\nFemale", "Pakistan\nMale"), main = "Ibex", las = 2, col = c("white", "grey"))

boxplot(dat$Total_Argali[which(dat$Country == "Mongolia" & 
                               dat$Gender == "Female")],
        dat$Total_Argali[which(dat$Country == "Mongolia" & 
                               dat$Gender == "Male")],
        dat$Total_Argali[which(dat$Country == "Kyrgyzstan" & 
                               dat$Gender == "Female")],
        dat$Total_Argali[which(dat$Country == "Kyrgyzstan" & 
                               dat$Gender == "Male")],
        dat$Total_Argali[which(dat$Country == "Pakistan" & 
                               dat$Gender == "Female")],
        dat$Total_Argali[which(dat$Country == "Pakistan" & 
                               dat$Gender == "Male")],
        names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
                 "Pakistan\nFemale", "Pakistan\nMale"), main = "Argali", las =2, col = c("white", "grey"))

boxplot(dat$Total_Blue.sheep[which(dat$Country == "Mongolia" & 
                                 dat$Gender == "Female")],
        dat$Total_Blue.sheep[which(dat$Country == "Mongolia" & 
                                 dat$Gender == "Male")],
        dat$Total_Blue.sheep[which(dat$Country == "Kyrgyzstan" & 
                                 dat$Gender == "Female")],
        dat$Total_Blue.sheep[which(dat$Country == "Kyrgyzstan" & 
                                 dat$Gender == "Male")],
        dat$Total_Blue.sheep[which(dat$Country == "Pakistan" & 
                                 dat$Gender == "Female")],
        dat$Total_Blue.sheep[which(dat$Country == "Pakistan" & 
                                 dat$Gender == "Male")],
        names= c("Mongolia\nFemale", "Mongolia\nMale", "Kyrgyzstan\nFemale", "Kyrgyzstan\nmale",
                 "Pakistan\nFemale", "Pakistan\nMale"), main = "Blue sheep", las = 2)

boxplot(dat$Total_Markhor[which(dat$Country == "Mongolia" & 
                                     dat$Gender == "Female")],
        dat$Total_Markhor[which(dat$Country == "Mongolia" & 
                                     dat$Gender == "Male")],
        dat$Total_Markhor[which(dat$Country == "Kyrgyzstan" & 
                                     dat$Gender == "Female")],
        dat$Total_Markhor[which(dat$Country == "Kyrgyzstan" & 
                                     dat$Gender == "Male")],
        dat$Total_Markhor[which(dat$Country == "Pakistan" & 
                                     dat$Gender == "Female")],
        dat$Total_Markhor[which(dat$Country == "Pakistan" & 
                                     dat$Gender == "Male")],
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Markhor")
###############################################################################
##### Analysing gender and SLE

dat.female<-subset(dat, dat$Gender == "Female")

boxplot(dat.female$Total_Snow.leopard[which(dat.female$SLE == 1)],
        dat.female$Total_Snow.leopard[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Snow leopard")

boxplot(dat.female$Total_Wolf[which(dat.female$SLE == 1)],
        dat.female$Total_Wolf[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Wolf")

boxplot(dat.female$Total_Lynx[which(dat.female$SLE == 1)],
        dat.female$Total_Lynx[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Lynx")

boxplot(dat.female$Total_Brown.bear[which(dat.female$SLE == 1)],
        dat.female$Total_Brown.bear[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Brown bear")

boxplot(dat.female$Total_Ibex[which(dat.female$SLE == 1)],
        dat.female$Total_Ibex[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Ibex")

boxplot(dat.female$Total_Argali[which(dat.female$SLE == 1)],
        dat.female$Total_Argali[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Argali")

boxplot(dat.female$Total_Markhor[which(dat.female$SLE == 1)],
        dat.female$Total_Markhor[which(dat.female$SLE != 1)], 
        names= c("SLE", "No SLE"), main = "Markhor")

############################################################################
######## Analysis with age

hist(dat$Age)
boxplot(dat$Age[which(dat$Gender == "Female")], 
        dat$Age[which(dat$Gender == "Male")], names = c("Female", "Male"), main = "Age")

plot(dat$Total_Snow.leopard ~ dat$Age, xlab = "Age", ylab = "Attitude SL", 
     cex = 1, pch = 20, col = c(ifelse(dat$Gender == "Female", 1, 2)))


mod1 <- lm(dat$Total_Snow.leopard ~ dat$Age)
summary(mod1)
abline(mod1)

plot(dat$Total_Wolf ~ dat$Age, xlab = "Age", ylab = "Attitude Wolf", 
     cex = 1, pch = 20, col = c(ifelse(dat$Gender == "Female", 1, 2)))
mod2 <- lm(dat$Total_Wolf ~ dat$Age)

summary(mod2)
abline(mod2)

#### Analysis by education

hist(dat$Education_ordered)

## Attitudes to snow leopard by education
par(mfrow = c(2,3))
par(mar = c(1,5,3,0.3))

boxplot(dat$Total_Snow.leopard[which(dat$Education_ordered == 0)],
        dat$Total_Snow.leopard[which(dat$Education_ordered == 1)],
        dat$Total_Snow.leopard[which(dat$Education_ordered == 2)],
        dat$Total_Snow.leopard[which(dat$Education_ordered == 3)], 
        col = grey.colors(4), main = "Snow leopard", ylab = "Attitude score")
par(mar = c(1,3,3,0.3))
boxplot(dat$Total_Wolf[which(dat$Education_ordered == 0)],
        dat$Total_Wolf[which(dat$Education_ordered == 1)],
        dat$Total_Wolf[which(dat$Education_ordered == 2)],
        dat$Total_Wolf[which(dat$Education_ordered == 3)], 
        col = grey.colors(4), main = "Wolf")

boxplot(dat$Total_Lynx[which(dat$Education_ordered == 0)],
        dat$Total_Lynx[which(dat$Education_ordered == 1)],
        dat$Total_Lynx[which(dat$Education_ordered == 2)],
        dat$Total_Lynx[which(dat$Education_ordered == 3)], 
        col = grey.colors(4), main = "Lynx")

par(mar = c(5,5,3,0.3))
boxplot(dat$Total_Brown.bear[which(dat$Education_ordered == 0)],
        dat$Total_Brown.bear[which(dat$Education_ordered == 1)],
        dat$Total_Brown.bear[which(dat$Education_ordered == 2)],
        dat$Total_Brown.bear[which(dat$Education_ordered == 3)], 
        col = grey.colors(4), main = "Brown.bear", ylab = "Attitude score",
        names = c("None", "Primary", "Secondary", "Higher"), las = 2)
par(mar = c(5,3,3,0.3))
boxplot(dat$Total_Ibex[which(dat$Education_ordered == 0)],
        dat$Total_Ibex[which(dat$Education_ordered == 1)],
        dat$Total_Ibex[which(dat$Education_ordered == 2)],
        dat$Total_Ibex[which(dat$Education_ordered == 3)], 
        col = grey.colors(4), main = "Ibex",
        names = c("None", "Primary", "Secondary", "Higher"), las =2 )

boxplot(dat$Total_Argali[which(dat$Education_ordered == 0)],
        dat$Total_Argali[which(dat$Education_ordered == 1)],
        dat$Total_Argali[which(dat$Education_ordered == 2)],
        dat$Total_Argali[which(dat$Education_ordered == 3)], 
        col = grey.colors(4), main = "Argali",
        names = c("None", "Primary", "Secondary", "Higher"), las = 2)

##### LMER analsis with livestock data hence all Kyrgyzstan points are ignored
library(lme4)
mod3 <- lmer(Total_Snow.leopard ~ Age + Gender + Education_ordered + Small_loss + Large_loss + Small_have + Large_have + (1|Country/Community), dat)
summary(mod3)
Sl.param <- summary(mod3)$coefficients[2:8,1]
Sl.ci <- summary(mod3)$coefficients[2:8,2]

mod4 <- lmer(Total_Wolf ~ Age + Gender + Education_ordered + Small_loss + Large_loss + Small_have + Large_have + (1|Country/Community), dat)
summary(mod4)
Wolf.param <- summary(mod4)$coefficients[2:8,1]
Wolf.ci <- summary(mod4)$coefficients[2:8,2]

mod5 <- lmer(Total_Lynx ~ Age + Gender + Education_ordered + Small_loss + Large_loss + Small_have + Large_have + (1|Community), dat)
summary(mod5)
Lynx.param <- summary(mod5)$coefficients[2:8,1]
Lynx.ci <- summary(mod5)$coefficients[2:8,2]

mod6 <- lmer(Total_Brown.bear ~ Age + Gender + Education_ordered + Small_loss + Large_loss + Small_have + Large_have + (1|Community), dat)
summary(mod6)
B.bear.param <- summary(mod6)$coefficients[2:8,1]
B.bear.ci <- summary(mod6)$coefficients[2:8,2]

mod7 <- lmer(Total_Ibex ~ Age + Gender + Education_ordered + Small_loss + Large_loss + Small_have + Large_have + (1|Country/Community), dat)
summary(mod7)
Ibex.param <- summary(mod7)$coefficients[2:8,1]
Ibex.ci <- summary(mod7)$coefficients[2:8,2]

mod8 <- lmer(Total_Argali ~ Age + Gender + Education_ordered + Small_loss + Large_loss + Small_have + Large_have + (1|Country/Community), dat)
summary(mod8)
Argali.param <- summary(mod8)$coefficients[2:8,1]
Argali.ci <- summary(mod8)$coefficients[2:8,2]
## plotting the modelling results
library(Hmisc)
par(mfrow = c(2,3))
par(mar = c(2,4,3,0.3))
plot(Sl.param, ylim = c(-2,3), ylab = "Parameter estimate", main = "Snow leopard", 
     xaxt = c("n"), xlab = "", xlim = c(0.5, 7.5))
errbar(seq(1,7,1), Sl.param, Sl.param+(1.96 * Sl.ci), Sl.param - (1.96 * Sl.ci), add= T, lwd = 2, cex =2)
abline(h = 0, lwd =2)

par(mar = c(2,3,3,0.3))
plot(seq(1,7,1), Wolf.param, ylim = c(-2,3), ylab = "Parameter estimate", main = "Wolf", 
     xaxt = c("n"), xlab = "", xlim = c(0.5, 7.5))
errbar(seq(1,7,1), Wolf.param, Wolf.param+(1.96 * Wolf.ci), Wolf.param - (1.96 * Wolf.ci), add= T, cex = 2, lwd = 2)
abline(h = 0, lwd = 2)

plot(seq(1,7,1), Lynx.param, ylim = c(-2,3), ylab = "Parameter estimate", main = "Lynx",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 7.5))
errbar(seq(1,7,1), Lynx.param, Lynx.param+(1.96 * Lynx.ci), 
       Lynx.param - (1.96 * Lynx.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)

par(mar = c(6,4,1,0.3))
plot(seq(1,7,1), B.bear.param, ylim = c(-4,3), ylab = "Parameter estimate", main = "Brown bear",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 7.5))
errbar(seq(1,7,1), B.bear.param, B.bear.param+(1.96 * B.bear.ci), 
       B.bear.param - (1.96 * B.bear.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)
axis(1, labels = c("Age", "Gender\n(male)", "Education", 
                   "Small stock\nlost", "Large stock\nlost", "Small stock\nowned", "Large stock\nowned"), 
     at = c(1,2,3,4,5,6,7), las = 2)

par(mar = c(6,3,1,0.3))
plot(seq(1,7,1), Ibex.param, ylim = c(-4,3), ylab = "", main = "Ibex",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 7.5))
errbar(seq(1,7,1), Ibex.param, Ibex.param+(1.96 * Ibex.ci), 
       Ibex.param - (1.96 * Ibex.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)
axis(1, labels = c("Age", "Gender\n(male)", "Education", 
                   "Small stock\nlost", "Large stock\nlost", "Small stock\nowned", "Large stock\nowned"), 
     at = c(1,2,3,4,5,6,7), las = 2)

plot(Argali.param, ylim = c(-4,3), ylab = "", main = "Argali",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 7.5))
errbar(seq(1,7,1), Argali.param, Argali.param+(1.96 * Argali.ci), 
       Argali.param - (1.96 * Argali.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)
axis(1, labels = c("Age", "Gender\n(male)", "Education", 
                   "Small stock\nlost", "Large stock\nlost", "Small stock\nowned", "Large stock\nowned"), 
     at = c(1,2,3,4,5,6,7), las = 2)

#### LMER analysis without livestock data thus includes all complete cases
mod3.1 <- lmer(Total_Snow.leopard ~ Age + Gender + Education_ordered + (1|Country/Community), dat, REML = F)
summary(mod3.1)
Sl.param <- summary(mod3.1)$coefficients[2:4,1]
Sl.ci <- summary(mod3.1)$coefficients[2:4,2]

mod4.1 <- lmer(Total_Wolf ~ Age + Gender + Education_ordered + (1|Country/Community), dat, REML = F)
summary(mod4.1)
Wolf.param <- summary(mod4.1)$coefficients[2:4,1]
Wolf.ci <- summary(mod4.1)$coefficients[2:4,2]

mod5.1 <- lmer(Total_Lynx ~ Age + Gender + Education_ordered + (1|Community), dat, REML = F)
summary(mod5.1)
Lynx.param <- summary(mod5.1)$coefficients[2:4,1]
Lynx.ci <- summary(mod5.1)$coefficients[2:4,2]

mod6.1 <- lmer(Total_Brown.bear ~ Age + Gender + Education_ordered + (1|Community), dat, REML = F)
summary(mod6.1)
B.bear.param <- summary(mod6.1)$coefficients[2:4,1]
B.bear.ci <- summary(mod6.1)$coefficients[2:4,2]

mod7.1 <- lmer(Total_Ibex ~ Age + Gender + Education_ordered + (1|Country/Community), dat, REML = F)
summary(mod7.1)
Ibex.param <- summary(mod7.1)$coefficients[2:4,1]
Ibex.ci <- summary(mod7.1)$coefficients[2:4,2]

mod8.1 <- lmer(Total_Argali ~ Age + Gender + Education_ordered +  (1|Country/Community), dat, REML = F)
summary(mod8.1)
Argali.param <- summary(mod8.1)$coefficients[2:4,1]
Argali.ci <- summary(mod8.1)$coefficients[2:4,2]

## plotting the modelling results
library(Hmisc)
par(mfrow = c(2,3))
par(mar = c(2,4,3,0.3))
plot(Sl.param, ylim = c(-4,4), ylab = "Parameter estimate", main = "Snow leopard", 
     xaxt = c("n"), xlab = "", xlim = c(0.5, 3.5))
errbar(seq(1,3,1), Sl.param, Sl.param+(1.96 * Sl.ci), Sl.param - (1.96 * Sl.ci), add= T, lwd = 2, cex =2)
abline(h = 0, lwd =2)

par(mar = c(2,3,3,0.3))
plot(seq(1,3,1), Wolf.param, ylim = c(-4,4), ylab = "Parameter estimate", main = "Wolf", 
     xaxt = c("n"), xlab = "", xlim = c(0.5, 3.5))
errbar(seq(1,3,1), Wolf.param, Wolf.param+(1.96 * Wolf.ci), Wolf.param - (1.96 * Wolf.ci), add= T, , cex = 2, lwd = 2)
abline(h = 0, lwd = 2)

plot(seq(1,3,1), Lynx.param, ylim = c(-4,4), ylab = "Parameter estimate", main = "Lynx",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 3.5))
errbar(seq(1,3,1), Lynx.param, Lynx.param+(1.96 * Lynx.ci), 
       Lynx.param - (1.96 * Lynx.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)

par(mar = c(5,4,1,0.3))
plot(seq(1,3,1), B.bear.param, ylim = c(-4,4), ylab = "Parameter estimate", main = "Brown bear",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 3.5))
errbar(seq(1,3,1), B.bear.param, B.bear.param+(1.96 * B.bear.ci), 
       B.bear.param - (1.96 * B.bear.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)
axis(1, labels = c("Age", "Gender\n(male)", "Education"), at = c(1,2,3), las = 2)

par(mar = c(5,3,1,0.3))
plot(seq(1,3,1), Ibex.param, ylim = c(-4,4), ylab = "", main = "Ibex",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 3.5))
errbar(seq(1,3,1), Ibex.param, Ibex.param+(1.96 * Ibex.ci), 
       Ibex.param - (1.96 * Ibex.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)
axis(1, labels = c("Age", "Gender\n(male)", "Education"), at = c(1,2,3), las = 2)

plot(Argali.param, ylim = c(-4,4), ylab = "", main = "Argali",
     xaxt = c("n"), xlab = "", xlim = c(0.5, 3.5))
errbar(seq(1,3,1), Argali.param, Argali.param+(1.96 * Argali.ci), 
       Argali.param - (1.96 * Argali.ci), add= T, lwd = 2, cex = 2)
abline(h = 0, lwd = 2)
axis(1, labels = c("Age", "Gender\n(male)", "Education"), at = c(1,2,3), las = 2, cex.lab = 2)
######## GLM analysis
mod9 <- glm(Total_Snow.leopard ~ Age + Gender + Education_ordered + Country, data = dat)
summary(mod9)

mod10 <- glm(Total_Wolf ~  Gender + Country, data = dat)
summary(mod10)

par(mfrow = c(2,2))
names(dat)
par(mar = c(3,4,2,1))
boxplot(dat$Total_Snow.leopard[which(dat$Number.of.schemes == 0)],
        dat$Total_Snow.leopard[which(dat$Number.of.schemes == 1)],
        dat$Total_Snow.leopard[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Snow leopard")
boxplot(dat$Total_Wolf [which(dat$Number.of.schemes == 0)],
        dat$Total_Wolf[which(dat$Number.of.schemes == 1)],
        dat$Total_Wolf[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Wolf")
par(mar = c(4,4,2,1))
boxplot(dat$Total_Lynx[which(dat$Number.of.schemes == 0)],
        dat$Total_Lynx[which(dat$Number.of.schemes == 1)],
        dat$Total_Lynx[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Lynx", xlab = "Number of schemes")
boxplot(dat$Total_Brown.bear[which(dat$Number.of.schemes == 0)],
        dat$Total_Brown.bear[which(dat$Number.of.schemes == 1)],
        dat$Total_Brown.bear[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Brown bear", xlab = "Number of schemes")

par(mar = c(3,4,2,1))
boxplot(dat$Total_Ibex[which(dat$Number.of.schemes == 0)],
        dat$Total_Ibex[which(dat$Number.of.schemes == 1)],
        dat$Total_Ibex[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Ibex")
boxplot(dat$Total_Argali[which(dat$Number.of.schemes == 0)],
        dat$Total_Argali[which(dat$Number.of.schemes == 1)],
        dat$Total_Argali[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Argali")
par(mar = c(4,4,2,1))
boxplot(dat$Total_ [which(dat$Number.of.schemes == 0)],
        dat$Total_Argali[which(dat$Number.of.schemes == 1)],
        dat$Total_Argali[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Markhor", xlab = "Number of schemes")
boxplot(dat$Total_Argali[which(dat$Number.of.schemes == 0)],
        dat$Total_Argali[which(dat$Number.of.schemes == 1)],
        dat$Total_Argali[which(dat$Number.of.schemes >= 2)], 
        names = c("0", "1", "2"), ylab = "Attitude", main = "Blue sheep", xlab = "Number of schemes")

mod.sl <- lmer(Total_Snow.leopard ~ Age + Gender + Education_ordered + Number.of.schemes +
                (1|Country/Community), data = dat)
summary(mod.sl)

mod.wolf <- lmer(Total_Wolf ~ Age + Gender + Education_ordered + Number.of.schemes +
                 (1|Country/Community), data = dat)
summary(mod.wolf)

mod.lynx <- lmer(Total_Lynx ~ Age + Gender + Education_ordered + Number.of.schemes +
                               (1|Community), data = dat)
summary(mod.lynx)

mod.bb <- lmer(Total_Brown.bear ~ Age + Gender + Education_ordered + Number.of.schemes +
                 (1|Community), data = dat)
summary(mod.bb)

mod.ibex <- lmer(Total_Ibex ~ Age + Gender + Education_ordered + Number.of.schemes +
                 (1|Country/Community), data = dat)
summary(mod.ibex)

mod.argali <- lmer(Total_Argali ~ Age + Gender + Education_ordered + Number.of.schemes +
                 (1|Country/Community), data = dat)
summary(mod.argali)

mod.markhor <- lmer(Total_Markhor ~ Age + Gender + Education_ordered + Number.of.schemes +
                 (1|Country/Community), data = dat)
summary(mod.markhor)

mod.bs <- lmer(Total_Blue.sheep ~ Age + Gender + Education_ordered + Number.of.schemes +
                 (1|Country/Community), data = dat)
summary(mod.bs)

####################################
### Analysis with the number of schmes

par(mfrow = c(2,3))
par(mar = c(2,4,3,0.3))
boxplot(dat$Total_Snow.leopard[which(dat$Number.of.schemes == 0)],
        dat$Total_Snow.leopard[which(dat$Number.of.schemes == 1)],
        dat$Total_Snow.leopard[which(dat$Number.of.schemes >= 2)], 
        names= c("0", "1", ">2"), ylab = "Attitude score", main = "Snow leopard")
boxplot(dat$Total_Wolf[which(dat$Number.of.schemes == 0)],
        dat$Total_Wolf[which(dat$Number.of.schemes == 1)],
        dat$Total_Wolf[which(dat$Number.of.schemes >= 2)], 
        names= c("0", "1", ">2"), ylab = "Attitude score", main = "Wolf")
boxplot(dat$Total_Lynx[which(dat$Number.of.schemes == 0)],
        dat$Total_Lynx[which(dat$Number.of.schemes == 1)],
        dat$Total_Lynx[which(dat$Number.of.schemes >= 2)], 
        names= c("0", "1", ">2"), ylab = "Attitude score", main = "Lynx")
par(mar = c(4,4,3,0.3))
boxplot(dat$Total_Brown.bear[which(dat$Number.of.schemes == 0)],
        dat$Total_Brown.bear[which(dat$Number.of.schemes == 1)],
        dat$Total_Brown.bear[which(dat$Number.of.schemes >= 2)], 
        names= c("0", "1", ">2"), ylab = "Attitude score", main = "Brown bear",
        xlab = "Number of Schemes")
boxplot(dat$Total_Ibex[which(dat$Number.of.schemes == 0)],
        dat$Total_Ibex[which(dat$Number.of.schemes == 1)],
        dat$Total_Ibex[which(dat$Number.of.schemes >= 2)], 
        names= c("0", "1", ">2"), ylab = "Attitude score", main = "Ibex",
        xlab = "Number of Schemes")
boxplot(dat$Total_Argali[which(dat$Number.of.schemes == 0)],
        dat$Total_Argali[which(dat$Number.of.schemes == 1)],
        dat$Total_Argali[which(dat$Number.of.schemes >= 2)], 
        names= c("0", "1", ">2"), ylab = "Attitude score", main = "Argali",
        xlab = "Number of Schemes")

