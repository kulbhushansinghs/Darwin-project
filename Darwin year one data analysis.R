
#### Setting working directory
setwd("/home/kullu/Desktop/Link to Kullu_desktop/Git/Darwin-project/Data")

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

table(dat$Gender)
dat$Gender[which(dat$Gender == "Fimale")] <- "Female"
dat$Gender[which(dat$Gender == "male")] <- "Male"

table(dat$Education) ## This is too complex to correct using code. I will do this mannually in the datasheet

colnames(dat)[7] <- "Number.of.schemes"
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
table(dat$Type)
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


##################################################################
########## Analysis of the attitude data by country

boxplot(dat$Total_Snow.leopard[which(dat$Country == "Mongolia")],
        dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Snow.leopard[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Snow leopard")

boxplot(dat$Total_Wolf[which(dat$Country == "Mongolia")],
        dat$Total_Wolf[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Wolf[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Wolf")

boxplot(dat$Total_Lynx[which(dat$Country == "Mongolia")],
        dat$Total_Lynx[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Lynx[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Lynx")

boxplot(dat$Total_Brown.bear[which(dat$Country == "Mongolia")],
        dat$Total_Brown.bear[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Brown.bear[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Brown bear")

boxplot(dat$Total_Ibex[which(dat$Country == "Mongolia")],
        dat$Total_Ibex[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Ibex[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Ibex")

boxplot(dat$Total_Argali[which(dat$Country == "Mongolia")],
        dat$Total_Argali[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Argali[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Argali")

boxplot(dat$Total_Markhor[which(dat$Country == "Mongolia")],
        dat$Total_Markhor[which(dat$Country == "Kyrgyzstan")],
        dat$Total_Markhor[which(dat$Country == "Pakistan")], 
        names= c("Mongolia", "Kyrgyzstan", "Pakistan"), main = "Markhor")

hist(dat$Total_Snow.leopard[which(dat$Country == "Kyrgyzstan")], main = "Snow leopard")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Snow leopard")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Wolf")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Lynx")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Brown.bear")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Ibex")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Argali")

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
        names= c("Mongolia_Female", "Mongolia_Male", "Kyrgyzstan _Female", "Kyrgyzstan_male",
                 "Pakistan_Female", "Pakistan_Male"), main = "Blue sheep")

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

library(lme4)
mod3 <- lmer(Total_Snow.leopard ~ Age + Gender + Small_loss + Large_loss + Small_have + Large_have + (1|Country/Community), dat)
summary(mod3)
mod4 <- lmer(Total_Wolf ~ Age + Gender + (1|Country/Community), dat)
summary(mod4)
mod5 <- lmer(Total_Lynx ~ Age + Gender + (1|Community), dat)
summary(mod5)
mod6 <- lmer(Total_Brown.bear ~ Age + Gender + (1|Community), dat)
summary(mod6)
mod7 <- lmer(Total_Ibex ~ Age + Gender + (1|Country/Community), dat)
summary(mod7)
mod8 <- lmer(Total_Argali ~ Age + Gender + (1|Country/Community), dat)
summary(mod8)
