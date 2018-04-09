
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
