#### Setting working directory
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Darwin-project/Data")

#### Importing individual level data
dat.ind <- read.csv("Individual level data.csv", header = T)

#### Cleaning Individual level data
summary(dat.ind)
names(dat.ind)
table(dat.ind$community)
table(dat.ind$age) # multiple symbols used so it will need to be cleaned up manually in the datasheet
table(dat.ind$education) # Needs manual cleaning up
table(dat.ind$profession) # Needs some cleaning up; can be done in R
table(dat.ind$Small_have)
hist(dat.ind$Small_have) #It is not numeric so it will need work in the excel sheet
table(dat.ind$Large_have)
hist(dat.ind$Large_have) #It is not numeric so it will need work in the excel sheet
hist(dat.ind$Small_loss)
hist(dat.ind$Largr_loss)
colnames(dat.ind)[10] <- "Large_loss"
hist(dat.ind$Large_loss)
table(dat.ind$If.a.snow.leopard.is.in.the.area..it.should.not.be.killed.) #what do the resonses mean?
table(dat.ind$If.a.snow.leopard.is.in.the.area.and.causing.damage..they.should.be.killed..) # Need to check what the response means
table(dat.ind$If.a.snow.leopard.is.killed.in.the.area.it.would.make.me.unhappy..)
table(dat.ind$Attitude)
hist(dat.ind$Attitude) # needs cleaning up in the excelsheet
table(dat.ind$Corral)
table(dat.ind$Vacci.tion)
table(dat.ind$Insurance)
table(dat.ind$SLE)
table(dat.ind$Number.of.Darwin.schemes)
table(dat.ind$Number.of.other.schemes)
table(dat.ind$number_scheme)
table(dat.ind$accuracy.of.the.overall.interview)
