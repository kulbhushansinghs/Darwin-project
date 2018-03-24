# Kullu and Piaopaio working code

rm(list=ls())
setwd("/home/kullu/Desktop/Link to Kullu_desktop/Git/Darwin-project/Data")
d=read.csv("Darwin sample data.csv")
#Libraries
library(Hmisc)
library(AICcmodavg)


na.pattern(d)
complete.cases(d)


#collinearity test

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method="pearson"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 1.6/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.diagonalLine <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                                cex = 1, col.diagLine = "red", ...) 
{ 
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(a=0, b=1, col=col.diagLine)
}
pairs(d[,c(12,14,15,16,18,19,20,21,22)], upper.panel=panel.cor, 
      lower.panel=panel.smooth,
      diag.panel=panel.hist)





Cand.models <- list( )
Cand.models[[1]] <- glm(SL ~ NS, data = d)
Cand.models[[2]] <- glm(SL ~ Edu, data = d)
Cand.models[[3]] <- glm(SL ~ Liv, data = d)
Cand.models[[4]] <- glm(SL ~ LL, data = d)
Cand.models[[5]] <- glm(SL ~ smallL, data = d)
Cand.models[[6]] <- glm(SL ~ Info,family=gaussian(identity), data = d)
Cand.models[[7]] <- glm(SL ~ Inc, data = d)
Cand.models[[8]] <- glm(SL ~ WO,family=gaussian(identity), data = d)
Cand.models[[9]] <- glm(SL ~ Age, data = d)
Cand.models[[10]] <- glm(SL ~ Gen, data = d)
Cand.models[[11]] <- glm(SL ~ Liv+LL+smallL, data = d)
Cand.models[[12]] <- glm(SL ~ NS+Liv+LL+smallL, data = d)
Cand.models[[13]] <- glm(SL ~ NS+Liv+LL+smallL+Inc, data = d)
Cand.models[[14]] <- glm(SL ~ NS+Liv+LL+smallL+Edu, data = d)
Cand.models[[15]] <- glm(SL ~ NS+Edu+Info, data = d)
Cand.models[[16]] <- glm(SL ~ Edu+Inc, data = d)
Cand.models[[17]] <- glm(SL ~ Gen+Age, data = d)
Cand.models[[18]] <- glm(SL ~ Edu+Inc+WO+Liv+LL+smallL, data = d)
Cand.models[[19]] <- glm(SL ~ NS+Liv+Edu, data = d)
Cand.models[[20]] <- glm(SL ~Liv+Edu ,data = d)
Cand.models[[21]] <- glm(SL ~NS+Age+Gen+Edu+NF+Info+Inc+WO+Liv+LL+smallL,data = d)
Cand.models[[22]] <- glm(SL ~1,data = d)
aictab(cand.set = Cand.models, sort = TRUE)



modavg(cand.set = Cand.models,parm = "NS1") 
modavg(cand.set = Cand.models,parm = "NS2") 
modavg(cand.set = Cand.models,parm = "Age") 
modavg(cand.set = Cand.models,parm = "Gen1") 
modavg(cand.set = Cand.models,parm = "Edu2.Primary") 
modavg(cand.set = Cand.models,parm = "Edu3.Secondary") 
modavg(cand.set = Cand.models,parm = "Edu4.Further education") 
modavg(cand.set = Cand.models,parm = "NF") 
modavg(cand.set = Cand.models,parm = "Info1") 
modavg(cand.set = Cand.models,parm = "Inc2") 
modavg(cand.set = Cand.models,parm = "Inc3") 
modavg(cand.set = Cand.models,parm = "Inc4") 
modavg(cand.set = Cand.models,parm = "Inc5") 
modavg(cand.set = Cand.models,parm = "WO1") 
modavg(cand.set = Cand.models,parm = "CouMongolia") 
modavg(cand.set = Cand.models,parm = "CouPakistan") 
modavg(cand.set = Cand.models,parm = "Liv") 
modavg(cand.set = Cand.models,parm = "LL") 
modavg(cand.set = Cand.models,parm = "SL") 
modavg(cand.set = Cand.models,parm = "Age") 
