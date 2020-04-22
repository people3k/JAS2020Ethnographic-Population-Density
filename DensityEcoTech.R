####Simple analysis of human population densities using multilevel regression

library(reshape2)
library(xtable)
library(ggplot2)
library(plyr)
library(scales)
library(effects)
library(splines)
library(latticeExtra) 
library(nlme)

#1.Explore ralationship between species diversity and human population density, controlling for
# basic economic orientation (capitalist world system, subsistence agriculture, hunter-gatherer)
#Load data
AllSoc<-read.csv(file="AllSocieties.csv", header=T)

#A.#Make scatter plots

###Scatter plot of NPP and Species richness by technology group.

p <- ggplot(AllSoc, aes((npp), (biodiv)))
p + aes(shape = factor(ID), color = factor(ID)) +
  theme_bw() +
  geom_point(aes(size = factor(ID)))+ 
  scale_size_manual(values=c(3,1,2))+
  theme(axis.text = element_text(size = rel(1.5), colour = "black"), axis.title=element_text(size=22))+
  labs(x = "NPP", y="Species richness")+
  #geom_smooth(method='lm', formula=y ~ ns(x, 2), se=FALSE)
  #geom_smooth(se=FALSE)+
  geom_vline(xintercept=1350)+
  geom_hline(yintercept=0.3)
#geom_vline(xintercept=0.38)


#basic exploratory plot of NPP against Standardized population density. Z-scores used to standardizeboth variables. 
#Density stadardized using the mean and standard deviation of each respective technology group
p <- ggplot(AllSoc, aes((nppZ), (DENZ)))
p + aes(shape = factor(ID), color = factor(ID)) +
  theme_bw() +
  geom_point(aes(size = factor(ID)))+ 
  scale_size_manual(values=c(3,1,2))+
  theme(axis.text = element_text(size = rel(1.5), colour = "black"), axis.title=element_text(size=16))+
  labs(x = "Standardized NPP", y="Standardized Population density")+
  geom_smooth(method='lm', formula=y ~ ns(x, 2), se=0.95)



####General Least Squares model of standardized ecosystem attributes on the log of human population density.
###Equation 1 from the main text
glsm<-gls(log(DENSITY)~nppZ+biodivZ+pathosZ+factor(ID2)+nppZ:biodivZ:pathosZ, method="ML", data=AllSoc)
##Call model values and coefficients
summary(glsm)
#plot diagnostics 
plot(glsm)
###Plot the interaction effects of NPP, Species richness and pathogens
plot(allEffects(glsm), multiline=TRUE)

##Combine the fitted values from the glsm model with the AllSoc datafram
predict2<-cbind(glsm$fitted,AllSoc)


# Plot contours of fitted values  NPP VS Density with Species richness contours

data <- data.frame(NPP = predict2$nppZ, Density = glsm$fitted) 
data$z <- with(data, AllSoc$biodivZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.6, 2.3)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39))+
  layer(panel.abline(glsm, col="green"))

data <- data.frame(Biodiversity = predict2$biodivZ, Density = glsm$fitted) 
data$z <- with(data, predict2$nppZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  Biodiversity* Density|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.7,2.6)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39)) 


# Plot contours of fitted values  NPP VS Density with pathogen contours

data <- data.frame(NPP = predict2$nppZ, Density = glsm$fitted) 
data$z <- with(data, predict2$pathosZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.6,2.4)) + 
  layer_(panel.2dsmoother(..., n = 100))


# Plot contours of fitted values  NPP VS Species richness with log density contours

data <- data.frame(NPP = predict2$nppZ, Biodiversity = predict2$biodiv) 
data$z <- with(data, glsm$fitted) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|factor(predict2$ID), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.6,2.4)) + 
  layer_(panel.2dsmoother(..., n = 100))


###MixedEffects Models

######Check for three way interaction of NPP, biodiv and pathos
#simple interept model
intercept<-gls(log(DENSITY) ~ 1, method="ML", control=list(opt="optim"), data=AllSoc)
#random intercept model
randomIntercept<-lme(log(DENSITY) ~ 1, random=~1|ID,  method="ML", control=list(opt="optim"), data=AllSoc)
#Equation 1: gls model with no random effects
glsm<-gls(log(DENSITY)~nppZ+biodivZ+pathosZ+nppZ:biodivZ:pathosZ, method="ML", data=AllSoc)
#Equation 2: random interept  model
TimeRI<-update(randomIntercept,.~.+nppZ+biodivZ+pathosZ+nppZ:biodivZ:pathosZ)
#Equation 3: Random intercept and slope model
TimeRS<-update(TimeRI,.~.+nppZ+biodivZ+pathosZ+nppZ:biodivZ:pathosZ, random=~nppZ|ID)

###Check the fit of the models (Table 1 in mian text)
ModelComp<-anova(intercept, randomIntercept, glsm, TimeRI,TimeRS)
ModelComp
#Export table to latex code
xtable(ModelComp)

####Plot the random effects of the best model (TimeTS, Equation 3)
RE<-random.effects(TimeRS)
plot(RE)
Int<-intervals(TimeRS)
Int

###Summary of the TimeRS model andrandom coefficients
RS<-coef(TimeRS)
RS
xtable(RS)
summary(TimeRS)

##SI ANALYSIS Examine the Fit of TimeRS Mixed Effect Model
fit<-predict(TimeRS, interval = 'confidence')

DenBio.predict <- cbind(AllSoc, fit)

# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(DenBio.predict, aes(nppZ,log(DENSITY)))
p <- p + geom_point(shape=15,aes(color=factor(ID2), size=factor(ID2)))
#p <-p+scale_size_manual(values=c(3,2,2,4))
p <- p + geom_line(aes(nppZ, fit))+
  #p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.3)+
  #stat_function(fun=f, colour="Treat")+
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.5), colour = "black"), axis.title=element_text(size=18))+
  labs(x = "Net primary productivity", y="Population density")+
  facet_wrap(~factor(ID2))
p


###Combine predicted values from TimeRS with AllSoc dataframe
predict2<-cbind(TimeRS$fitted,AllSoc)

#Write predict to a table
write.table(predict2, file = "MixedEffectPre.csv", sep = ",")

#Plot the interactions of the fixed effects using the effects package
plot(allEffects(TimeRS), multiline=TRUE)
TimeEffect<-(allEffects(TimeRS, xlevels=list(nppZ=c(-1,0,1,1.5),biodivZ=c(-1,0,1,1.5),pathosZ=c(-1,0,1,1.5))))
TimeEffect

##Plot interaction of fixed effects using ggplot
Effect1<-read.csv(file="OptEffect.csv", header=T)

p <- ggplot(Effect1, aes((npp), PreDen)) 
p<- p + aes(colour=factor(biodivZ)) +
  theme_bw() +
  geom_point()+ 
  # scale_x_discrete(limits=c("1", "2", "3", "4","5","6"))+
  #ylim(-.09,0.88)+
  theme(axis.text = element_text(size = rel(1.25), colour = "black"), axis.title=element_text(size=24))+
  labs(x = "NPP", y="ln Predicted density")+
  geom_smooth(method='lm', formula=y ~ ns(x, 1), se=FALSE)+
  #geom_smooth()+
  #geom_hline(yintercept = mean(Effect1$lnProportion))+
  facet_wrap(pathosZ~.)+
  theme(strip.text.x = element_text(size = 16, colour = "black"))
#geom_smooth(se=FALSE)
p


# Plot contours of fitted values for the TimeRS models and compare with real values

#Plot of NPP vs Predicted log density, species richness contours. Best fit lines plotted
data <- data.frame(NPP = predict2$nppZ, Density = predict2$ID) 
data$z <- with(data, predict2$biodivZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.58, 2.25)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.49)) +
  layer(panel.abline(-2.21, 1.1))+
  layer(panel.abline(0.47, 0.83))+
  layer(panel.abline(3.94, 0.48))

##Real population densities #Plot of NPP vs Real log density, species richness contours. Best fit lines plotted

data <- data.frame(NPP = predict2$nppZ, Density = log(predict2$DENSITY)) 
data$z <- with(data, predict2$biodivZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.58, 2.25)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.49)) +
  layer(panel.abline(-2.21, 1.1))+
  layer(panel.abline(0.47, 0.83))+
  layer(panel.abline(3.94, 0.48))


#Plot of NPP vs Predicted log density, pathogen contours. Best fit lines plotted
data <- data.frame(NPP = predict2$nppZ, Density = predict2$ID) 
data$z <- with(data, predict2$pathosZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.58,2.25)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.89)) +
  layer(panel.abline(-2.21, 1.1))+
  layer(panel.abline(0.47, 0.83))+
  layer(panel.abline(3.94, 0.48))

#Plot of NPP vs. Species richness with predicted density contours

data <- data.frame(NPP = predict2$nppZ, Biodiversity = predict2$biodivZ) 
data$z <- with(data, predict2$ID) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","darkred"))(1e3),
          xlim=c(-1.6,2.3), ylim = c(-1.5,2.9)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))

####Biodiv vs. Predicted Density with NPP contours

data <- data.frame(Biodiv = predict2$biodivZ, Density = predict2$ID) 
data$z <- with(data, predict2$nppZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  Biodiv* Density|predict2$ID2, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.75, 2.5)) + 
  layer_(panel.2dsmoother(..., n = 100))
# layer(panel.abline(v = 0.49)) +
#  layer(panel.abline(-2.21, 1.1))+
#  layer(panel.abline(0.47, 0.83))+
#  layer(panel.abline(3.94, 0.48))


#####Contours of three way effects of NPP, species richness and pathogens on population density, holding 
#technology group equal.===================================

##Load seperated files
HGMixed<-read.csv(file="HGMixed.csv", header=T)
AGGMixed<-read.csv(file="AGGMixed.csv", header=T)
INDMixed<-read.csv(file="INDMixed.csv", header=T)


##Hunter-gatherer mixed effect predicted density TimeRS
data <- data.frame(NPP = HGMixed$nppZ, Biodiversity = HGMixed$biodivZ) 
data$z <- with(data, HGMixed$ID) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|HGMixed$pathID, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.4,2.2), ylim = c(-1.5,2.8)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))

#hunter-gatherer real population density contours graphed against NPP, species richness and high vs. low pathogens
data <- data.frame(NPP = HGMixed$nppZ, Biodiversity = HGMixed$biodivZ) 
data$z <- with(data, HGMixed$DENSITY) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|HGMixed$pathID, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.4,2.2), ylim = c(-1.5,2.8)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))

#3Agricultural mixed effect predicted density countours NPP vs biodiversity holding pathogens equal at high and low
data <- data.frame(NPP = AGGMixed$nppZ, Biodiversity = AGGMixed$biodivZ) 
data$z <- with(data, AGGMixed$ID) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|AGGMixed$pathID, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.4,2.2), ylim = c(-1.5,2.8)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))

#Agriculturalist real population density contours graphed against NPP, species richness and high vs. low pathogens
data <- data.frame(NPP = AGGMixed$nppZ, Biodiversity = AGGMixed$biodivZ) 
data$z <- with(data, AGGMixed$DENSITY) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|AGGMixed$pathID, data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.4,2.2), ylim = c(-1.5,2.8)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))

#Industrial mixed effect predicted density for TimeRS
data <- data.frame(NPP = INDMixed$nppZ, Biodiversity = INDMixed$biodivZ) 
data$z <- with(data, INDMixed$ID) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|INDMixed$pathID, data, 
          panel = panel.levelplot.points, 
          cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.4,2.15), ylim = c(-1.5,2.8)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))


#Industrial real population density contours graphed against NPP, species richness and high vs. low pathogens

data <- data.frame(NPP = INDMixed$nppZ, Biodiversity = INDMixed$biodivZ) 
data$z <- with(data, INDMixed$DENSITY) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|INDMixed$pathID, data, 
          panel = panel.levelplot.points, 
          cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.4,2.15), ylim = c(-1.5,2.8)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))


####SI ANALYSIS of Two-way interaction effects#######################
##SI===========================================================================
#######===========================================================================
#===================================================


######Check for seperate interactions of NPP, biodiv and pathos
intercept<-gls(log(DENSITY) ~ 1, method="ML", control=list(opt="optim"), data=AllSoc)
randomIntercept<-lme(log(DENSITY) ~ 1, random=~1|ID,  method="ML", control=list(opt="optim"), data=AllSoc)
glsm<-gls(log(DENSITY)~nppZ+biodivZ+pathosZ+nppZ:biodivZ+nppZ:pathosZ, method="ML", data=AllSoc)
TimeRI<-update(randomIntercept,.~.+nppZ+biodivZ+pathosZ+nppZ:biodivZ+nppZ:pathosZ)
TimeRS<-update(TimeRI,.~.+nppZ+biodivZ+pathosZ+nppZ:biodivZ+nppZ:pathosZ, random=~nppZ|ID)

ModelComp<-anova( glsm, TimeRI,TimeRS)
ModelComp
xtable(ModelComp)
RE<-random.effects(TimeRI)
plot(RE)
Int<-intervals(TimeRI)
Int

fit<-predict(TimeRI, interval = 'confidence')

DenBio.predict <- cbind(AllSoc, fit)

# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(DenBio.predict, aes(nppZ,log(DENSITY)))
p <- p + geom_point(shape=15,aes(color=factor(ID2), size=factor(ID2)))
#p <-p+scale_size_manual(values=c(3,2,2,4))
p <- p + geom_line(aes(nppZ, fit))+
  #p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.3)+
  #stat_function(fun=f, colour="Treat")+
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.5), colour = "black"), axis.title=element_text(size=18))+
  labs(x = "Net primary productivity", y="log Population density")+
  facet_wrap(~factor(ID2))
p

RS<-coef(TimeRI)
RS
xtable(RS)
summary(TimeRI)
summary(glsm)

##Interaction plots
predict2<-cbind(TimeRI$fitted,AllSoc)

library(latticeExtra) 

# Plot contours of fitted values
#Predicted population densities
data <- data.frame(NPP = predict2$nppZ, Density = predict2$ID) 
data$z <- with(data, predict2$biodivZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.58, 2.25)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.49)) +
  layer(panel.abline(-1.73, 0.93))+
  layer(panel.abline(0.99, 0.93))+
  layer(panel.abline(4.29, 0.93))

##Real population densities

data <- data.frame(NPP = predict2$nppZ, Density = log(predict2$DENSITY)) 
data$z <- with(data, predict2$biodivZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.58, 2.25)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.49)) +
  layer(panel.abline(-1.73, 0.93))+
  layer(panel.abline(0.99, 0.93))+
  layer(panel.abline(4.29, 0.93))

#NPP Vs. Density with Pathogen contours

data <- data.frame(NPP = predict2$nppZ, Density = predict2$ID) 
data$z <- with(data, predict2$pathosZ) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP* Density|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","red"))(1e3),
          xlim=c(-1.58,2.25)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.89)) +
  layer(panel.abline(-1.73, 0.93))+
  layer(panel.abline(0.99, 0.93))+
  layer(panel.abline(4.29, 0.93))

##Species richness vs. NPP with Density Contours by ID group 0=HG, 1=Agg, 2=IND

data <- data.frame(NPP = predict2$nppZ, Biodiversity = predict2$biodivZ) 
data$z <- with(data, predict2$ID) 

# Attributes of level plot axis, etc
p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5, col='black'), height=0.75)
x.scale <- list(cex=2, alternating=1)
y.scale <- list(cex=2, alternating=1)
x.label<-list(cex=2, alternating=1)
y.label<-list(cex=2, alternating=1)
##Level plot
levelplot(z ~  NPP*Biodiversity|factor(predict2$ID2), data, 
          panel = panel.levelplot.points, cex = 1, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale), xlab=x.label, ylab=y.label, colorkey=ckey, col.regions = colorRampPalette(c("yellow", "orange","darkred"))(1e3),
          xlim=c(-1.6,2.3), ylim = c(-1.5,2.9)) + 
  layer_(panel.2dsmoother(..., n = 100))+
  layer(panel.abline(v = 0.39, h=1))





