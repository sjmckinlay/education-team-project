#K. Jones and SJ McKinlay
#TMATH 410 FINAL PROJECT R SCRIPT
#Winter Quarter, 2020



crime.df=read.csv(file.choose()) # reading in the file, and creating data frame
crimeNoDC.df=crime.df[-9,] # removing the line from the dataset that contained
# Washington DC
head(crimeNoDC.df) # first six lines of file
dim(crimeNoDC.df) # dimensions of file

par(mfrow=c(2,2)) #this sets up the parameters for the graphs 
#png(file="pngTry1.png",height=360,width=960)
par(mfrow=c(2,2),mar=c(3.65,3.35,0.5,0.5),mgp=c(2.5,0.5,0),las=1) 
#this sets up the parameters for the graphs 
# these are scatterplots for predictor variables plotted against the response
# variable
plot(crimeNoDC.df$People...Sq..Mile..2010. ,crimeNoDC.df$Crimes.Per.1000.People ,
     xlab="Population Density\n(people per sq. mile)", 
     ylab="Crime Rate (x1000)",pch=16)
plot(crimeNoDC.df$Per.Pupil.Amount.Spent...1. ,crimeNoDC.df$Crimes.Per.1000.People ,
     xlab="Annual Amount spent\non Education per-pupil ", 
     ylab="Crime Rate (x1000)",pch=16)
plot(crimeNoDC.df$Region ,crimeNoDC.df$Crimes.Per.1000.People ,
     xlab="Region", 
     ylab="Crime Rate (x1000)",pch=16)
plot(crimeNoDC.df$Unemployment.. ,crimeNoDC.df$Crimes.Per.1000.People ,
     xlab="Unemployment Rate", 
     ylab="Crime Rate (x1000)",pch=16)
#dev.off()

par(mfrow=c(2,3)) #this sets up the parameters for the graphs 
par(mfrow=c(2,3),mar=c(4,3.85,.5,.5),mgp=c(2.85,0.5,0),las=1) #this sets up the 
# parameters for the graphs 
# these scatterplots show the predictor variables being plotted against each other
plot(crimeNoDC.df$People...Sq..Mile..2010.,crimeNoDC.df$Per.Pupil.Amount.Spent...1. ,
     xlab="Population Density (sq. mile)", 
     ylab="Annual Education Spending (per-pupil)",pch=16)
plot(crimeNoDC.df$Region ,crimeNoDC.df$People...Sq..Mile..2010. ,
     xlab="Region", 
     ylab="Population Density (sq. mile)",pch=16)
plot(crimeNoDC.df$People...Sq..Mile..2010. ,crimeNoDC.df$Unemployment.. ,
     xlab="Population Density (sq. mile)", 
     ylab="Unemployment Rate",pch=16)
plot(crimeNoDC.df$Per.Pupil.Amount.Spent...1. ,crimeNoDC.df$Unemployment.. ,
     xlab="Annual Education Spending \n(per-pupil)", 
     ylab="Unemployment Rate",pch=16)
plot(crimeNoDC.df$Region ,crimeNoDC.df$Per.Pupil.Amount.Spent...1. ,
xlab="Region", 
ylab="Annual Education Spending (per-pupil)",pch=16)
plot(crimeNoDC.df$Region ,crimeNoDC.df$Unemployment.. ,
     xlab="Region", 
     ylab="Unemployment Rate",pch=16)

crimeNoDC.lm=lm(Crimes.Per.1000.People~People...Sq..Mile..2010.+Per.Pupil.Amount.Spent...1.+Region+Unemployment..,
                data=crimeNoDC.df) # this creates the linear model
summary(crimeNoDC.lm) # this shows the summary for the linear model

# below are individual/simple linear models to examine the relationship between
# the predictor variables and the response variable, and the predictor variables
# and each other
crim2=lm(Crimes.Per.1000.People ~ Per.Pupil.Amount.Spent...1., data = crimeNoDC.df)
summary(crim2)
crim3=lm(Crimes.Per.1000.People ~ People...Sq..Mile..2010. , data = crimeNoDC.df)
summary(crim3)
crim4=lm(Crimes.Per.1000.People ~ Region , data = crimeNoDC.df)
summary(crim4)
crim5=lm(Crimes.Per.1000.People ~ Unemployment.. , data = crimeNoDC.df)
summary(crim5)
crim6=lm(Per.Pupil.Amount.Spent...1. ~ People...Sq..Mile..2010. , data = crimeNoDC.df)
summary(crim6)
crim7=lm( People...Sq..Mile..2010.~ Region , data = crimeNoDC.df)
summary(crim7)
crim8=lm(Unemployment..  ~ People...Sq..Mile..2010. , data = crimeNoDC.df)
summary(crim8)
crim9=lm(Unemployment..  ~ Per.Pupil.Amount.Spent...1. , data = crimeNoDC.df)
summary(crim9)
crim10=lm(Per.Pupil.Amount.Spent...1.  ~ Region , data = crimeNoDC.df)
summary(crim10)
crim11=lm( Unemployment.. ~ Region , data = crimeNoDC.df)
summary(crim11)

crimeNoDC.infl=influence(crimeNoDC.lm) 
crimeNoDC.infl$hat[1:3] # first three leverage values
crimeNoDC.lm.sum=summary(crimeNoDC.lm) # this creates an object that stores the results
# of the summary of the lm object
attributes(crimeNoDC.lm.sum) # this allows us to look at the attributes of the 
# new object
crimeNoDC.stdRes=crimeNoDC.lm$residuals/(crimeNoDC.lm.sum$sigma*sqrt(1-crimeNoDC.infl$hat))
# We can use this to calculate our standardized residuals
crimeNoDC.stdRes=rstandard(crimeNoDC.lm)  # The function R standard also calculates 
# the standardized residuals directly
boxplot(crimeNoDC.stdRes,ylab=" Standardized Residual Values",col="grey") # this
# creates a boxplot for the standardized residuals
qqnorm(crimeNoDC.stdRes,ylab="Standardized Residual Values") # this creates a QQ-Plot
# of the standardized residuals
qqline(crimeNoDC.stdRes, col = 2,lwd=1,lty=1) # this creates the line for the QQ-Plot


par(mfrow=c(2,3)) # this sets the parameters for the graphs
par(mfrow=c(2,3),mar=c(6,6,1,1),mgp=c(3.5,1.5,1),las=1)
# this sets the parameters for the graphs 
# this creates scatter plots of the standardized residual values against each
# of the predictor variables and the y fitted values
plot(crimeNoDC.df$People...Sq..Mile..2010.,crimeNoDC.stdRes,
     xlab="Population Density \n(sq. mile)", 
     ylab="Standardized \nResiduals",pch=16)
plot(crimeNoDC.df$Per.Pupil.Amount.Spent...1.,crimeNoDC.stdRes,
     xlab="Annual Education \n Spending (per-pupil)", 
     ylab="Standardized \nResiduals",pch=16)
plot(crimeNoDC.df$Region,crimeNoDC.stdRes,
     xlab="Region", 
     ylab="Standardized \nResiduals",pch=16)
plot(crimeNoDC.df$Unemployment..,crimeNoDC.stdRes,
     xlab="Unemployment Rate", 
     ylab="Standardized \nResiduals",pch=16)
plot(crimeNoDC.lm$fitted.values,crimeNoDC.stdRes,
     xlab="Y fitted Values", 
     ylab="Standardized \nResiduals",pch=16)

par(mfrow=c(2,2),mar=c(4,3.5,2,1.25),mgp=c(2.5,0.5,0),las=1)
plot(crimeNoDC.lm) # this is a built-in residual diagnostics tool 

# below are the linear models created to calculate VIF by hand. I did not know
# there was a built in way until after I had done this all. 

VIF1=lm( People...Sq..Mile..2010. ~ Region + Unemployment.. + Per.Pupil.Amount.Spent...1., data = crimeNoDC.df) 
summary(VIF1)
VIF2=lm( Unemployment.. ~ Region + People...Sq..Mile..2010. + Per.Pupil.Amount.Spent...1., data = crimeNoDC.df) 
summary(VIF2)
VIF3=lm( Per.Pupil.Amount.Spent...1. ~ Unemployment.. + People...Sq..Mile..2010. + Region, data = crimeNoDC.df) 
summary(VIF3)





