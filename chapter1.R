Carbon<-read.table("http://stat4ds.rwth-aachen.de/data/Carbon.dat", header=TRUE)
Carbon
breaks<- seq(2.0,10.0,by=1.0) # frequency distribution intervals od 2 do 10 wielkośc 1
freq<-table(cut(Carbon$CO2,breaks,right = FALSE)) # with right=FALSE, right-most value not included in interval
freq


cbind(freq,freq/nrow(Carbon))
#Frequency distribution of CO2 values,showing
# intervals of CO2 values, followed by frequency
# (freq) and proportion, which divides freq by
# nrow(Carbon) = number of rows indata frame=31


hist(Carbon$CO2,xlab = "CO2", ylab="Proportion",freq = FALSE)
plot(density(Carbon$CO2))
summary(Carbon$CO2)
boxplot(Carbon$CO2,horizontal = TRUE)


summary(Carbon$CO2) # 1stQu=lowerquartile, 3rd Qu=upperquartile

c(mean(Carbon$CO2),sd(Carbon$CO2),quantile(Carbon$CO2,0.90))

boxplot(Carbon$CO2,xlab="CO2values",horizontal=TRUE)

#murders comparison
Crime<-read.table("http://stat4ds.rwth-aachen.de/data/Murder2.dat", header=TRUE)
boxplot(Crime$murder~Crime$nation,xlab="Murderrate",horizontal=TRUE)
#applies summary to murder by nation
tapply(Crime$murder,Crime$nation,summary)

#Filter data 
CanadaData<-subset(Crime, nation=="Canada")

boxplot(CanadaData$murder,horizontal = TRUE)

summary(CanadaData$murder)
plot(density(CanadaData$murder))



GS<-read.table("http://stat4ds.rwth-aachen.de/data/Guns_Suicide.dat", header=TRUE)
Guns<-GS$guns;Suicides<-GS$suicide
plot(Guns,Suicides)#scatterplotwitharguments x, y
cor(Guns,Suicides) # correlation

summary(lm(Suicides~Guns)) #lm(y~ x) is"linearmodel"for


PID<-read.table("http://stat4ds.rwth-aachen.de/data/PartyID.dat", header=TRUE)
table(PID$race,PID$id)#formscontingencytable (not shownhere;see Table 1.1)
options(digits=2)
prop.table(table(PID$race,PID$id),margin=1)#Formargin=1, proportions # sumto1.0within rows
mosaicplot(table(PID$race,PID$id))#graphicalportrayalof cell sizes that's fucked btw


# Ex 1.42

#a)
#y = number of hours of physical exercise in the past week having ¯y < s. / mean < sd
set.seed(5793645)   # Set random seed for reproducibility

#Note that we are specifying a mean of 2 and a standard deviation of 5 in the following syntax:
x1 <- rnorm(20, 2, 5)
summary(x1)
sd(x1)
boxplot(x1,xlab="mean < sd",horizontal = TRUE)
plot(density((x1)))
# observation 1 : SD larger than mean makes larger range 
# no observation on shape


