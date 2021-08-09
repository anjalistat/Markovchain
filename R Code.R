install.packages("msm")
library(msm)
library(class)
library(stats)
library(MASS)
rim<-read.csv("D:/Data/Rim_Project/rim.csv",header=T, na.strings = "NA",fill=TRUE)
print(rim[1:10,])

# A frequency table counting the number of times each pair of states were observed

rs<-statetable.msm(State,data=rim)
rs
ti<- aggregate(Wi_Hr ~ State, data=rim, FUN=sum)
ti
q<-rs/ti[,2]
q
p<-0
for(i in 1:15){
  p[i]<-q[i,i]
  q[i,i]<--(sum(q[i,])-p[i])
  i=i+1
}

#initial transition intensitiy matrix

qmat<-q



#crude initial values for transition intensities by assuming that 
#the data represent the exact transition times of the Markov process. 

qmat.crude<-crudeinits.msm(State ~ Time,subject=Ref, data=rim, qmatrix=qmat)


#Fit a continuous-time Markov or hidden Markov multi-state model by maximum likelihood
#Observations of the process are made at exact times of transition between states

t<-rim$Time
rim.msm<-msm (State ~Time,data = rim, qmatrix=qmat,opt.method="optim",exacttimes=TRUE,cl = 0.95,fixedpars = TRUE,pci=t,hessian=TRUE,analyticp=FALSE,use.deriv=TRUE,control=list(fnscale = 2500))
rim.msm

# Extract the estimated transition intensity matrix. 

rim.A<-qmatrix.msm(rim.msm)
rim.A


#Extract estimated transition probability matrix from a fitted multi-state model for a given time interval

rim.p<-pmatrix.msm(rim.msm)
rim.p

# Mean sojourn times from a multi-state model

est.rim<-sojourn.msm(rim.msm)
est.rim

qmat.rim<-read.csv("D:/RIM/qmat.csv",header=F)


# Simulation of states and time on the basis of continuous time multi state Markov Jump process.


qmat.rim<-data.matrix(qmat.rim)

sim.rim<-sim.msm(qmat.rim, maxtime=2928, obstimes=0, start=1,mintime=2184)

sim<-data.frame(sim.rim$times,sim.rim$states)

write.table(sim,file = "D:\\RIM/sim.rim1.csv",sep = ",")

