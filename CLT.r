#Sampling
n<-500
sampSize<-200
xbar <- rep(NA, n)
for (i in 1:n) {
  mysamp <- sample(x, size = sampSize)
  xbar[i] <- mean(mysamp)
}

#histogram of my sampled values 
mySD<-as.character( abs(as.integer((xbar - mean(xbar)) / sd(xbar) )))
myDF<-data.frame(xbar,mySD)
xAxis<-as.integer(max(abs(xbar)))
mu<-round(mean(xbar),2)
sd<-round(sd(xbar),2)
myBin<-sd/10
ggplot(myDF, aes(xbar)) +
  geom_histogram(aes(fill = mySD), binwidth = myBin, col="black", size=.1) +  # change binwidth
  labs(x="x", y="Frequency") + 
  labs(title="Histogram of my sampled values",
  subtitle=paste0(  "mean = ", mu, ", sd = ", sd, ", Sample size = ",sampSize,", nb of samples = ",n))+
  scale_x_continuous(breaks = seq(mu-sd*5, mu+sd*5, sd))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(expression(sigma)))+
  geom_density(aes(y=..count../90))
