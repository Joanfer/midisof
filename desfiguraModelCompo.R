harmonia<-function(intervals) {intervals2<-24*intervals/max(intervals);
#xx<-c(0, 4, 7, 12, 19, 22, 24); yy<-c(0, 5, 7, 14, 17, 22, 24 )
#xx3<-xx^3; xx2<-xx^2; xx4<-xx^4; 
#summary(lm(yy~xx4+xx3+xx2+xx))

intervals2<- matrix( rep(intervals2, 5), nrow=length(intervals), ncol=5);
intervals2[,1]<-intervals2[,1]^4; 
intervals2[,2]<-intervals2[,2]^3;
intervals2[,3]<-intervals2[,3]^2; 
intervals2[,5]<-1; print(intervals2);  

intervalsNous<-intervals2 %*% (c(0.0002275, -0.0095820,0.1092033, 0.7638574,0.0997800 ));return((max(intervals))*intervalsNous/24)}

nota<-function(x) return(24*(x+1)/28)

x.fig<-c(0.25, 0.5, 1, 2); y.fig<-c(1/6, 2/3, 1.5, 3)
x2.fig<-x.fig^2; x3.fig<-x.fig^3; model.fig<-lm(y.fig~x.fig+x2.fig+x3.fig); summary(model.fig)

figura<-function(x,b0,b1,b2,b3) {y<-ifelse(x<=2, (x^3)*(b3)+(x^2)* b2+x*b1+b0, 4-( ((2-(x-2))^3)*(b3)+((2-(x-2))^2)*(b2)+(2-(x-2))*b1+b0)); return(y)}
figura(3.5, 0, 0.6743,1.2715, -0.4295 )
figura(2, 0, 0.6743,1.2715, -0.4295 )
