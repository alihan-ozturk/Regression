



x <- rnorm(50)
mean(x)
sd(x)
var(x)
median(x)
quantile(x)
median(x)==quantile(x)


##install.packages("ISwR")
library(ISwR)
attach(juul)
mean(igf1)
mean(igf1,na.rm=T)

sum(igf1)
sum(!is.na(igf1))

summary(igf1)
summary(juul)
detach(juul)


load("L1.data")
head(L1, 8)
tail(L1, 8)
sum(L1)
mean(L1)
var(L1)
sd(l1)
range(L1)
quantile(L1)
quantile(L1)[3]
median(L1)
quantile(L1,seq(0,1,0.1))
summary( L1 )


##install.packages("DescTools")
library(DescTools)
Mode(L1)

L3<-c(11,11,11,11,9,9,9,9,5,5,2,7,10,15,7,2,4,14)
Mode(L3)

##anakütle için varyans
p.var<- function(x) { out<-var(x)*(length(x)-1)/length(x)
return(out)
}
p.var(L1)

##anakütle için standart sapma
p.sd <- function(x) { out<-sqrt(p.var(x))
return(out)
}
p.sd(L1)


##basıklık ve çarpıklık
install.packages("moments")
library(moments) 

par(mfrow=c(2,2))

a1<-c(runif(150,0,100),rnorm(150,50,5))
hist(a1,freq = FALSE,breaks = 10,ylim=c(0,0.025),
     xlab = round(c(skewness(a1),kurtosis(a1)),3))
points(density(a1))

a2<-c(runif(200,0,100),rnorm(100,30,10))
hist(a2,freq = FALSE,breaks = 10,ylim=c(0,0.025),
     xlab = round(c(skewness(a2),kurtosis(a2)),3))
points(density(a2))

a3<-c(runif(200,0,100),rnorm(100,50,35))
hist(a3,freq = FALSE,breaks = 10,ylim=c(0,0.025),
     xlab = round(c(skewness(a3),kurtosis(a3)),3))
points(density(a3))

a4<-c(runif(200,0,100),rnorm(100,70,10))
hist(a4,freq = FALSE,breaks = 10,ylim=c(0,0.025),
     xlab = round(c(skewness(a4),kurtosis(a4)),3))
points(density(a4))


boxplot(a1,horizontal = T)
boxplot(a2,horizontal = T)
boxplot(a3,horizontal = T)
boxplot(a4,horizontal = T)


par(mfrow=c(1,1))




#geometrik ortalama ve harmonik ortalama
library("DescTools")  
geometric.mean(L1)
harmonic.mean(L1)

#frekans tablosu oluşturma
frkns<-c(1.60,1.70,1.70,1.80,1.90,1.90,1.90,2,2.10,2.40,2.40,
         2.40,2.40,2.45,2.50,2.50,2.60,2.60,2.70,2.70,2.70,2.70,
         2.75,2.80,2.80,2.90,3,3.10,3.20,3.40)

table(frkns)

data.frame(table(frkns))

##L1 verisi için sınıflandırılmış frekans tablosu oluşturma
L1
min(L1)
max(L1)
low_val <- 30
high_val <- 110
step_val <- 10

##sınıf aralığı sqrt(n) <= k ya da log(n) şeklinde de bulunabilir.
x_breaks <- seq(low_val, high_val, step_val)

x_breaks

x_mid <- seq( low_val+step_val/2,
              high_val-step_val/2, step_val )
x_mid  

x <- cut(L1, breaks=x_breaks,right=FALSE)
x

y <- table(x)

y

df <- data.frame(y)
df

df$midpnt <- x_mid 
df

rf <- round(df$Freq/length(L1),2)
rf

df$relFreq <- rf
df  

cs <- cumsum( df$Freq )
cs
df$cumul <- cs             # append cumulative sum
n <- length( L1 )
rcf <- cs/n
rcf
df$rel_cumul <- round(rcf,2)# append rel cumul sum

View(df)


pie(df$Freq,df$x,col = rainbow(7),radius = 1.1)


##sınıflandırılmış frekans tablosu için fonksiyon
collate3 <- function( lcl_list, use_low=NULL, use_width=NULL, ...)
{ 
  lcl_real_low <- min( lcl_list )
  lcl_real_high <- max( lcl_list ) 
  lcl_size <- length(lcl_list)
  
  if( is.null(use_low) | is.null(use_width) )
  {
    
    cat(c("The lowest value is ",lcl_real_low ,"\n"))
    cat(c("The highest value is ", lcl_real_high,"\n" ))
    suggested_width <- (lcl_real_high-lcl_real_low) / 10 
    cat(c("Suggested interval width is ", suggested_width,"\n" ))
    cat(c("Repeat command giving collate3( list, use_low=value, use_width=value)","\n"))
    cat("waiting...\n")
    return( "waiting..." )    
  }
  use_num_bins <- floor( (lcl_real_high - use_low)/use_width)+1
  lcl_max <- use_low+use_width*use_num_bins
  lcl_breaks <- seq(use_low, lcl_max, use_width)
  lcl_mid<-seq(use_low+use_width/2, lcl_max-use_width/2, use_width)
  
  lcl_cuts<-cut(lcl_list, breaks=lcl_breaks, ...)
  lcl_freq <- table( lcl_cuts )
  lcl_df <- data.frame( lcl_freq )
  lcl_df$midpnt <- lcl_mid
  lcl_df$relfreq <- lcl_df$Freq/lcl_size
  lcl_df$cumulfreq <- cumsum( lcl_df$Freq )
  lcl_df$cumulrelfreq <- lcl_df$cumulfreq / lcl_size
  lcl_df$pie <- round( 360*lcl_df$relfreq, 1 )
  
  lcl_df
}


#collate fonksiyonu için örnekler
collate3( L1 )
collate3( L1, 30, 10 )
collate3( L1, 30, 10,right=FALSE )

load("L2.data")

collate3(L2)
collate3( L2, 150, 10, right=FALSE)


##istatistiksel grafikler
attach(energy)
energy
lean <- expend[stature=="lean"]
obese <- expend[stature=="obese"]

par(mfrow=c(1,2))
hist(lean,breaks=5,xlim=c(6,11),ylim=c(0,4),col="blue")
hist(obese,breaks=5,xlim=c(8,13),ylim=c(0,4),col="red")
par(mfrow=c(1,1))
boxplot(expend ~ stature,horizontal = TRUE)


attach(secretin)
secretin
table(person)
table(person,time)
table(person,repl)
table(time,time.comb)


##matris oluşturma
m1 <- matrix(c(652,1537,598,242,150,172,120,200,218,327,106,67),
             nrow=3,byrow=T)
rownames(m1) <- c("milka","toblerone","ülker")
colnames(m1) <- c("a","b","c","d")
names(dimnames(m1)) <- c("markalar","şehirler")
m1

##kategorik veriler için sütun grafiği
par(mfrow=c(2,2))
barplot(m1, col="blue")
barplot(t(m1), col="red")
barplot(t(m1), col="green", beside=T)
barplot(prop.table(t(m1),2), col="white", beside=T)
par(mfrow=c(1,1))

barplot(prop.table(t(m1),2),beside=T,
        legend.text=colnames(m1),
        col=c("white","grey80","grey50","black"))



##dal-yaprak grafiği
library(datasets)
trees$Volume
stem(trees$Volume)

stackloss$Air.Flow
stem(stackloss$Air.Flow)

faithful$eruptions
stem(faithful$eruptions) 


##normal dağılım için giriş
x1<-rnorm(150,mean=100,sd=15)
shapiro.test(x1)
hist(x1,freq = FALSE,breaks = 20)
points(density(x1))
rug(x1)
boxplot(x1,horizontal = TRUE)
qqnorm(x1)
qqline(x1)



x2<-runif(150,min = 50,max = 150)
shapiro.test(x2)
hist(x2,freq = FALSE,breaks = 20)
points(density(x2))
rug(x2)
boxplot(x2,horizontal = TRUE)
qqnorm(x2)
qqline(x2)

x3<-L2
x3
shapiro.test(x3)
hist(x3,freq = FALSE,breaks = 20)
points(density(x3))
rug(x3)
boxplot(x3,horizontal = TRUE)
qqnorm(x3)
qqline(x3)

##sınıflandırılmış veri için ki-kare uyum iyiliği testi(normal dağılım)
load("L1.data")

L1
low_val <-30
high_val <-110
step_val <-10
x_breaks <-seq(low_val,high_val,step_val)

x_mid <-seq(low_val+step_val/2,high_val-step_val/2,step_val)

x<-cut(L1, breaks=x_breaks,right=FALSE)

y<-table(x)

df<-data.frame(y)
df

##ki kare testi test istatistiği 
##{Toplamı((Gözlenen değer-Beklenen değer)^2/beklenen değer)}

mean<-sum(x_mid*df$Freq/sum(df$Freq))
sd<-sqrt(sum(df$Freq*(x_mid^2)/sum(df$Freq))-mean^2)
mean
sd

CDF<-c(0,pnorm(x_breaks,mean,sd),1)
probs.null<-diff(CDF,lag = 1)

N<-sum(df$Freq)

expect.counts<-round(N*probs.null,2)
obs<-c(0,df$Freq,0)
expect.counts
obs

##Eğer 5ten küçük beklenen değer varsa en yakın komşusuna eklenir

i<-1
n<-3
for (i in i:n) {
  if(expect.counts[i]<5){
    expect.counts[i+1]<-expect.counts[i]+expect.counts[i+1];
    obs[i+1]<-obs[i]+obs[i+1]
  }
  
}
i<-1
n<-10
for (i in n:8 ) {
  if(expect.counts[i]<5){
    expect.counts[i-1]<-expect.counts[i]+expect.counts[i-1];
    obs[i-1]<-obs[i]+obs[i-1]
  }
}


new.exp<-round(expect.counts[expect.counts>5],1)
new.obs<-round(obs[expect.counts>5],1)
test.stat <- sum((new.obs-new.exp)^2/new.exp)

test.stat
p<-pchisq(test.stat, df=6-1-2, lower=F)
alpha<-0.05
p
if(p > alpha){
  print("h0 accept")
} else {
  print("ho reject")
}


##install.packages("nortest")
library(nortest)
pearson.test(df$Freq,8,adjust = TRUE)##??


##anakütle için varyans ve standart sapma
library(readr)
honk <- read_delim("C:/Users/aliha/OneDrive/Masaüstü/honk.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
View(honk)

p.var<- function(x) { out<-var(x)*(length(x)-1)/length(x)
return(out)
}
p.var(honk$height)
p.var(honk$weigh)

p.sd <- function(x) { out<-sqrt(p.var(x))
return(out)
}


p.sd(honk$height)
p.sd(honk$weigh)


##kolmogorov normallik testi(anakütle için)
##anakütlenin ortalaması ve standart sapması bilinmelidir

#ks.test(honk$height, "pnorm", mean(honk$height), sd=(anakütle standart sapması),
#        alternative = "two.sided", exact=T)
#ks.test(honk$weigh, "pnorm", mean(honk$weigh), sd=(anakütle standart sapması),
#      alternative = "two.sided", exact=T)





##lilliefors normallik testi fonksiyonu ve örnekler
##F(x)=P(X<=x) boş hipotezinde belirtilen birikimli dağılım fonksiyonu 
##olmak üzere her x için F(x)=F0(x) eşitliğinin doğru olup olmadığı sınanır.
##S(xi) = (xi değerine eşit ya da daha küçük birimli değerlerin sayısı)/n 
##ks.test.stat kesikli ise (D)= Enbüyük{|S(xi)-F0(xj)|} 
##ks.test.stat sürekli ise (D)= Enbüyük{|S(xi)-F0(xj)|,S(xj-1)-F0(xj)|} 
ks.test.stat <- function(x){
  xi<-sort(unique(x))
  frek<-as.numeric(table(x))
  cumu<-cumsum(frek)
  Sx<-round(cumu/sum(frek),5)
  xifi<-xi*frek
  zi<-round((xi-mean(x))/sd(x),5)
  f<-round(pnorm(xi,mean(x),sd(x),lower.tail = T),5)
  d1<-abs(Sx-f)
  sx_1<-c(0,Sx[1:length(Sx)-1])
  d2<-abs(sx_1-f)
  out<-max(d1,d2)
  return(out)
}



lilliefors.normal.pval <- function(x, nsimul=10000){
  mu.hat<-mean(x); sigma.hat <- sd(x); n <- length(x)
  T1 <- ks.test.stat((x-mu.hat)/sigma.hat)
  mymat<-matrix(rnorm(nsimul*n),nrow=nsimul, ncol=n)
  T1distn <- apply(mymat,1,ks.test.stat)
  pval <- mean(T1 < T1distn)
  out <- list(test.stat=T1, p.value=pval)
  return(out)
}


x<-c(1.60,1.70,1.70,1.80,1.90,1.90,1.90,2,2.10,2.40,
     2.40,2.40,2.40,2.45,2.50,2.50,2.60,2.60,2.70,
     2.70,2.70,2.70,2.75,2.80,2.80,2.90,3,3.10,3.20,3.40)
x<-L1
x<-L2

lilliefors.normal.pval(x)


##sayı üretip lilliefors fonksiyonunu çalıştırma
my.data.t <- rt(30,df=4)
my.data.norm <- rnorm(30, mean=30, sd=3)
my.data.exp <- rexp(30, rate=1)


lilliefors.normal.pval(my.data.t)
lilliefors.normal.pval(my.data.norm)
lilliefors.normal.pval(my.data.exp)


##diğer normallik testleri

shapiro.test(x)
##install.packages("nortest")
library(nortest)
ad.test(x)           ##Anderson – Darling Testi
cvm.test(x)          ##Cramer- Von Mises Testi
lillie.test(x)       ##Kolmogorov – Smirnov Tek Ornek Testi
sf.test(x)           ## Shapiro – Francia Testi

