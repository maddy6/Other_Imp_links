# Other_Imp_links

https://github.com/safer41/aKountable

https://safer41.github.io/

https://github.com/safer41/aKountable

https://github.com/bigchaindb/privacy-protocols


setwd("E:/Users/mayur.v/Documents/Index_Method/New-Dataset-master/New-Dataset-master/")
data1<-read.csv("Bitcoin.csv")
data2<-read.csv("Bitcoindark.csv")
data3<-read.csv("Bitshares.csv")
data4<-read.csv("Blocknet.csv")
data5<-read.csv("Dash.csv")
data6<-read.csv("Decred.csv")
data7<-read.csv("Digibyte.csv")
data8<-read.csv("Digixdao.csv")
data9<-read.csv("Dogecoin.csv")
data10<-read.csv("Ethereum.csv")

data11<-read.csv("Ethereum-Classic.csv")
data12<-read.csv("Factom.csv")
data13<-read.csv("Gamecredits.csv")
data14<-read.csv("Lisk.csv")
data15<-read.csv("Litecoin.csv")
data16<-read.csv("Maidsafecoin.csv")
data17<-read.csv("Monero.csv")
data18<-read.csv("Nav_coin.csv")
data19<-read.csv("Nem.csv")
data20<-read.csv("NXT.csv")

data21<-read.csv("PIVX.csv")
data22<-read.csv("Ripple.csv")
data23<-read.csv("Siacoin.csv")
data24<-read.csv("Steem.csv")
data25<-read.csv("Stellar.csv")

data26<-read.csv("Stratis.csv")
data27<-read.csv("Syscoin.csv")
data28<-read.csv("Vertcoin.csv")
data29<-read.csv("Waves.csv")


#dim(data9)

data_final<-data.frame(data1$Date,data1$Market.Cap,data2$Market.Cap,data3$Market.Cap,
                       data4$Market.Cap,data5$Market.Cap,data6$Market.Cap,data7$Market.Cap,
                       data8$Market.Cap,data9$Market.Cap,data10$Market.Cap,data11$Market.Cap,
                       data12$Market.Cap,data13$Market.Cap,data14$Market.Cap,data15$Market.Cap,
                       data16$Market.Cap,data17$Market.Cap,data18$Market.Cap,data19$Market.Cap,
                       data20$Market.Cap,data21$Market.Cap,data22$Market.Cap,data23$Market.Cap,
                       data24$Market.Cap,data25$Market.Cap,data26$Market.Cap,data27$Market.Cap,
                       data28$Market.Cap,data29$Market.Cap)
dim(data_final)
#View(data_final)
#dat<-data.frame(data1$Date,data1$Market.Cap,data2$Market.Cap)
                       
colnames(data_final)<-c("Date","Bitcoin", "Bitcoindark", "Bitshares", "Blocknet","Dash",
                        "Decred", "Digibyte", "Digixdao", "Dogecoin", "Ethereum",
                        "Ethereum_classic","Factom","Gamecredits","Lisk","Litecoin",
                        "Maidsafecoin","Monero","Nav_coin","Nem","NXT","PIVX","Ripple",
                        "Siacoin","Steem","Stellar","Stratis","Syscoin","Vertcoin","Waves")


head(data_final)
dim(data_final)

#######################

data_final[,2:30] = levels(droplevels(data_final[2:30]))

head(data_final[2:30])
data_final[2:30] <- lapply(data_final[2:30], function(x) as.numeric(as.character(x)))


###### Sum of market capitalization by date#
#dat = transform(dat, Marketcap_sum=rowSums(dat[,-1]))
data_final[2:30] <- lapply(data_final[2:30], as.numeric)

for (column in 2:30) {
  data_final[, column] <- as.numeric(data_final[, column])
}

data_final <- droplevels(data_final[,-1])
typeof(data_final$Bitcoin)

y = droplevels(data1$Market.Cap)
head(s)
s = levels(droplevels(data1$Market.Cap))
s1 = levels(droplevels(data1$Market.Cap))
s1[1]
s = as.character(as.numeric(s))

data_final = transform(data_final, Marketcap_sum=rowSums(data_final[,-1]))
head(data_final)
###############################################


colnames(Final_df5)<-c("Index_Value")
plot(Final_df5$Index_Value,type = "l",col = "green")
