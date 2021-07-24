install.packages('twitteR')
library(twitteR)
install.packages('RCurl')
install.packages('ROAuth')
install.packages('ggplot2')
install.packages('SnowballC')
install.packages('tm')
install.packages('stringr')
install.packages('wordcloud')
install.packages('dplyr')
install.packages('syuzhet')


apikey<-"XUeKeSl2ppFW8SKFWBukYKpQB"
apisecret<-"4vmndHGEnzydqLjXfOftab4imbWWGpGSAhvFgLHzZPj0xEQUj6"
accesstoken<-"3182619649-q73yaRWTM2GZ9Kt6NwilOLtgvzqwNDHleSgh2Vp"
access_secret<-"qPNhDGDadEpKc1C29Sb03b6Cuw7KB2nTEyEfoJIf3llIT"
setup_twitter_oauth(apikey,apisecret,accesstoken,access_secret)

mamta_tweet=searchTwitter("@MamataOfficial",n=4000,since="2021-03-30",until="2021-04-05")
mamta_tweet.df=twListToDF(mamta_tweet)
head(mamta_tweet.df)

mamta_tweet.df$text=gsub("&amp","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("@\\w+","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("[[:punct:]]","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("[[:digit:]]","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("http\\w+","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("[\t]{2,}","",mamta_tweet.df$text)
mamta_tweet.df$text=gsub("^\\s+|\\s+$","",mamta_tweet.df$text)

mamta_tweet.df$text<-iconv(mamta_tweet.df$text,"UTF-8","ASCII",sub="")
install.packages("plotly")

emotions<-get_nrc_sentiment(mamta_tweet.df$text)
emo_bar=colSums(emotions)
emo_sum=data.frame(count=emo_bar,emotion=names(emo_bar))
emo_sum$emotion=factor(emo_sum$emotion,levels = emo_sum$emotion[order(emo_sum$count,decreasing = TRUE)])

p<-plot_ly(emo_sum,x=~emotion,y=~count,type="bar",color=~emotion)%>%
  layout(xaxis=list(title=""),title="EMOTION FOR MAMTA BANERJEE")


p

wordcloud_tweet=c(
  paste(mamta_tweet$text[emotions$anger>0],collapse = ""),
  paste(mamta_tweet$text[emotions$anticipation>0],collapse = ""),
  paste(mamta_tweet$text[emotions$disgust>0],collapse = ""),
  paste(mamta_tweet$text[emotions$fear >0],collapse = ""),
  paste(mamta_tweet$text[emotions$joy>0],collapse = ""),
  paste(mamta_tweet$text[emotions$sadness>0],collapse = ""),
  paste(mamta_tweet$text[emotions$trust>0],collapse = ""),
  paste(mamta_tweet$text[emotions$suprise>0],collapse = "")
  )


Suvend_tweet=searchTwitter("@SuvenduWB",n=4000,since="2021-03-30",until="2021-04-05")
Suvend_tweet.df=twListToDF(Suvend_tweet)
head(Suvend_tweet.df)

Suvend_tweet.df$text=gsub("&amp","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("@\\w+","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("[[:punct:]]","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("[[:digit:]]","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("http\\w+","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("[\t]{2,}","",Suvend_tweet.df$text)
Suvend_tweet.df$text=gsub("^\\s+|\\s+$","",Suvend_tweet.df$text)

Suvend_tweet.df$text<-iconv(Suvend_tweet.df$text,"UTF-8","ASCII",sub="")


emotions<-get_nrc_sentiment(Suvend_tweet.df$text)
emo_bar=colSums(emotions)
emo_sum=data.frame(count=emo_bar,emotion=names(emo_bar))
emo_sum$emotion=factor(emo_sum$emotion,levels = emo_sum$emotion[order(emo_sum$count,decreasing = TRUE)])

s<-plot_ly(emo_sum,x=~emotion,y=~count,type="bar",color=~emotion)%>%
  layout(xaxis=list(title=""),title="EMOTION FOR Suvend Adhikari ")



s
p