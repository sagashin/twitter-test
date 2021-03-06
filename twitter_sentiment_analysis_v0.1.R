#install.packages("RMeCab", repos = "http://rmecab.jp/R", type = "source") 
#install.packages("base64enc")
#install.packages("twitteR")

#ライブラリの読み込み
library(RMeCab)
library(wordcloud)
library(RColorBrewer)
library("twitteR")
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(scales)
library(ggthemes)
library(reshape2)
library(base64enc)

##################################################################################################
#情報の入力
consumerKey <- "HKfGKdSWAfP5J3JmXpLtiFZOU"
consumerSecret <- "Q3seBCU6FTyQtpGDD2NVS6H7Fsa9EzGNLdD7BJ2LxRpldl20vz"
accessToken <- "106618656-qH4Htbn5vtUK9iL85xqzOJyQcUDR62gWDj0sHVG1"
accessSecret <- "NAj5f0eUsptu4aj0v5RCMFpXJSt5JhC0Y2sI30FguroeZ"

#処理の準備
#httr_oauth_chcheを保存
options(httr_oauth_cache = TRUE)
#認証情報の取得
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)
##################################################################################################

#キーワード入力
SearchWords <- c("アクサダイレクト OR アクサ損保 OR アクサ損害保険 OR アクサ")
#SearchWords <- c("東京海上")
TwGetDF <- twListToDF(searchTwitter(iconv(SearchWords,"CP932","UTF-8"), #検索キーワード
                                    n = 10000,resultType = "mixed" #取得するツイート数
                                    #since = YYYY-MM-DD #取得する期間
))




#windowsは下記コマンドで結果の文字化けを防ぐことができます。
res <- docMatrixDF(iconv(TwGetDF[, 1], from = "utf-8", to = "cp932"), pos = c("名詞", "形容詞", "動詞"))
res <- res[row.names(res) != "[[LESS-THAN-1]]", ] #[[LESS-THAN-1]]の削除
resc <- res[row.names(res) != "[[TOTAL-TOKENS]]", ]　#[[TOTAL-TOKENS]]の削除
########

###単語解析結果をデータフレーム化#####
AnalyticsFileDoc <- as.data.frame(apply(resc, 1, sum)) #単語の出現数を集計
WordFreq <- 30
AnalyticsFileDoc <- subset(AnalyticsFileDoc, AnalyticsFileDoc[, 1] >= WordFreq) #出現数で抽出
colnames(AnalyticsFileDoc) <- "出現数" #行名の設定
########

###タグクラウドのテキストの色を設定#####
Col <- c("#deb7a0", "#505457", "#4b61ba") #文字色の指定
########

###タグクラウドのプロット#####
#par(family = "HiraKakuProN-W3") #実行でMACの文字化け防止
wordcloud(row.names(AnalyticsFileDoc), AnalyticsFileDoc[, 1],scale = c(4, 1.5),
          random.order = F, rot.per = .09, random.color = TRUE, colors =  brewer.pal(8, "Dark2"))

# 単語感情極性対応表の取得###########################################################
pndic <- read.table("http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic",
                    sep = ":",stringsAsFactors = FALSE,
                    col.names = c("term", "kana", "pos", "value"),
                    colClasses = c("character", "character", "factor", "numeric"),
                    fileEncoding = "CP932")
                    #fileEncoding = "Shift_JIS")
#名詞＋品詞で複数の候補がある場合は平均値を採用
#「アイス」の読みとして「アイス」、「アイスホッケー」が割り当てられている例もある
pndic2 <- aggregate(value ~ term + pos, pndic, mean)

# pndic に登録されている品詞のみ抽出
pos <- unique(pndic2$pos)
#####################################################################################

TwGetDF$text <- iconv(TwGetDF$text, from = "utf-8", to = "cp932")
tweetDF <- docDF(TwGetDF, column = "text", type = 1, pos = pos)

tweetDF[1005:1010, 1:5]

# pndic に登録されている単語のみ抽出
tweetDF <- subset(tweetDF, TERM %in% pndic2$term)
# 単語感情極性スコアを付与
tweetDF <- merge(tweetDF, pndic2, by.x = c("TERM", "POS1"), by.y = c("term", "pos"))

test <- tweetDF[c("TERM","value")]

# 単語の出現回数にスコアを掛けて総和を取る
score <- colSums(tweetDF[4:(ncol(tweetDF) - 1)] * tweetDF$value)
# ポジティブツイート数
sum(score > 0)
sum(score < 0)
sum(score == 0)

#table(ifelse(pndic$value > 0, "positive",ifelse(pndic$value == 0, "neutral", "negative")))

m <- mean(score)
# 平均スコアでポジティブとネガティブを分離
tweetType <- factor(ifelse(score > m, "positive",ifelse(score == m, "neutral", "negative")),
                    levels = c("positive", "neutral", "negative"))
tweetType2 <- factor(ifelse(score > 0, "positive",ifelse(score == 0, "neutral", "negative")),
                     levels = c("positive", "neutral", "negative"))
table(tweetType)

TwGetDF$tweetType <- droplevels(tweetType)
TwGetDF$score <- score
TwGetDF$tweetType2 <- droplevels(tweetType2)

a <- is.na(TwGetDF$text)
#remove NAs
TwGetDF <- TwGetDF[!a,]

# 月を横軸にとってプロット

month <- month(as.POSIXlt(TwGetDF$created, format="%d/%m/%Y"))
day <- day(as.POSIXlt(TwGetDF$created, format="%d/%m/%Y"))

TwGetDF$mthday <- format(TwGetDF$created, format = "%y-%m-%d")

TwGetDF$day <- day(as.POSIXlt(TwGetDF$created, format="%d/%m/%Y"))

#month <- month(dmy(TwGetDF$created))

#qplot(day, data = TwGetDF,geom = "bar", fill = tweetType, position = "fill")
#qplot(day, data = TwGetDF,geom = "bar", fill = tweetType2, position = "fill")

#wordcloud(tweetDF$TERM, AnalyticsFileDoc[, 1], scale = c(3, .5),
#          random.order = F, rot.per = .1, random.color = TRUE,colors =  brewer.pal(8, "Dark2"))

dftest <- TwGetDF %>% group_by(mthday,day,tweetType) %>% 
        dplyr::summarise(n=n())

# Calculate the percentages

dftest = ddply(dftest, .(mthday), transform, percent = n/sum(n) * 100)

# Format the labels and calculate their positions
dftest$label = paste0(sprintf("%.0f", dftest$percent), "%")

ggplot(dftest,aes(mthday,n,fill=tweetType))+
        geom_bar(position = position_stack(),stat="identity")+
        geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4)
        
        geom_text(aes(label=n,group=mthday))
        
        geom_text(data = dftest, aes(x=mthday,y=n/max(n),label = paste0((n/100),"%")),
                  position = position_stack(vjust = 0.5))

        geom_text(data = dftest, aes(label = n,group=mthday),
                  position=position_dodge(width=0.9))+
        scale_y_continuous(labels = comma)+
        theme_hc()+scale_colour_hc()
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        stat_bin(aes(label = paste(scales::percent((..count..)/sum(..count..)))), vjust=1, geom="text")
        #coord_flip()+
        labs(
                title = "Premium STEC by Risk Category",
                subtitle = "From FY2014",
                caption = "Y-lim Unit: JPY, Data Label Unit: MJPY",
                x = "Risk Category",
                y = "STEC"
        )

dftest170920 <- dftest

setwd("C:/Users/S.Sagara/Documents/R/twitteR")
save(TwGetDF,file="Tweet_170920.Rda")
load("Tweet_170920.Rda")

