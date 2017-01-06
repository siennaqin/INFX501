business=read.csv("yelp_academic_dataset_business_numID.csv",stringsAsFactors = FALSE)
business <- business[,-1]
user=read.csv("yelp_academic_dataset_user_numID.csv",stringsAsFactors = FALSE)
user <- user[,-1]
review <- read.csv("finalreviews (1).csv",stringsAsFactors = FALSE)

#checkinraw=read.csv("yelp_academic_dataset_checkin.csv")

library(MatchIt)
eliteuser <- read.csv("eliteUsers.csv",colClasses=c("NULL",NA,NA,NA,NA))
#eliteuser$join_year <- as.Date(eliteuser$join_year,'%Y-%M')
#eliteuser$year <- format(eliteuser$join_year,'%Y')
#eliteuserpool <- subset(eliteuser,year<=2013)
#positive <- subset(eliteuser,isElite==1)
#negative <- subset(eliteuser,isElite==0)
user$yelping_since <- as.Date(user$yelping_since,'%Y-%M')
user$year <- format(user$yelping_since,'%Y')
colnames(mergeduser)
userpool <- subset(user,year<=2012)
mergeduser <- merge(x=eliteuser,y=userpool,by.x="id",by.y="numID")

model <- glm(isElite~review_count+fans+average_stars+votes.cool+votes.funny+votes.useful,data=mergeduser,family="binomial")
mergeuserss <- mergeduser[c("isElite","review_count","fans","average_stars","votes.cool","votes.funny","votes.useful")]
na.omitdf <- na.omit(mergeuserss)
model <- matchit(isElite~review_count+fans+average_stars+votes.cool+votes.funny+votes.useful,data=na.omitdf)
na.omituser <- na.omit(mergeduser)
psm <- matchit(isElite~year+compliments.plain+review_count+friends,data=mergeduser)

library(MatchIt)
business=read.csv("yelp_academic_dataset_business_numID.csv",stringsAsFactors = FALSE)
business <- business[,-1]
user=read.csv("yelp_academic_dataset_user_numID.csv",stringsAsFactors = FALSE)
user <- user[,-1]
review <- read.csv("finalreviews (1).csv",stringsAsFactors = FALSE)
eliteuser <- read.csv("eliteUsers.csv",colClasses=c("NULL",NA,NA,NA,NA))

elitereview <- subset(review,isElite=="1")
nonelitereview <- subset(review,isElite=="0")

elitereview$reviewyear <- as.Date(elitereview$date,"%Y-%m-%d")
elitereview$reviewyear <- format(elitereview$reviewyear,'%Y')

nonelitereview$reviewyear <- as.Date(nonelitereview$date,"%Y-%m-%d")
nonelitereview$reviewyear <- format(nonelitereview$reviewyear,'%Y')

elitereview$gapbtreel <- elitereview$first_elite_in-as.numeric(elitereview$reviewyear)
nonelitereview$gapbtreel <- nonelitereview$first_elite_in-as.numeric(elitereview$reviewyear)+2012

elite.bf.review <- subset(elitereview,gapbtreel>=0)
elite.af.review <- subset(elitereview,gapbtreel<0)

nonelite.bf.review <- subset(nonelitereview,gapbtreel>=0)
nonelite.af.review <- subset(nonelitereview,gapbtreel<0)

library(data.table)
elite.bf.review.agg <- data.table(elite.bf.review)
elite.bf.review.agg <- elite.bf.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

elite.af.review.agg <- data.table(elite.af.review)
elite.af.review.agg <- elite.af.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

nonelite.bf.review.agg <- data.table(nonelite.bf.review)
nonelite.bf.review.agg <- nonelite.bf.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

nonelite.af.review.agg <- data.table(nonelite.af.review)
nonelite.af.review.agg <- nonelite.af.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

bf.review.agg <- rbind(elite.bf.review.agg,nonelite.bf.review.agg)
af.review.agg <- rbind(elite.af.review.agg,nonelite.af.review.agg)

selected.attr.user <- user[c(24,1,2,4,5,6,7,8,10,11,12,14,17,19,21,22)]

bf.review.merged <- merge(bf.review.agg,selected.attr.user,by.x="id",by.y="numID")
af.review.merged <- merge(af.review.agg,selected.attr.user,by.x="id",by.y="numID")
mergeddf <- rbind(bf.review.merged,af.review.merged)

countfriend <- function(i){
  friends=as.character(mergeddf[10,10])
  friend1=strsplit(friends,"[",fixed=TRUE)
  friend1=strsplit(friend1[[1]][2],"]",fixed=TRUE)
  friend1=strsplit(friend1[[1]][1],",",fixed=TRUE)
  length(friend1[[1]])
}
bf.review.merged$friendcount <- c()
for (i in 1:length(bf.review.merged$id)){
  bf.review.merged$friendcount[i] <- countfriend(bf.review.merged,i)
}

af.review.merged$friendcount <- c()
for (i in 1:length(af.review.merged$id)){
  af.review.merged$friendcount[i] <- countfriend(af.review.merged,i)
}
