#--------------------------Importing data-------------------------------------- 
battles <- read.csv("F:/data/game-of-thrones/battles.csv")
  View(battles)
 character.deaths <- read.csv("F:/data/game-of-thrones/character-deaths.csv")
   View(character.deaths)
 character.predictions <- read.csv("F:/data/game-of-thrones/character-predictions.csv")
 View(character.predictions)
 
#-------------------------------------------------------------------------------
 
library(ggplot2)
attach(battles)
#-------------Which King fought Maximum Number of Battles?----------------
ggplot(battles,aes(x=factor(attacker_king)))+geom_bar(aes(fill=factor(attacker_king)))+
xlab("Attacker King")+ylab("No. of battles")+ggtitle("No. of battles fought by King")+
  theme(axis.text.x = element_text(angle=90))
 

#-------------------Attacker size vs Defender size-------------------------
#---------------------Who wins?-------------------------------

ggplot(battles,aes(x=defender_size,y=attacker_size,col=attacker_outcome,size=attacker_outcome))+geom_point()+
xlab("Attacker Size")+ylab("Defender Size")

#------------------------Types of Battles----------------------------
ggplot(battles,aes(x=battle_type))+geom_bar(aes(fill=battle_type))+
  xlab("Battle Type")+ylab("No. of Battles")+ggtitle("Types of battle fought")
#--------------Types of battle and attacker king--------------------------
ggplot(battles,aes(x=factor(attacker_king)))+geom_bar(aes(fill=attacker_king))+
  facet_wrap(~battle_type)+xlab("Attacker King")+ylab("No. of Battles")+ggtitle("Types of Battle and Attacker king")
#------------------How commander have performed----------------------
ggplot(data = battles, aes(x = factor(attacker_commander), fill=attacker_outcome)) +
  geom_bar(aes(fill=factor(attacker_outcome)), width=1) +
  facet_wrap(~ attacker_king) + coord_flip() +
  ggtitle("Attacker Kings and Their Commanders") +
  xlab("Commander") + ylab("No of Battles")
#------------Which pitch causes major death----------------
ggplot(battles,aes(x=major_death))+geom_bar(aes(fill=factor(major_death)))+
  facet_wrap(~battle_type)

#-----------number of death of allegiance-------------------------
attach(character.deaths)
ggplot(character.deaths,aes(x=Allegiances))+geom_bar(aes(fill=Allegiances))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Death of Allegiance") + ylab("Deaths")
  
#--------------Which character is more populat--------------------
attach(character.predictions)
#make a dataset which contain name of character with more popularity

c<-character.predictions[popularity>0.5,]
head(c)
attach(c)

ggplot(c,aes(x=c$name))+geom_bar(aes(fill=popularity))+coord_flip()+
  labs(title="Popularity of characters",y=" ",x="Character")

#-----------------Text mining on battle notes------------------------

library(tm)
library(wordcloud)
word<-Corpus(VectorSource(note))  #List of 38
word<-tm_map(word,removePunctuation)
word<-tm_map(word,PlainTextDocument)
word<-tm_map(word,content_transformer(tolower))
word<-tm_map(word,removeWords,stopwords("english"))
word<-tm_map(word,stemDocument)
word<-tm_map(word,stripWhitespace)
wordcloud(word,random.order=F,sampe=c(5,0.5)) 
#Most of the notes are about stark,battle,greyjoy, men and number
 
 
 