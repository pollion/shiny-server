working_dir<-getwd()
source(paste(working_dir, "/functions.r",sep="",collapse=NULL))


##create vector of packages to either install or load
packages_to_install <- c("ggplot2","shiny","reshape","scales","grid","gridExtra",
                         "rjson","tm","wordcloud","RColorBrewer","psych")
packages_to_install

#install.packages(packages_to_install)
##use lapply to be able to use library() despite quatation marks
lapply(packages_to_install, library, character.only=T)

############################ data manipulation
#get and set working directory
working_dir<-getwd()
working_dir

startpoll_json_file<-"/Users/Thomas/Documents/app_pollion/app_pasta_neu/data/pasta_data2.json"


#load data
poll_data<-fromJSON(file=startpoll_json_file)

#in case of empty list element, replace by NA
poll_data <- lapply(poll_data, function(x) {
        as.data.frame(replace(x, sapply(x, is.list), NA))
})

#use rbind.fill to automatically fill empty elements to avoid messing up number of columns
poll_data <- rbind.fill(poll_data)
poll_data
head(poll_data)
names(poll_data)
table(poll_data$citySize)

oldnames<-c(colnames(poll_data))
oldnames

newnames<-c("X_v","X_id",                  
            "creationtime","finished",
            "pollid","publicpoll",
            "userid","gender",
            "birthYear","citySize",
            "organicConsumptionFrequency1","pasta_sorte",           
            "erwartung_erfullt","bewertung_geschmack",          
            "bewertung_konsistenz","wieder_kaufen",           
            "weiter_empfehlen","gut_offen",          
            "schlecht_offen","gender",          
            "birthYear","organicConsumptionFrequency")

for(i in 1:length(colnames(poll_data))) {
        names(poll_data)[names(poll_data)==oldnames[i]]=newnames[i]
}

head(poll_data)

data<-poll_data[,c(8:19)]

names(data)

#gender
level_reorder_gender <- c("female","male","not answered")
level_labels_gender <- c("weiblich","mannlich","keine Angabe")

func_categorial("gender",level_reorder_gender,level_labels_gender)

#year of birth
upper_bound_birthyear <- 1990
lower_bound_birthyear <- 1900

func_integer("birthYear",upper_bound_birthyear,lower_bound_birthyear)

#city of residence
levels(data$citySize)
level_reorder_city <- c("less than 5000","5000 - 19999","20000 - 99999","100000 - 1000000","more than 1000000","not answered")
level_labels_city <- c("weniger als 5000","5000 - 19999","20000 - 99999","100000 - 1000000","mehr als 1000000","keine Angabe")

func_ordinal("citySize",level_reorder_city,level_labels_city)

##################make sure how to deal with socio demographic dublicates!!!!!!!!!!!!

#organicConsumptionFrequency
level_reorder_consumption=c("daily","weekly","monthly")
level_label_consumptionlabels=c("taglich","wochentlich","monatlich")

func_ordinal("organicConsumptionFrequency1",level_reorder_consumption,level_label_consumptionlabels)


#Pasta Sort
level_reorder_pasta_sorte=c(levels(data$pasta_sorte)[1],levels(data$pasta_sorte)[4],
                            levels(data$pasta_sorte)[5],levels(data$pasta_sorte)[3],
                            levels(data$pasta_sorte)[2])

level_label_pasta_sorte=c(levels(data$pasta_sorte)[1],levels(data$pasta_sorte)[4],
                          levels(data$pasta_sorte)[5],levels(data$pasta_sorte)[3],
                          levels(data$pasta_sorte)[2])

func_categorial("pasta_sorte",level_reorder_pasta_sorte,level_label_pasta_sorte)

#Erwartung erfullt
data$erwartung_erfullt
level_reorder_erwartung_erfullt<-c("1 (ganz und gar nicht)","2","3","4","5 (voll und ganz)")
level_label_erwartung_erfullt<-c("1","2","3","4","5")

func_ordinal("erwartung_erfullt",level_reorder_erwartung_erfullt,level_label_erwartung_erfullt)

#bewertung_geschmack
level_reorder_bewertung_geschmack<-c("1 (sehr schlecht)","2","3","4","5 (sehr gut)")
level_label_bewertung_geschmack<-c("1","2","3","4","5")

func_ordinal("bewertung_geschmack",level_reorder_bewertung_geschmack,level_label_bewertung_geschmack)
data$bewertung_geschmack

#bewertung_konsistenz
level_reorder_bewertung_konsistenz<-level_reorder_bewertung_geschmack
level_label_bewertung_konsistenz<-level_label_bewertung_geschmack

func_ordinal("bewertung_konsistenz",level_reorder_bewertung_konsistenz,level_label_bewertung_konsistenz)

#wieder kaufen
level_reorder_wieder_kaufen<-c("1 (bestimmt nicht wieder kaufen)","2","3","4","5 (bestimmt wieder kaufen)")
level_label_wieder_kaufen<-c("1","2","3","4","5")

func_ordinal("wieder_kaufen",level_reorder_wieder_kaufen,level_label_wieder_kaufen)

#weiter empfehlen
level_reorder_weiter_empfehlen<-c("Ja","Nein","Keine Angabe")
level_label_weiter_empfehlen<-c("Ja","Nein","Keine Angabe")


table(data[, 11])


pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
#wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
xkcd.corpus <- Corpus(DataframeSource(data.frame(data[, 11])))
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
v


xkcd.corpus2 <- Corpus(DataframeSource(data.frame(data[, 12])))
tdm2 <- TermDocumentMatrix(xkcd.corpus2)
m2 <- as.matrix(tdm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
v2

#sumage<-summary(data[,2])
#sumage[1,1]
#names(sumage)
      
#library(psych)
##sumtable<-as.matrix(describe(data[,2]))
#sumtable
#sumtable<-sumtable[,c(2:5,8:9)]
#sumtable
#xtable(as.matrix(sumtable),type="html")

#sumtable<-as.matrix(describe(data[,2]))
#sumtable
#sumtable[,c(2:5,8:19)]

#sumtable <- summary(as.matrix(data[,2]))
#sumtable   
#mean(data[,2])
#meadian(data[,2])
#xtable(sumtable,type="html")

