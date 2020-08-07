library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)

booksyn = read.csv('/Users/manel/Documents/Experiments/Dataset_final/books.csv')
ratingsyn = read.csv('/Users/manel/Documents/Experiments/Dataset_final/Goodbooksyn.csv')
colnames(ratingsyn)
dataBookSyn = merge(booksyn, ratingsyn, by ="bookid")
View(booksyn)

glimpse(booksyn)
glimpse(dataBookSyn)
library(dplyr)
##not working as expected
#write.csv(dataBookSyn, file = '/Users/manel/Documents/Experiments/Dataset_final/bookPre.csv')


colnames(dataBookSyn)
BD_RatingBookSyn= dataBookSyn[dataBookSyn$rating %in% c(5),]
table(BD_RatingBookSyn$rating)
colnames(BD_RatingBookSyn)
best_authorsSyn <- BD_RatingBookSyn %>%
  select(userid, authors, rating) %>%
  separate_rows(authors, sep = "\\|") 
best_authorsSyn$rating <- as.factor(best_authorsSyn$rating)
best_authorsSyn$authors <- as.factor(best_authorsSyn$authors)

list.of.authorsSyn <- as.data.frame(xtabs(~ userid + authors + rating, data = best_authorsSyn)) 
list.of.authors.2Syn= list.of.authorsSyn[list.of.authorsSyn$Freq > 0,]

allUserBookSyn = unique(list.of.authors.2Syn$userid)

Best.Vote.AuthorSyn= data.frame()

for (i in 1:length(allUserBookSyn)) {
  print(c(i, allUserBookSyn[i]))
  df_authorSyn = list.of.authors.2Syn[which(list.of.authors.2Syn$userid== allUserBookSyn[i]),]
  tot_sumAuthSyn = sum(df_authorSyn$Freq)
  df_authorSyn["Percentage"] = (df_authorSyn$Freq / tot_sumAuthSyn) *100
  PrefUserAuthorSyn = df_authorSyn[which.max(df_authorSyn$Percentage),]
  Best.Vote.AuthorSyn = rbind(Best.Vote.AuthorSyn,PrefUserAuthorSyn)
}

write.table(Best.Vote.AuthorSyn, file = "/Users/manel/Documents/Experiments/Dataset_final/ResultBestActorDirector/Best.Vote.AuthorSyn.csv")



### percentage of best authors in original and synthetic data

original.user.author= Best.Vote.Author[,c(1,2)]
Synthetic.user.author= Best.Vote.AuthorSyn[,c(1,2)]



#compareDataFrame2 = function(d1, d2){
 # find=0
#  for (i in 1:nrow(d1)) {
    
 #   u= as.character(d1[i,1]) 
    
  #    print(c(i,j))
   #   u2= as.character(d2[j,1])
  #    if(u==u2){
  #      dr=as.character(d1[i,2])
  #      dr2= as.character(d2[j,2])
  #      if (dr==dr2){
  #        find= find + 1
        
  #    }
  #  }
  #}
  #return(find)
#}


compareDataFrame = function(d1, d2, idu, ida){
  find=0
  allUs = as.character(d2[,idu])
  for (i in 1:nrow(d1)) {
    u = as.character(d1[i,idu])
    print(c(i,u))
    belonfsTo = is.element(u, allUs)
    if(belonfsTo){
      index = which(u == allUs)
      dru  = as.character(d1[i, ida])
      drus = as.character(d2[index, ida]) 
      if (dru == drus){
        find= find + 1
      }
    }
  }
  return(100 - (find / nrow(d1) * 100))
}


find.testAuthor = compareDataFrame(original.user.author, Synthetic.user.author, 1,2)


