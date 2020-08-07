### Sources:
# https://github.com/dorukkilitcioglu/books2rec/blob/master/Visualization/Ratings.md

library(tidyverse)
library(ggthemes)
library(ggplot2)

GoodBook_data_original <- read.csv('/Users/manel/surfdrive/wraprec.2.0.15_mono64/ml-100k/GoodBook-Data/book-ratings.csv')

#yahoo <- readr::read_table('/Users/manel/Downloads/Webscope_R1/ydata-ymusic-ratings.txt')
BookCross1 = read.csv("C:/Users/manel/surfdrive/Experiments/Dataset_final/data PLUS/NewCommon/BookCross-Randomization/book-crossItemIDsansString1.txt")
#BookCross1$itemid2 <- NULL
#BookCross1 %>%
#  drop_na()

GoodBook_data_syn = read.csv('Output/Goodbooksyn.csv')
GoodBook_data_ran = read.csv('Output/RandomSampleML2.csv')
GoodBook_data_swap = read.csv('Output/df_syn_swapped_users.csv')
#GoodBook_data_shuffle = read.csv('C:/Users/manel/Documents/Use_Clusters_TUDelft/Result_experiments_clusters/07-01-2019/Idea-4-Shuffling/All_Shuffle/results/df_syn_Shuffle_All.csv')
#GoodBook_data_swap_shuffle = read.csv('C:/Users/manel/Documents/Use_Clusters_TUDelft/Result_experiments_clusters/07-01-2019/Idea-1-Swapping-Shuffling-All/results/df_syn_Swap_Shuffle_All.csv')

write.csv(GoodBook_data_original, file = "/Users/manel/Documents/sigir_experiments/RecSys_Performance_Long_Tail/GoodBook_data_original.csv", quote = FALSE, col.names = TRUE, index = FALSE)
write.csv(aa, file = "C:/Users/manel/surfdrive/Experiments/Dataset_final/data PLUS/NewCommon/BookCross-Randomization/Book_Cross_Original.csv", quote = FALSE, row.names = FALSE)

aa = read.csv('C:/Users/manel/surfdrive/Experiments/Dataset_final/data PLUS/NewCommon/BookCross-Randomization/Book_Cross_Original.csv')
head(aa)
aa$a <- NULL
#head(GoodBook)
#head(BookCross)
head(GoodBook_data_original)

GoodBook_data_original = as.data.frame(cbind(GoodBook_data_original[,2], GoodBook_data_original[,1], GoodBook_data_original[,3]))
colnames(GoodBook_data_original)= c('userid', 'itemid', 'rating')
write.csv(GoodBook_data_original, file = "/Users/manel/Documents/sigir_experiments/RecSys_Performance_Long_Tail/GoodBook_data_original.csv", quote = FALSE, row.names = FALSE)

colnames(Ml_data_syn) = c('userid', 'itemid', 'rating')
colnames(Ml_data_swap) = c('userid', 'itemid', 'rating')
colnames(Ml_data_shuffle) = c('userid', 'itemid', 'rating')
colnames(Ml_data_swap_shuffle) = c('userid', 'itemid', 'rating')

GoodBook_data_original = as.data.frame(cbind(GoodBook_data_original[,2], GoodBook_data_original[,1], GoodBook_data_original[,3]))
colnames(GoodBook_data_original)= c('userid', 'itemid', 'rating')



BookCross$itemid2 <- NULL
colnames(BookCross1)
colSums(bookCross_Original)
## BookCross original
bookCross_Original = read.csv('Data/df_book_Cross_WithoutNA.csv')
head(bookCross_Original)
install.packages("ggthemes")
library(ggthemes)
all_ratings <- bind_rows(
  bookCross_Original %>%
    mutate(Source = "bookCross_Original"))
  
  Ml_data_syn %>%
    mutate(Source = "Ml_data_syn"),
  
  Ml_data_swap %>%
    mutate(Source = "Ml_data_swap"),
  
  Ml_data_shuffle %>%
    mutate(Source = "Ml_data_shuffle"),
  
  Ml_data_swap_shuffle %>%
    mutate(Source = "Ml_data_swap_shuffle"),
  
  Ml_data_ran %>%
    mutate(Source = "Ml_data_ran")
  
)

gsum <- bookCross_Original %>%
  group_by(rating) %>%
  summarize(Count = n()) %>%
  mutate(Source = "bookCross_Original", Percent = Count/sum(Count)) %>%
  ungroup()
asum <- Ml_data_syn %>%
  group_by(rating) %>%
  summarize(Count = n()) %>%
  mutate(Source = "Ml_data_syn", Percent = Count/sum(Count)) %>%
  ungroup()
swsum <- Ml_data_swap %>%
  group_by(rating) %>%
  summarize(Count = n()) %>%
  mutate(Source = "Ml_data_swap", Percent = Count/sum(Count)) %>%
  ungroup()
shsum <- Ml_data_shuffle %>%
  group_by(rating) %>%
  summarize(Count = n()) %>%
  mutate(Source = "Ml_data_shuffle", Percent = Count/sum(Count)) %>%
  ungroup()
swshsum <- Ml_data_swap_shuffle %>%
  group_by(rating) %>%
  summarize(Count = n()) %>%
  mutate(Source = "Ml_data_swap_shuffle", Percent = Count/sum(Count)) %>%
  ungroup()
ransum <- Ml_data_ran %>%
  group_by(rating) %>%
  summarize(Count = n()) %>%
  mutate(Source = "Ml_data_ran", Percent = Count/sum(Count)) %>%
  ungroup()
csum <- all_ratings %>%
  group_by(rating, Source) %>%
  summarize(Count = n()) %>%
  group_by(Source) %>%
  mutate(Percent = Count/sum(Count))
head(csum)


### PLot
ggplot(csum, aes(x = reorder(rating, -rating), y = Percent, fill = Source)) +
  geom_bar(data = filter(csum, Source == "Ml_data_Original"), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_ran") %>% mutate(Percent = -Percent), stat = "identity") +
  geom_text(data = filter(csum, Source == "Ml_data_Original"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = Percent + 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_ran"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = -Percent - 0.05)) +
  coord_flip() +
  scale_y_continuous(limits = c(-.7, .7), labels = NULL) +
  ylab("# Ratings") +
  xlab("Rating") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  ggtitle("MovieLens100k-Part1: Relative distribution of ratings") +
  # set transparency
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

ggplot(csum, aes(x = reorder(rating, -rating), y = Percent, fill = Source)) +
  geom_bar(data = filter(csum, Source == "Ml_data_shuffle"), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_swap") %>% mutate(Percent = -Percent), stat = "identity") +
  geom_text(data = filter(csum, Source == "Ml_data_shuffle"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = Percent + 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_swap"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = -Percent - 0.05)) +
  coord_flip() +
  scale_y_continuous(limits = c(-.7, .7), labels = NULL) +
  ylab("# Ratings") +
  xlab("Rating") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  ggtitle("MovieLens100k-Part2: Relative distribution of ratings") +
  # set transparency
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )


ggplot(csum, aes(x = reorder(rating, -rating), y = Percent, fill = Source)) +
  geom_bar(data = filter(csum, Source == "Ml_data_Original"), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_ran") %>% mutate(Percent = -Percent), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_shuffle"), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_swap") %>% mutate(Percent = -Percent), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_swap_shuffle"), stat = "identity") +
  geom_bar(data = filter(csum, Source == "Ml_data_syn") %>% mutate(Percent = -Percent), stat = "identity") +
  geom_text(data = filter(csum, Source == "Ml_data_Original"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = Percent + 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_ran"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = -Percent - 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_shuffle"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = Percent + 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_swap"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = Percent + 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_swap_shuffle"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = Percent + 0.05)) +
  geom_text(data = filter(csum, Source == "Ml_data_syn"), aes(label = sprintf("%.0f%%", Percent * 100), x = reorder(rating, -rating), y = -Percent - 0.05)) +
  coord_flip() +
  scale_y_continuous(limits = c(-.7, .7), labels = NULL) +
  ylab("# Ratings") +
  xlab("Rating") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  ggtitle("MovieLens 100k: Relative distribution of ratings")


### Average rating for both datasets
##  Source     Avg
#<chr>    <dbl>
# 1 GoodBook  3.88
# 2 Ml_data   3.59
## The average ratings per user are also quite high. Even though 
#the distribution looks very skewed for Amazon, 
#the average ratings per user are not too different.

all_ratings %>%
  group_by(user_id, Source) %>%
  summarize(Avg_Rating = mean(rating)) %>%
  group_by(Source) %>%
  summarize(Avg = mean(Avg_Rating))

## We also look at the distributions of user ratings.
user_averages <- all_ratings %>%
  group_by(user_id, Source) %>%
  summarize(Avg_Rating = mean(rating), Num_Rated = n(), Variance = var(rating))


########

ggplot(user_averages, aes(x = Avg_Rating, color = Source)) +
  geom_density(alpha = .3, kernel = "gaussian", adjust = 1.8) +
  xlab("Average Rating per User") +
  ylab("") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank()) 
#ggtitle("Average user rating distribution")

######## density_rating_dist

ggplot(user_averages, aes(x = Variance, color = Source), show.legend = FALSE) +
  geom_density(alpha = .3, kernel = "gaussian", adjust = 1.8) +
  xlab("Variance Rating per User") +
  ylab("") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank()) +
  theme_bw() +
  guides(fill=FALSE, color=FALSE) +
  ggtitle("Variance user rating distribution")



## let's have a look at the correlation between the number of ratings 
#of a user and the user's average rating.

ggplot(filter(user_averages, Num_Rated <=200), aes(x = Num_Rated, y = Avg_Rating)) +
  geom_hex(bins = 60) +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(.~Source) +
  ylab("Average Rating per User") +
  xlab("Number of Ratings per User") +
  theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold")) +
  labs(fill = "# Users")


var(all_ratings$rating)
# VS
var(bookCross_Original$rating)


######################## Rating per items ###############
colnames(GoodBook) = c('userid', 'itemid', 'rating')

all_ratings %>%
  group_by(itemid, Source) %>%
  summarize(Avg_Rating = mean(rating)) %>%
  group_by(Source) %>%
  summarize(Avg = mean(Avg_Rating))

## We also look at the distributions of item ratings.
item_averages <- all_ratings %>%
  group_by(itemid, Source) %>%
  summarize(Avg_Rating = mean(rating), Num_Rated = n(), Variance = var(rating))

ggplot(item_averages, aes(x = Avg_Rating, fill = Source, color = Source)) +
  geom_density(alpha = .3, kernel = "gaussian", adjust = 1.8) +
  xlab("Average Rating per Item") +
  ylab("") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank()) +
  ggtitle("Average item rating distribution")


## let's have a look at the correlation between the number of ratings of a user and the user's average rating.

ggplot(filter(item_averages, Num_Rated <=200), aes(x = Num_Rated, y = Avg_Rating)) +
  geom_hex(bins = 60) +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(.~Source) +
  ylab("Average Rating per Item") +
  xlab("Number of Ratings per Item") +
  theme_minimal() +
  theme(strip.text = element_text(size = 14, face = "bold")) +
  labs(fill = "# Items")
