install.packages("synthpop")

library("synthpop")

######################read dataset####################################
ratings <- read.csv("Data/obsRating.csv")

ratings$timestamp <- NULL

ratings$rating= factor(as.character(ratings$rating))



my.seed <- 100000
sdsRating <- syn(ratings, visit.sequence = c(1, 2, 3), method = c(" ", " ", "cart"), m=5, seed = NA, proper = FALSE)


sdsRating

summary(sdsRating)
head(sdsRating)

compare(sdsRating, ratings, vars = "rating", msel=1:5)

model.ratings <- glm(rating ~ userid + itemid,
                     family = "gaussian", data = ratings) 
model.ratings

model.sds <- glm.synds(rating ~ userid + itemid,
                       family = "gaussian", data = sdsRating) 
model.sds

compare.fit.synds(model.sds, ratings, msel=1:3)

u1 <- utility.gen(sdsRating, ratings)
u1 <- utility.tab(sdsRating, ratings, vars = "rating")
print(u1, print.zscores = TRUE, usethresh = TRUE)
     sdsRating 
write.syn(sdsRating, filename = "CARTdraft", filetype = "csv", convert.factors = "numeric", data.labels = NULL, save.complete = TRUE )

######################

sdssdc <- sdc(sdsRating, ratings, label = "false_data",
              rm.replicated.uniques = TRUE, recode.vars = "rating",
              bottom.top.coding = c(200, 5000), recode.exclude = -8)
View(sdssdc) 
sdssdc
