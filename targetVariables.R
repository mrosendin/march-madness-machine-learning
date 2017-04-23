library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(caret)

tourney = read.csv("./data/TourneyCompactResults.csv")

tourney = tourney %>% filter(Season >2002) %>% select(-Wscore, -Lscore, -Numot, -Wloc)
head(tourney)
nrow(tourney)

#SPlit half data to be 1's indicating win by team A, and half to be 0's indicating loss by team A
data1 = tourney %>% filter(Season <2010) %>% mutate(Outcome = 1) %>% rename(team_A = Wteam, team_B = Lteam)
data2 = tourney %>% filter(Season >2009) %>% mutate(Outcome = 0) %>% select(Season, Daynum, Lteam, everything()) %>% 
  rename(team_A = Lteam, team_B = Wteam)
tail(data2)

tourney = rbind(data1, data2)
table(tourney$Outcome)
head(tourney)

ratings = read.csv("./data/TeamRatings.csv")
names(ratings)[2] = "TeamID"
stats = read.csv("./data/FinalStats.csv")
head(ratings)
head(stats)

#Final stats of every team by season (nabeels + kaggles)
finalStats = left_join(stats, ratings, by = c("TeamID", "Season")) %>% select(-Pts, -W, -L, -X.y, -X.x)
head(finalStats)

# Begin to joins stats to games played
#First for the team A
names(tourney)[3] = "TeamID"
tourney = left_join(tourney, finalStats, by = c("TeamID", "Season"))
names(tourney)[6:length(tourney)] = paste0(names(tourney)[6:length(tourney)], "_A")

#then for team B
names(tourney)[3] = "Team_A"
names(tourney)[4] = "TeamID"
tourney = left_join(tourney, finalStats, by = c("TeamID", "Season"))
names(tourney)[27:length(tourney)] = paste0(names(tourney)[27:length(tourney)], "_B")

tourney = na.omit(tourney) #Omit NA's
tourney = tourney %>% filter(Season != 2017)


######## Some basic Statistics
round(cor(tourney.train[,6:17]), digits = 3)
round(cor(tourney.train[,21:38]), digits = 2)


# SPlit data
tourney$Outcome = as.factor(tourney$Outcome)
tourney$Seed_A = as.factor(tourney$Seed_A)
tourney$Seed_B = as.factor(tourney$Seed_B)

set.seed(123) #111
split = sample.split(tourney$Outcome, SplitRatio = 0.7)

tourney.train <- filter(tourney, split == TRUE)
tourney.test <- filter(tourney, split == FALSE)

xnam <- paste(names(tourney)[6:17], sep= "")
xnam2 <- paste(names(tourney)[21:38],sep = "")
xnam3 <- paste(names(tourney)[42:47], sep= "")
xnam4 = c(xnam, xnam2, xnam3)

#Different attributes for consideration
fmla <- as.formula(paste("Outcome ~ ", paste(xnam4, collapse= "+")))
fmla2 = Outcome ~ FGP_A + TPP_A + FTP_A + ORPG_A + DRPG_A + APG_A + SPG_A + BPG_A + PFPG_A + PCT_A + MOV_A + SOS_A + 
  SRS_A + Seed_A + FGP_B + TPP_B + FTP_B + ORPG_B + DRPG_B + APG_B + SPG_B + BPG_B + PFPG_B + PCT_B + MOV_B + SOS_B + 
  SRS_B + Seed_B
fmla3 = Outcome ~ FGP_A + TPP_A + FTP_A + ORPG_A + DRPG_A + APG_A + SPG_A + BPG_A + PFPG_A + PCT_A + MOV_A + SOS_A + 
  SRS_A + FGP_B + TPP_B + FTP_B + ORPG_B + DRPG_B + APG_B + SPG_B + BPG_B + PFPG_B + PCT_B + MOV_B + SOS_B + 
  SRS_B + Seed_B
fmla4 = Outcome ~ FGP_A + TPP_A + FTP_A + ORPG_A + DRPG_A + APG_A + SPG_A + BPG_A + PFPG_A + MOV_A + SOS_A + 
  SRS_A +FGP_B + TPP_B + FTP_B + ORPG_B + DRPG_B + APG_B + SPG_B + BPG_B + PFPG_B + MOV_B + SOS_B + SRS_B + PCT_A + PCT_B

# LOGISTIC REGRESSION
mod <- glm(fmla4, data=tourney.train, family="binomial")
summary(mod)

pred = predict(mod, newdata = tourney.test, type = "response")
hist(pred)
table(tourney.test$Outcome, pred > 0.5)

rocr.pred <- prediction(pred, tourney.test$Outcome)
ROC.performance <- performance(rocr.pred, "tpr", "fpr")
plot(ROC.performance, col='red')
abline(0, 1)

### with regulatization

library(glmnet)
tourney.train.mat = as.matrix(tourney.train)
tourney.train.mat.y = as.factor(tourney.train.mat[,5])
tourney.train.mat = tourney.train.mat[,c(6:17, 21,38,42:47)]

tourney.test.mat = as.matrix(tourney.test)
tourney.test.mat.y = as.factor(tourney.test.mat[,5])
tourney.test.mat = tourney.test.mat[,c(6:17, 21,38,42:47)]

# GLM with regulatization
mod3 = glmnet(tourney.train.mat, alpha = 0,tourney.train.mat.y, family = "binomial")
pred3 = predict(mod3, newx = as(tourney.test.mat, "dgCMatrix"), type = "response", s = c(.05,.1,2,3))
table(tourney.test.mat.y, pred3[,3] >.5)


plot(mod3, xvar = "dev", label = TRUE)

mod3.cv = cv.glmnet(as(tourney.train.mat,"dgCMatrix"),alpha=0 ,tourney.train.mat.y, family = "binomial",
                    type.measure = "class")
plot(mod3.cv)

mod3.cv$lambda.min
mod3.cv$lambda.1se

pred2 = predict(mod3.cv, newx = as(tourney.test.mat, "dgCMatrix"), s = "lambda.min", type = "response")
plot(pred2,pred)
abline(0,1)
abline(v=.5)
abline(h=.5)
table(tourney.test.mat.y, pred2 > 0.5)

#### RANDOM FOREST
library(randomForest)

train.rf <- train(fmla3,
                  data = tourney.train,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:20),
                  trControl = trainControl(method="cv", number=10, verboseIter = FALSE),
                  metric = "Accuracy")
train.rf$results
train.rf$bestTune
mod.rf = train.rf$finalModel

ggplot(train.rf$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 3) + 
  ylab("CV Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) + 
  geom_line() + scale_x_continuous(breaks = (seq(1,34,2)))

pred.rf = predict(mod.rf, newdata = tourney.test, type = "prob") #  Make Prediction and get probabilities
hist(pred.rf)
table(tourney.test$Outcome, pred.rf[,2]>.5)
varImpPlot(mod.rf)

#Add random forest ROC plot
rocr.pred <- prediction(pred.rf[,2], tourney.test$Outcome)
ROC.performance <- performance(rocr.pred, "tpr", "fpr")
par(new=T)
plot(ROC.performance, col='blue')
abline(0, 1)

acc.perf = performance(rocr.pred, measure = "acc")
plot(acc.perf)

#Optimal Cutoff -- not necessary but good to know
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

#### CART
library(rpart)
library(rpart.plot)

mod.cart = rpart(fmla,
             data = tourney.train, 
             method="class",
             cp=.05)
rpart.plot(mod.cart)
prp(mod.cart)

cpVals = data.frame(cp = seq(0, .5, by=.005))

# Perform Cross-Validation
train.cart = train(fmla3,
                    data = tourney.train,
                    method = "rpart",
                    tuneGrid = cpVals,
                    trControl = trainControl(method = "cv", number = 10),
                    metric = "Accuracy")
train.cart$results

ggplot(train.cart$results, aes(x = cp, y = Accuracy)) + geom_line() + geom_point(size = 2)
ggplot(train.cart$results, aes(x = cp, y = Accuracy)) + geom_line() + geom_point(size = 2) + xlim(0,.2)
train.cart$bestTune

mod.cart = train.cart$finalModel

rpart.plot(mod.cart)
prp(mod.cart)
mod.cart$variable.importance
pred.cart = predict(mod.cart, newdata = tourney.test, type = "prob")
table(tourney.test$Outcome, pred.cart[,2]>.5)
hist(pred.cart[,2])

rocr.pred <- prediction(pred.cart[,2], tourney.test$Outcome)
ROC.performance <- performance(rocr.pred, "tpr", "fpr")
par(new=T)
plot(ROC.performance, col='green', lwd=2)
abline(0, 1)

# SUPPORT VECTOR MACHINE
library(e1071)

#Default SVM
mod.svm <- svm(fmla, data = tourney.train, cost = .5, probability = TRUE, kernel= "linear")
pred.svm = predict(mod.svm, newdata = tourney.test, probability = TRUE)
table(tourney.test$Outcome, pred.svm)
attributes(mod.svm)
summary(mod.svm)
# With Cross_validation
set.seed(99)
mod.svm = tune(svm, fmla, data = tourney.train, kernel = "linear", ranges = list(cost = seq(.001,10,1))) #within the e1071 package and does 10-fold by default
mod.svm$performances

ggplot(mod.svm$performances, aes(cost, error))+geom_point(size =2) +geom_line() + scale_x_continuous(breaks = seq(0,8,1))
mod.svm = mod.svm$best.model

pred.svm = predict(mod.svm, newdata = tourney.test)
table(tourney.test$Outcome, pred.svm)

# radial kernel
mod.svm.radial = svm(fmla, data = tourney.train, cost = 1, gamma = .01, kernel = "radial")
pred.svm.radial = predict(mod.svm.radial, newdata = tourney.test)
table(tourney.test$Outcome, pred.svm.radial)

#radial with CV
mod.svm.radial = tune(svm, fmla, data = tourney.train, kernel = "radial", 
                      ranges = list(cost = seq(.001,10,1), gamma = seq(.001,.01,.001)))
summary(mod.svm.radial)

ggplot(mod.svm.radial$performances, aes(cost, error)) + geom_point() + geom_line(aes(color = as.factor(gamma)), lwd = 1)
mod.svm.radial$best.performance
mod.svm.radial = mod.svm.radial$best.model
pred.svm.radial = predict(mod.svm.radial, newdata=tourney.test)
table(tourney.test$Outcome, pred.svm.radial)

########################### MAKING PREDICTIONS FOR 2017 ##########################################

teams_2017 = read.csv("./data/TourneyCompactResults.csv")

teams_2017 = teams_2017 %>% filter(Season == 2017, Daynum < 138) %>% select(Wteam, Lteam)
teams_2017 = c(teams_2017$Wteam, teams_2017$Lteam) #the teams that made bracket

#all pairwise matcheups
games_comb = as.data.frame(t(combn(teams_2017, 2)))
colnames(games_comb) = c("Team_A", "Team_B")

dim(games_comb)
finalStats_2017 = finalStats %>% filter(Season == 2017)

# Begin to joins stats to games played
#First for the team A
names(games_comb)[1] = "TeamID"
games_comb = left_join(games_comb, finalStats_2017, by = "TeamID")
names(games_comb)[4:length(games_comb)] = paste0(names(games_comb)[4:length(games_comb)], "_A")

#then for team B
names(games_comb)[1] = "Team_A"
names(games_comb)[2] = "TeamID"
games_comb = left_join(games_comb, finalStats_2017, by = "TeamID")
names(games_comb)[25:length(games_comb)] = paste0(names(games_comb)[25:length(games_comb)], "_B")

head(games_comb)

#Some teams have NA for seeds so fix them
games_comb = games_comb %>% mutate(Seed_A = ifelse(Team_A==1307,14,Seed_A)) #fix newmexico
games_comb = games_comb %>% mutate(Seed_A = ifelse(Team_A==1377,16,Seed_A)) #fix South dakota
games_comb = games_comb %>% mutate(Seed_B = ifelse(TeamID==1307,14,Seed_B)) #fix newmexico
games_comb = games_comb %>% mutate(Seed_B = ifelse(TeamID==1377,16,Seed_B)) #fix South dakota

#feed seed factors
games_comb$Seed_A = as.factor(games_comb$Seed_A)
games_comb$Seed_B = as.factor(games_comb$Seed_B)


write.csv(games_comb, './data/dataSetfor2017.csv')
unique(games_comb$Seed_B)
#predict using logistic
pred_2017 = predict(mod, newdata = games_comb, type = "response")
hist(pred_2017)
pred_2017 = cbind(games_comb[,1:2], pred_2017)

#predict using random forest
pred2_2017 = predict(mod.rf, newdata = games_comb, type = "prob")
hist(pred2_2017[,2])
pred2_2017 = cbind(games_comb[,1:2], pred2_2017[,2])

#average probs
probs = as.data.frame(cbind(pred_2017, pred2_2017[,3]))
names(probs)[3:4] = c("logModelProb", "rfModelProb")
probs = probs %>% mutate(avgProb = (logModelProb + rfModelProb)/2, skewAvg = (3*logModelProb + rfModelProb)/4) #average the probabilities

ggplot(probs, aes(x = logModelProb, y = rfModelProb))+geom_point()+
  geom_abline(slope = 1, lwd=1, color="blue")+
  geom_hline(yintercept = .5, lwd =1, color = "red") +
  geom_vline(xintercept = .5, lwd=1, color="red")


write.csv(probs, "./data/SampleSubmission.csv")


svm.pred.2017=predict(mod.svm.radial, newdata = games_comb)
svm.pred.2017 = cbind(games_comb[,1:2], svm.pred.2017)








################################## BOOSTING #############################################
library(gbm)
#boosting requires special outcome to be character not factor
tourney.train.boost = tourney.train
tourney.train.boost$Outcome = as.character(tourney.train.boost$Outcome)

tourney.test.boost = tourney.test
tourney.test.boost$Outcome = as.character(tourney.test.boost$Outcome)

mod.boost = gbm(fmla,
                data = tourney.train.boost,
                distribution = "bernoulli",
                n.trees = 1000,
                interaction.depth = 9)

# NOTE: we need to specify number of trees to get a prediction for boosting
pred.boost = predict(mod.boost, newdata = tourney.test, n.trees = 1000, type = "response")

table(tourney.test$Outcome, pred.boost >.5)

# Cross validation now NOT WORKING

tGrid = expand.grid(n.trees = seq(100,1000,50), interaction.depth = seq(1,20,5),
                    shrinkage = seq(.01,.02,.005), n.minobsinnode = 10)

set.seed(232)
train.boost = train(fmla,
                    data = tourney.train.boost,
                    method = "gbm",
                    tuneGrid = tGrid,
                    trControl = trainControl(method="cv", number=5, verboseIter = FALSE),
                    metric = "Accuracy",
                    distribution = "bernoulli")

train.boost$results

ggplot(train.boost$results, aes(x = n.trees, y = Accuracy, colour = as.factor(interaction.depth))) + geom_line(lwd=1) + 
  ylab("CV Accuracy") + theme(axis.title=element_text(size=13), axis.text=element_text(size=13)) + 
  scale_color_discrete(name = "interaction.depth")

train.boost$bestTune

mod.boost = train.boost$finalModel
pred.boost = predict(mod.boost, newdata =tourney.test.boost, n.trees = 500, type = "response")









