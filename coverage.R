# This script reads kaggle files and runs the machine learning

rm(list=ls(all=TRUE)) 
cat("\014")  
dev.off()

######### Part 1 ###############
# Create dataset, validation set
################################

# Ensure the right packages are available

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(knitr)) install.packages("knitr")
if(!require(stringr)) install.packages("stringr")
if(!require(corrr)) install.packages("corrr")

library(readxl)


# Read in files & fix colums
# Read in NKOM Dekning
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/nkom-dekning")
#setwd("C:/Users/harald.wium.lie/Dropbox (Analysys Mason AS)/2020-QGIS/data/nkom-dekning")
dat_nkom <- read_delim("nkom_med_tett.csv",delim = ",", locale = locale(decimal_mark = "."))

# Filter rural only, translate variables, remove unnneeded variables
dat_nkom <- dat_nkom %>% filter(tettsted == FALSE)
dat_nkom <- dat_nkom %>%
  rename(muni_nr = kommune_nr, building_nr = bygnings_nr,
         homes = boliger) %>%
  subset(select = -c(building_nr, nord, ost, tettsted))

# exclude Svalbard islands (muni 2100)
dat_nkom <- dat_nkom[!(dat_nkom$muni_nr == 2100), ]

# read municipal information, translate variables, remove unneeded columns
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/kommuneinfo/2018")
#setwd("C:/Users/harald.wium.lie/Dropbox (Analysys Mason AS)/2020-QGIS/data/kommuneinfo/2018")
dat_muni1 <- read_delim("2018_muni_all.csv",delim = ",", locale = locale(decimal_mark = "."))

dat_muni1 <- dat_muni1 %>% 
  rename(muni_nr = kommune_nr, muni_homes = edr_homes) %>% 
  subset(select = -c(fylke, kommune))

# change muni 1567 ("old Rindal") to 5061 ("new Rindal")
dat_muni1[422, 1] = 5061

# make "county" column from muni_nr
dat_muni1$muni_nr <- as.character(dat_muni1$muni_nr)
dat_muni1 <- dat_muni1 %>% mutate(n_char = nchar(muni_nr),
                                  county = str_sub(muni_nr,1,n_char-2))

# set Trøndelag region to 15, set numeric, remove n_char
dat_muni1$county <- recode(dat_muni1$county,"50"="16")
dat_muni1$muni_nr <- as.numeric(dat_muni1$muni_nr)
dat_muni1$county <- as.numeric(dat_muni1$county)
dat_muni1 <- subset(dat_muni1, select = -c(n_char))


# read historical coverage info and merge into muni set
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/nkom-dekning")
#setwd("C:/Users/harald.wium.lie/Dropbox (Analysys Mason AS)/2020-QGIS/data/kommuneinfo/2018")
dat_hist <- read_delim("hist_coverage.csv",delim = ",", locale = locale(decimal_mark = "."))
dat_hist <- dat_hist %>% dplyr::rename(muni_nr = knr, 
                                oneh_s_13 ='100S_2013', oneh_a_13 ='100A_2013')

dat_muni <- left_join(dat_muni1, dat_hist, by = "muni_nr")

rm(dat_hist, dat_muni1)

# read public support information, tally and merge
setwd("C:/Users/workhome/Dropbox (Analysys Mason AS)/2020-QGIS/data/nkom-dekning")
dat_support <- read_delim("bb_support.csv",delim = ",", locale = locale(decimal_mark = "."))
dat_support <- dat_support %>% 
  group_by(knr) %>% 
  summarize(count=n()) %>% 
  rename(muni_nr = knr)
dat_muni <- left_join(dat_muni, dat_support, by = "muni_nr")
dat_muni$count[is.na(dat_muni$count)]<-0 
dat_muni <- dat_muni %>% rename(public_support = count)
rm(dat_support)

# merge into one dataset, rearrange so that Y comes first, factorize variables
dat_all <- left_join(dat_nkom, dat_muni, by = "muni_nr")
refcol <- "100mbit"
dat_all <- dat_all[, c(refcol, setdiff(names(dat_all), refcol))]
dat_all <- dat_all %>% rename(Y100 = "100mbit", b30 = "30mbit", b10 = "10mbit")

# dat_all$Y100 <- as.factor(dat_all$Y100)
rm(refcol)

# make new variables and remove unneeded variables
dat_all <- dat_all %>%
  mutate(pop_per_km2 = population / area_km2,
         urban_pop_per_km2 = (population * share_urban2) / urban_area,
         core = ifelse(oneh_s_13 > 0.10,1,0),
         share_live_homes = households / muni_homes)

dat_all$urban_pop_per_km2[is.na(dat_all$urban_pop_per_km2)] <- 0
dat_all <- subset(dat_all, select = -c(muni_nr, b30, knr_old, Navn, share_urban))


# check for NAs and remove
summary(dat_all$Y100)
rm(dat_muni, dat_nkom)

mean(dat_all$Y100 == 0)

# reduce dat_all to speed up algorithm (to be removed), make test and training set

set.seed(1, sample.kind = "Rounding")
dat_small <- sample_n(dat_all,10000,replace = FALSE)

test_index <- createDataPartition(y = dat_small$Y100, times = 1, p = 0.1, list = FALSE)
train_set <- dat_small[-test_index,]
test_set <- dat_small[test_index,]

train_set <- as.data.frame(train_set)
test_set <- as.data.frame(test_set)

summary(dat_small$Y100)

######### Part 2 ###############
# Data exploration
################################

y_hat <- 1
mean(y_hat == test_set$Y100)

test <- table(dat_small$homes, dat_small$Y100)
test <- table(dat_small$op_hq, dat_small$Y100)
test <- table(dat_small$public_support, dat_small$Y100)
test <- table(dat_small$core, dat_small$Y100)
test <- table(dat_small$b10, dat_small$Y100)
test <- table(dat_small$county, dat_small$Y100)

test_prop <- prop.table(test,1)
plot(test_prop)

test_prop <- as.data.frame(test_prop)
colnames(test_prop) <- c("Variable","Y100","Frequency")
test_prop %>% ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity')


# county
as.data.frame(prop.table(table(dat_small$county, dat_small$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity')

# op_hq
as.data.frame(prop.table(table(dat_small$op_hq, dat_small$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity')

# public support
as.data.frame(prop.table(table(dat_small$public_support, dat_small$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity')

# core
as.data.frame(prop.table(table(dat_small$core, dat_small$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity')

# b10 - what is this?
as.data.frame(prop.table(table(dat_small$b10, dat_small$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity')


# Continous distributions

dat_small$Y100 <- as.factor(dat_small$Y100)
dat_small %>% ggplot(aes(centrality, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(ave_income, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(pop_urban, colour = Y100)) + geom_density() +
  scale_x_continuous(limits = c(0, 50000))
dat_small %>% ggplot(aes(muni_free_rev, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(muni_debt, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(muni_net, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(employment, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(share_urban2, colour = Y100)) + geom_density()
dat_small %>% ggplot(aes(area_km2, colour = Y100)) + geom_density() +
  scale_x_continuous(limits = c(0, 5000))
dat_small %>% ggplot(aes(muni_homes, colour = Y100)) + geom_density() +
  scale_x_continuous(limits = c(0, 10000))
dat_small %>% ggplot(aes(households, colour = Y100)) + geom_density() +
  scale_x_continuous(limits = c(0, 10000))
dat_small %>% ggplot(aes(virksomheter, colour = Y100)) + geom_density() +
  scale_x_continuous(limits = c(0, 4000))

dat_small %>% ggplot(aes(core, op_hq, color = Y100)) +
  geom_point()

dat_small %>% ggplot(aes(op_hq, basic_2001, fill = factor(Y100))) +
  geom_boxplot()

dat_small %>% ggplot(aes(op_hq, fill = factor(Y100))) +
  geom_bar()


corr <- correlate(dat_small)


######### Part 3 ###############
# Modeling
################################


# train model
y_hat <- 0
mean(y_hat == test_set$Y100)



# feature selection using rfe in caret
#control <- rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 3,verbose = FALSE)
#outcome_name <- 'Y100'
#predictors <- names(train_set)[!names(train_set) %in% outcome_name]
#coverage_profile <- rfe(train_set[,predictors], as.matrix(train_set[,outcome_name]),
#                        rfeControl = control)
#coverage_profile
#predictors<-c('urban', 'b10', 'area_km2', 'centrality', 'ave_income')


# train model

y_hat <- 0
mean(y_hat == test_set$Y100)


train_nnet<-train(Y100 ~.,method='nnet', data = train_set)
confusionMatrix(predict(train_nnet, test_set), test_set$Y100)$overall["Accuracy"]
imp_nnet <- varImp(train_nnet)
plot(imp_nnet)

train_glm <- train(Y100 ~ .,method = "glm",data = train_set)
confusionMatrix(predict(train_glm, test_set), test_set$Y100)$overall["Accuracy"]
glm_imp <- varImp(train_glm)
plot(glm_imp)

train_knn <- train(Y100 ~ ., method = "knn",data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$Y100)$overall["Accuracy"]
imp_knn <- varImp(train_knn)
plot(imp_knn)

train_rf <- train(Y100 ~ ., method = "rf", data = train_set)
confusionMatrix(predict(train_rf, test_set), test_set$Y100)$overall["Accuracy"]
rf_imp <- varImp(train_rf)
plot(rf_imp)
rf_imp

install.packages("FCNN4R-package")

train_rf <- train(Y100 ~ ., method = "mlpSGD", data = train_set)
confusionMatrix(predict(train_rf, test_set), test_set$Y100)$overall["Accuracy"]
rf_imp <- varImp(train_rf)
plot(rf_imp)
rf_imp





#################### old code
test <- dat_small %>% {prop.table(table(.$county, .$Y100))}

test <- dat_small %>% with(table(county,Y100)) %>% as.data.frame.matrix() 

dat_small$Y100 <- as.numeric(dat_small$Y100)
test <- dat_small %>% group_by(county) %>% summarise(count = n())
test
dat_small$Y100 <- as.factor(dat_small$Y100)


