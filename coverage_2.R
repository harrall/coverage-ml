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
if(!require(readxl)) install.packages("readxl")
if(!require(forcats)) install.packages("forcats")


# Read in files & fix columns
# Download main coverage file from nkom.no, rename columns
url <- "https://www.nkom.no/npt/dekningskart/2019/r%c3%a5data/2019_dekningskart_bygninger.csv"
download.file(url, "nkom.csv")
dat_nkom <- read_delim("nkom.csv",delim = ";", locale = locale(decimal_mark = "."))
dat_nkom <- dat_nkom %>% rename(building_nr = byggnr)

# Read in rural buildings
url <- 'https://raw.githubusercontent.com/harrall/coverage-ml/master/rural_b.csv'
rural_b <- read_delim(url,delim = ",", locale = locale(decimal_mark = "."))
rural_b <- rural_b %>% rename(building_nr = bygnings_nr) %>% mutate(rural = 1)

# Join rural buildings with main coverage file, filter on rural only
dat_nkom <- left_join(dat_nkom, rural_b, by = "building_nr")
dat_nkom <- dat_nkom %>% filter(rural == 1)

# Translate variables into English, remove unnneeded variables
dat_nkom <- dat_nkom %>%
  rename(muni_nr = komnr, homes = boliger) %>%
  subset(select = -c(building_nr, nord, ost, rural))

# exclude Svalbard islands (muni 2100)
dat_nkom <- dat_nkom[!(dat_nkom$muni_nr == 2100), ]

# Read municipal information, translate variables, remove unneeded columns
url <- 'https://raw.githubusercontent.com/harrall/coverage-ml/master/2018_muni_all.csv'
dat_muni <- read_delim(url,delim = ",", locale = locale(decimal_mark = "."))

dat_muni <- dat_muni %>% 
  rename(muni_nr = kommune_nr, muni_homes = edr_homes) %>% 
  subset(select = -c(fylke, kommune))

# change muni 1567 ("old Rindal") to 5061 ("new Rindal")
dat_muni[422, 1] = 5061

# make "county" column from muni_nr
dat_muni$muni_nr <- as.character(dat_muni$muni_nr)
dat_muni <- dat_muni %>% mutate(n_char = nchar(muni_nr),
                                  county = str_sub(muni_nr,1,n_char-2))

# set Trøndelag region to 15, set numeric, remove n_char
dat_muni$county <- recode(dat_muni$county,"50"="16")
dat_muni$muni_nr <- as.numeric(dat_muni$muni_nr)
dat_muni$county <- as.numeric(dat_muni$county)
dat_muni <- subset(dat_muni, select = -c(n_char))

# Read historical coverage info and merge into muni set, remove unneeded tibbles
url <- 'https://raw.githubusercontent.com/harrall/coverage-ml/master/hist_coverage.csv'
dat_hist <- read_delim(url,delim = ",", locale = locale(decimal_mark = "."))
dat_hist <- dat_hist %>% dplyr::rename(muni_nr = knr, 
                                oneh_s_13 ='100S_2013', oneh_a_13 ='100A_2013')

dat_muni <- left_join(dat_muni, dat_hist, by = "muni_nr")
rm(dat_hist, rural_b)

# Read public support information, tally and merge
url <- 'https://raw.githubusercontent.com/harrall/coverage-ml/master/bb_support.csv'
dat_support <- read_delim(url,delim = ",", locale = locale(decimal_mark = "."))
dat_support <- dat_support %>% 
  group_by(knr) %>% 
  summarize(count=n()) %>% 
  rename(muni_nr = knr)
dat_muni <- left_join(dat_muni, dat_support, by = "muni_nr")
dat_muni$count[is.na(dat_muni$count)]<-0 
dat_muni <- dat_muni %>% rename(public_support = count)
rm(dat_support)

# Merge into one dataset, rearrange so that Y comes first, factorize variables
dat_all <- left_join(dat_nkom, dat_muni, by = "muni_nr")
refcol <- "100mbit"
dat_all <- dat_all[, c(refcol, setdiff(names(dat_all), refcol))]
dat_all <- dat_all %>% rename(Y100 = "100mbit", b30 = "30mbit", b10 = "10mbit")
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

######### Part 2 ###############
# Data exploration
################################

# Correlation table
corr <- correlate(dat_all) 

# Correlation plot
corr %>% select(rowname, Y100) %>%
  filter(rowname != 'Y100')%>%
  mutate(rowname=factor(rowname, levels = rowname[order(Y100)])) %>%
  ggplot(aes(x = rowname, y = Y100)) +
  geom_segment( aes(xend=rowname, yend=0)) +
  geom_point( size=4, color="#993366") +
  labs(title = "Correlation with Y100",
       x = "Feature",
       y = "Correlation with Y") +
  coord_flip()
rm(corr)

# Find average coverage
y_hat <- 1
average <- mean(y_hat == dat_all$Y100)

# Rural coverage by county
as.data.frame(prop.table(table(dat_all$county, dat_all$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency)) + 
  geom_segment( aes(xend=Variable, yend=0)) +
  geom_point( size=4, color="#993366") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Rural coverage by county",
       x = "County ID",
       y = "Rural share with 100 Mbit/s coverage") + 
  geom_hline(yintercept = average)

# op_hq
as.data.frame(prop.table(table(dat_all$op_hq, dat_all$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency)) + 
  geom_bar(stat='identity', fill = "#993366") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Rural coverage by operator HQ precense",
       x = "Operator HQ in municipality?",
       y = "Rural share with 100 Mbit/s coverage") + 
  geom_hline(yintercept = average)

# public support
as.data.frame(prop.table(table(dat_all$public_support, dat_all$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency)) + 
  geom_bar(stat='identity', fill = "#993366") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Rural coverage by number of national projects",
       x = "Number of subsidized projects in municipality",
       y = "Rural share with 100 Mbit/s coverage") + 
  geom_hline(yintercept = average)

# core
as.data.frame(prop.table(table(dat_all$core, dat_all$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  filter(Y100 == 1) %>% arrange(Frequency) %>%
  mutate(Variable=factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y=Frequency)) +
  geom_bar(stat='identity', fill = "#993366") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Coverage by 2013 coverage status",
       x = "Municipality had >10% fiber coverage in 2013",
       y = "Rural share with 100 Mbit/s coverage") + 
  geom_hline(yintercept = average)


# b10
as.data.frame(prop.table(table(dat_all$b10, dat_all$Y100),1)) %>%
  rename(Variable = Var1, Y100=Var2, Frequency = Freq) %>%
  ggplot(aes(x = Variable, y=Frequency, fill = Y100)) + geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Rural coverage by 2019 basic broadband status",
       x = "Building has 10 Mbit/s coverage in 2019",
       y = "Rural share with 100 Mbit/s coverage") + 
    geom_hline(yintercept = average)

# Continous distributions

dat_all$y100f <- as.factor(dat_all$Y100)
dat_all %>% ggplot(aes(ave_income, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(centrality, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(pop_urban, colour = y100f)) + geom_density(size=1.2) +
  scale_x_continuous(limits = c(0, 50000))
dat_all %>% ggplot(aes(muni_free_rev, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(muni_debt, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(muni_net, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(employment, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(share_urban2, colour = y100f)) + geom_density(size=1.2)
dat_all %>% ggplot(aes(area_km2, colour = y100f)) + geom_density(size=1.2) +
  scale_x_continuous(limits = c(0, 5000))
dat_all %>% ggplot(aes(muni_homes, colour = y100f)) + geom_density(size=1.2) +
  scale_x_continuous(limits = c(0, 10000))
dat_all %>% ggplot(aes(households, colour = y100f)) + geom_density(size=1.2) +
  scale_x_continuous(limits = c(0, 10000))
dat_all %>% ggplot(aes(virksomheter, colour = y100f)) + geom_density(size=1.2) +
  scale_x_continuous(limits = c(0, 4000))

######### Part 3 ###############
# Modeling
################################

## Make test and training set
set.seed(1, sample.kind = "Rounding")
dat_small <- sample_n(dat_all,10000,replace = FALSE)
# for final round: set dat_small <- dat_all

dat_small <- dat_small %>%
  subset(select = -c(Y100))

test_index <- createDataPartition(y = dat_small$y100f, times = 1, p = 0.1, list = FALSE)
train_set <- dat_small[-test_index,]
test_set <- dat_small[test_index,]

train_set <- as.data.frame(train_set)
test_set <- as.data.frame(test_set)

## The first model - accuracy = 0.55
y_hat <- 0
mu_naive <- mean(y_hat == test_set$y100f)
mu_naive

# LDA - 0.59 - short run time
train_lda <- train(y100f ~ .,method = "lda",data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y100f)$overall["Accuracy"]
lda_imp <- varImp(train_lda)
plot(lda_imp)

# GLM model - works - 0.58 - short run time
train_glm <- train(y100f ~ .,method = "glm",data = train_set)
confusionMatrix(predict(train_glm, test_set), test_set$y100f)$overall["Accuracy"]
glm_imp <- varImp(train_glm)
plot(glm_imp)

# KNN model - works - 0.65 - medium run time
train_knn <- train(y100f ~ ., method = "knn",data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y100f)$overall["Accuracy"]
imp_knn <- varImp(train_knn)
plot(imp_knn)
ggplot(train_knn, highlight = TRUE)


# Random forest - long run time - 0.72
train_rf <- train(y100f ~ ., method = "rf", data = train_set)
confusionMatrix(predict(train_rf, test_set), test_set$y100f)$overall["Accuracy"]
rf_imp <- varImp(train_rf)
plot(rf_imp)
rf_imp

