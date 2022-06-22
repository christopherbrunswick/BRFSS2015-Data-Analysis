#loading tidyverse package
suppressPackageStartupMessages(library(tidyverse))

#loading olsrr package
suppressPackageStartupMessages(library(olsrr))

#loading psych package
suppressPackageStartupMessages(library(psych))

data <- read_csv('BRFSS2015.csv', show_col_types = FALSE)

Q1 <- data %>% select(HLTHPLN1) %>% filter(HLTHPLN1 == 1) %>% count()
Q2 <- data %>% filter(c(MENTHLTH == 88 | MENTHLTH <= 30), STATE == 42) %>% 
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>% 
  summarize(avg_NDMHNG = round(mean(MENTHLTH, na.rm = TRUE), 2))

Q3 <- data %>% select(HAVARTH3, WTKG3) %>% 
  filter(HAVARTH3 == 1 | HAVARTH3 == 2) %>% 
  filter(WTKG3 != 99999) %>% mutate(wtkg3_to_lbs = WTKG3 * 2.20462) %>% 
  group_by(HAVARTH3) %>% 
  summarize(mean_weight = round(mean(wtkg3_to_lbs * .01), 2), sd_weight = round(sd(wtkg3_to_lbs * .01), 2)) %>% 
  data.frame()

convert_fctr_data <- data %>% mutate(MARITAL = as.factor(MARITAL)) %>% filter(PA1MIN_ != "NA") %>% filter(MARITAL != 9)
PA1MIN_upper <- quantile(convert_fctr_data$PA1MIN_, 0.997, na.rm = TRUE)
PA1MIN_lower <- quantile(convert_fctr_data$PA1MIN_, 0.003, na.rm = TRUE)
PA1MIN_out <- which(convert_fctr_data$PA1MIN_ > PA1MIN_upper | convert_fctr_data$PA1MIN_ < PA1MIN_lower)

Q4 <- round((nrow(convert_fctr_data) - length(PA1MIN_out))/nrow(convert_fctr_data)*100, 2)
Q5 <- convert_fctr_data %>% group_by(MARITAL) %>% summarize(total_ex_mean = round(mean(PA1MIN_), 2),
                                                            total_ex_std = round(sd(PA1MIN_), 2),
                                                            total_ex_min = round(min(PA1MIN_), 2),
                                                            total_ex_max = round(max(PA1MIN_), 2))

Q6 <- ggplot(convert_fctr_data, aes(PA1MIN_, MARITAL)) + geom_boxplot() + scale_y_discrete(labels = c("Married", "Divorced", "Widowed", "Seperated", "Never married", "A member of an unmarried couple"))

data_mod1 <- lm(PA1MIN_ ~ MARITAL, data = convert_fctr_data)
Q7 <- summary(data_mod1)

data_anov1 <- aov(formula = PA1MIN_ ~ MARITAL, data = convert_fctr_data)
Q8 <- TukeyHSD(data_anov1)

convert_new <- convert_fctr_data %>% filter(FRUTSUM_ != "NA")
data_mod2 <- lm(PA1MIN_ ~ MARITAL + FRUTSUM_, data = convert_new)
summary(data_mod2)

modelBest_possible <- ols_step_all_possible(data_mod2)
modelBest_possible
modelBest_subset <- ols_step_best_subset(data_mod2)
modelBest_subset


Q9 <- AIC(data_mod2, k = 2)

#For the final section, you will choose four variables to explore we
#we previously have not.

#In the code beneath I chose 4 categorical variables 
#Q10
new_dataset <- data %>% select(RFSEAT3_, EMPLOY1, CHLDCNT_, RFHLTH_) %>% 
  filter(EMPLOY1 != 9 | EMPLOY1 < 9 & CHLDCNT_ != 9 & RFHLTH_ != 9 & RFSEAT3_ != 9 | RFSEAT3_ < 9) %>% 
  filter(RFHLTH_ != 9) %>% filter(RFSEAT3_ != 9) %>% filter(EMPLOY1 != 9) %>% 
  filter(CHLDCNT_ != 9)

Q10 <- new_dataset

#Since the variables I chose were categorical by nature 
#(to make sure they were categorical in the dataframe, i coerced them into factors
#just so that it would show in the dataframe), 
#removing outliers would be irrelevant since all categories are needed. 
#However, I did remove missing values, didn't ask values, 
#don't know values, and refused to answer values. I had to filter some of
#the same variables twice because they failed to filter properly the 
#first time.

#Q11
convertd_final_dataset <- new_dataset %>% mutate(RFSEAT3_ = as.factor(RFSEAT3_),
                                                 EMPLOY1 = as.factor(EMPLOY1),
                                                 CHLDCNT_ = as.factor(CHLDCNT_),
                                                 RFHLTH_ = as.factor(RFHLTH_))

levels(convertd_final_dataset$CHLDCNT_) <- c(0,1,2,3,4,5)

Q11 <- convertd_final_dataset

#The CHLDCNT_ variable has a "none" value label equal to 1 instead of 0. 
#Also, the levels of CHLDCNT_ are c(1,2,3,4,5,6). I changed the levels to 
#c(0,1,2,3,4,5) so that 0 can be interpreted as "none" or "nothing" instead of 1
#and so that the levels can match the value label accordingly. This is seen
#in the convertd_final_dataset

#Q12 - data visualizations 
RFSEAT3_labs <- c("Always Wear Seat Belt", "Don't Always Wear Seat Belt")
EMPLOY1_labs <- c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")
RFHLTH_labs <- c("Good or Better Health", "Fair or Poor Health")
CHLDCNT_labs <- c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")

graphOne <- ggplot(convertd_final_dataset, aes(RFSEAT3_)) + geom_bar() + scale_x_discrete(labels = RFSEAT3_labs) + coord_flip()
graphOne

graphTwo <- ggplot(convertd_final_dataset, aes(RFSEAT3_)) + geom_bar(aes(fill = EMPLOY1), position = "dodge") + scale_x_discrete(labels = RFSEAT3_labs) + scale_fill_discrete(labels = c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")) + coord_flip()
graphTwo

graphThree <- ggplot(convertd_final_dataset, aes(RFSEAT3_)) + geom_bar(aes(fill= RFHLTH_), position = "dodge") + scale_x_discrete(labels = RFSEAT3_labs) + scale_fill_discrete(labels = c("Good or Better Health", "Fair or Poor Health")) + coord_flip()
graphThree

graphFour <- ggplot(convertd_final_dataset, aes(RFSEAT3_)) + geom_bar(aes(fill = CHLDCNT_), position = "dodge") + scale_x_discrete(labels = RFSEAT3_labs) + scale_fill_discrete(labels = c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")) + coord_flip()
graphFour

graphFive <- ggplot(convertd_final_dataset, aes(EMPLOY1)) + geom_bar() + scale_x_discrete(labels = EMPLOY1_labs) + coord_flip()
graphFive

graphSix <- ggplot(convertd_final_dataset, aes(EMPLOY1)) + geom_bar(aes(fill = RFSEAT3_), position = "dodge") + scale_x_discrete(labels = c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")) + scale_fill_discrete(labels = c("Always Wear Seat Belt", "Don't Always Wear Seat Belt")) + coord_flip()
graphSix

graphSeven <- ggplot(convertd_final_dataset, aes(EMPLOY1)) + geom_bar(aes(fill = RFHLTH_), position = "dodge") + scale_x_discrete(labels = c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")) + scale_fill_discrete(labels = c("Good or Better Health", "Fair or Poor Health")) + coord_flip()
graphSeven

graphEight <- ggplot(convertd_final_dataset, aes(EMPLOY1)) + geom_bar(aes(fill = CHLDCNT_), position = "dodge") + scale_x_discrete(labels = c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")) + scale_fill_discrete(labels = c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")) + coord_flip()
graphEight

graphNine <- ggplot(convertd_final_dataset, aes(CHLDCNT_)) + geom_bar() + scale_x_discrete(labels = CHLDCNT_labs) + coord_flip()
graphNine

graphTen <- ggplot(convertd_final_dataset, aes(CHLDCNT_)) + geom_bar(aes(fill = RFSEAT3_), position = "dodge") + scale_x_discrete(labels = c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")) + scale_fill_discrete(labels = c("Always Wear Seat Belt", "Don't Always Wear Seat Belt")) + coord_flip()
graphTen

graphEleven <- ggplot(convertd_final_dataset, aes(CHLDCNT_)) + geom_bar(aes(fill = EMPLOY1), position = "dodge") + scale_x_discrete(labels = c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")) + scale_fill_discrete(labels = c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")) + coord_flip()
graphEleven

graphTwelve <- ggplot(convertd_final_dataset, aes(CHLDCNT_)) + geom_bar(aes(fill = RFHLTH_), position = "dodge") + scale_x_discrete(labels = c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")) + scale_fill_discrete(labels = c("Good or Better Health", "Fair or Poor Health")) + coord_flip()
graphTwelve

graphThirteen <- ggplot(convertd_final_dataset, aes(RFHLTH_)) + geom_bar() + scale_x_discrete(labels = RFHLTH_labs)
graphThirteen

graphFourteen <- ggplot(convertd_final_dataset, aes(RFHLTH_)) + geom_bar(aes(fill = RFSEAT3_), position = "dodge") + scale_x_discrete(labels = c("Good or Better Health", "Fair or Poor Health")) + scale_fill_discrete(labels = c("Always Wear Seat Belt", "Don't Always Wear Seat Belt"))
graphFourteen

graphFifteen <- ggplot(convertd_final_dataset, aes(RFHLTH_)) + geom_bar(aes(fill = EMPLOY1), position = "dodge") + scale_x_discrete(labels = c("Good or Better Health", "Fair or Poor Health")) + scale_fill_discrete(labels = c("Employed for wages", "Self-employed", "Out of work for 1 year or more", "Out of work for less than 1 year", "A homemaker", "A student", "Retired", "Unable to work")) + coord_flip()
graphFifteen

graphSixteen <- ggplot(convertd_final_dataset, aes(RFHLTH_)) + geom_bar(aes(fill = CHLDCNT_), position = "dodge") + scale_x_discrete(labels = c("Good or Better Health", "Fair or Poor Health")) + scale_fill_discrete(labels = c("No children in household", "One child in household", "Two children in household", "Three children in household", "Four children in household", "Five or more children in household")) + coord_flip()
graphSixteen

#Q13
Q13 <- describe(convertd_final_dataset)


convertd_glm <- convertd_final_dataset %>% mutate(Rfhlthbin = ifelse(RFHLTH_ == 1, 0, 1)) 

convertd_glm_mod1 <- glm(Rfhlthbin ~ CHLDCNT_ + RFSEAT3_ + EMPLOY1, binomial(), data = convertd_glm)

summary(convertd_glm_mod1)

convertd_glm_mod2 <- glm(Rfhlthbin ~ CHLDCNT_ + RFSEAT3_, binomial(), data = convertd_glm)
summary(convertd_glm_mod2)

convertd_glm_mod3 <- glm(Rfhlthbin ~ CHLDCNT_, binomial(), data = convertd_glm)
summary(convertd_glm_mod3)

convertd_glm_mod4 <- glm(Rfhlthbin ~ RFSEAT3_, binomial(), data = convertd_glm)
summary(convertd_glm_mod4)

convertd_glm_mod5 <- glm(Rfhlthbin ~ EMPLOY1, binomial(), data = convertd_glm)
summary(convertd_glm_mod5)

c(AIC(convertd_glm_mod1, k=2), 
  AIC(convertd_glm_mod2, k=2),
  AIC(convertd_glm_mod3, k=2),
  AIC(convertd_glm_mod4, k=2),
  AIC(convertd_glm_mod5, k=2))

Q14 <- AIC(convertd_glm_mod1, k=2)