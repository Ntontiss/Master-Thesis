#Clear workspace 
rm(list =ls())

#install packages
install.packages("psych")
install.packages("data.table")
library(psych)
library(data.table)
library(ggplot2)

#import voucher data
library(readxl)
voucher <- read_excel("voucher.xlsx")
View(voucher)
summary(voucher)
str(voucher)
describe(voucher)

#delete the empty rows
voucher<- voucher[!apply(is.na(voucher) | voucher=="",1,all), ]

#add the name of variables in the columns
names(voucher)[1] <- "CONTACT_KEY"
names(voucher)[2] <- "MEMBER_KEY"
names(voucher)[3] <- "CARD_NUM"
names(voucher)[4] <- "VOUCHER_NAME"
names(voucher)[5] <- "VOUCHER_START_DT"
names(voucher)[6] <- "VOUCHER_EXPIRY_DT"
names(voucher)[7] <- "VOUCHER_STATUS_CODE"
names(voucher)[8] <- "VOUCHER_USED_DT"
names(voucher)[9] <- "BU_KEY"

#add variables
voucher$VOUCHER_USED<- ifelse(voucher$VOUCHER_STATUS_CODE == "Used" , 1,0)
table(voucher$VOUCHER_USED)

colSums(is.na(voucher)) 

#import demographics
demographics <- read_excel("demographics.xlsx")
View(demographics)
summary(demographics)
#check na's
colSums(is.na(demographics)) 
#remove na's
demographics <- na.omit(demographics)

#rename the column age_num
names(demographics)[3] <- "AGE"

# check for outliers AGE
boxplot(demographics$AGE, main = "demographics - AGE") #OUTLIERS
hist(demographics$AGE) 
boxplot.stats(demographics$AGE)$out
summary(demographics$AGE)


#import online_22
library(readxl)
online_22 <- read_excel(" online_22.xlsx")
View(online_22)
summary(online_22)
str(online_22)
describe(online_22)

#delete the empty rows
online_22<- online_22[!apply(is.na(online_22) | online_22=="",1,all), ]

#add the name of variables in the columns
colnames(online_22)[colnames(online_22) == "1"] <- "CONTACT_KEY"
colnames(online_22)[colnames(online_22) == "2"] <- "ORDER_NUM"
colnames(online_22)[colnames(online_22) == "3"] <- "TRANSACTION_DT"
colnames(online_22)[colnames(online_22) == "4"] <- "STORE_CODE"
colnames(online_22)[colnames(online_22) == "5"] <- "POS_NO"
colnames(online_22)[colnames(online_22) == "6"] <- "ICI_MEMBERS_SALES"
colnames(online_22)[colnames(online_22) == "7"] <- "PAYMENT_AMT"
colnames(online_22)[colnames(online_22) == "8"] <- "MEMBER_TYPE"
colnames(online_22)[colnames(online_22) == "9"] <- "PRODUCT_SKU"
colnames(online_22)[colnames(online_22) == "10"] <- "BRAND_NAME"
colnames(online_22)[colnames(online_22) == "11"] <- "PRODUCT_HIER_1_L1_NAME"
colnames(online_22)[colnames(online_22) == "12"] <- "PRODUCT_HIER_1_L2_NAME"
colnames(online_22)[colnames(online_22) == "13"] <- "PRODUCT_HIER_1_L3_NAME"
colnames(online_22)[colnames(online_22) == "14"] <- "ITEM_QUANTITY"
colnames(online_22)[colnames(online_22) == "15"] <- "ORIGINAL_PRICE"
colnames(online_22)[colnames(online_22) == "16"] <- "DIS_PRICE"
colnames(online_22)[colnames(online_22) == "17"] <- "PROMO_KEY"
colnames(online_22)[colnames(online_22) == "18"] <- "POS_PROMO_NUM"
colnames(online_22)[colnames(online_22) == "19"] <- "PROMO_NAME"

#store_cod and pos_no since we dont need them
online_22$STORE_CODE <- NULL
online_22$POS_NO <- NULL

#transformation the variables into appropriate format
online_22$ICI_MEMBERS_SALES<- as.numeric(online_22$ICI_MEMBERS_SALES)
online_22$PAYMENT_AMT<- as.numeric(online_22$PAYMENT_AMT)
online_22$PRODUCT_SKU<- as.numeric(online_22$PRODUCT_SKU)
online_22$ITEM_QUANTITY<- as.numeric(online_22$ITEM_QUANTITY)
online_22$ORIGINAL_PRICE<- as.numeric(online_22$ORIGINAL_PRICE)
online_22$DIS_PRICE<- as.numeric(online_22$DIS_PRICE)

# Split the TRANSACTION_DT  column into date columns
online_22$TRANSACTION_DATE<- as.Date(sapply(strsplit(online_22$TRANSACTION_DT , " "), "[", 1), format = "%d-%m-%Y")
class(online_22$TRANSACTION_DATE)

#create a new  data frame for 2022, only.
table(online_22$TRANSACTION_DATE)
online_22<- online_22[online_22$TRANSACTION_DATE >= "2022-01-01" , ]

# Create a vector of unique contact keys from the voucher dataset
voucher_contact_keys <- voucher$CONTACT_KEY
# Add a new column to the online_21 dataset indicating if the customer possesses a voucher
online_22$HAS_VOUCHER <- ifelse(online_22$CONTACT_KEY %in% voucher_contact_keys, 1, 0)
#when 0 it means that contact key from online_21 is not in the data set of voucher_21

#replace the na from the other columns with 0, when the customer do not posses a voucher
online_22 <- online_22[online_22$HAS_VOUCHER == 1, ]

library(dplyr)
#create a new subset of online 21 only with the variables that we need
online_22 <- online_22 %>%
  dplyr::select(CONTACT_KEY, ORDER_NUM, TRANSACTION_DATE, ITEM_QUANTITY, ORIGINAL_PRICE, DIS_PRICE, HAS_VOUCHER)

#check na's
colSums(is.na(online_22)) #we have na's because they did not purchase anything so we remove them
#remove na's
online_22 <- na.omit(online_22)

# Create a vector of unique contact keys from the voucher dataset
online_22_contact_keys <- online_22$CONTACT_KEY
# Add a new column to the voucher dataset indicating if the customer made a purchase
voucher$PURCHASE <- ifelse(voucher$CONTACT_KEY %in% online_22_contact_keys, 1, 0)
#when 0 it means that contact key from voucher is not in the data set of online22
#replace the na from the other columns with 0, when the customer do not posses a voucher
voucher <- voucher[voucher$PURCHASE == 1, ]

#create a new subset of voucher only with the variables that we need
voucher <- voucher %>% 
  dplyr::select(CONTACT_KEY,VOUCHER_USED,PURCHASE)

#check na's
colSums(is.na(voucher)) 

# Define the columns you want to select from 'demographics'
demographics <- demographics %>% 
  dplyr::select(CONTACT_KEY, AGE)

#check na's
colSums(is.na(demographics)) 

# Merge the data using the contact key
online_22 <- left_join(online_22, voucher, by = "CONTACT_KEY")
online_22<- left_join(online_22, demographics, by = "CONTACT_KEY")

#check na's
colSums(is.na(online_22)) #we have na's because they did not purchase anything so we remove them
#remove na's
online_22 <- na.omit(online_22)

#remove duplicates
online_22 <- online_22 %>%
  distinct(CONTACT_KEY,TRANSACTION_DATE,ORIGINAL_PRICE, .keep_all = TRUE)


#Check for outliers
#ITEM_QUANTITY_VAL
boxplot(online_22$ITEM_QUANTITY, main = "online_22 - ITEM_QUANTITY")
hist(online_22$ITEM_QUANTITY) 
boxplot.stats(online_22$ITEM_QUANTITY)$out # outliers,but they are realistic
summary(online_22$ITEM_QUANTITY)

#ORIGINAL_PRICE
boxplot(online_22$ORIGINAL_PRICE, main = "online_22 - ORIGINAL_PRICE")
hist(online_22$ORIGINAL_PRICE) 
boxplot.stats(online_22$ORIGINAL_PRICE)$out # outliers,but they are realistic
summary(online_22$ORIGINAL_PRICE)


summary(lm(ITEM_QUANTITY ~ ORIGINAL_PRICE, online_22))
ggplot(online_22, aes(x=ITEM_QUANTITY,y=ORIGINAL_PRICE)) + stat_smooth(method="lm",col="red")
# The lowest the price the more likely a customer is to buy the product.

#DIS_PRICE
boxplot(online_22$DIS_PRICE, main = "online_22 - DIS_PRICE- before trimmed")
hist(online_22$DIS_PRICE,main = "online_22 - DIS_PRICE- before trimmed") 
boxplot.stats(online_22$DIS_PRICE)$out 
summary(online_22$DIS_PRICE)

summary(lm(ITEM_QUANTITY ~ DIS_PRICE, online_22))
ggplot(online_22, aes(x=ITEM_QUANTITY,y=DIS_PRICE)) + stat_smooth(method="lm",col="red")
# The lowest the discount price the more likely a customer is to buy the product.

#check how many observations of discount price exceed the original price.
# Find observations where discount price is higher than original price
higher_discount <- online_22$DIS_PRICE > online_22$ORIGINAL_PRICE

# Count the number of observations where discount price is higher
count_higher_discount <- sum(higher_discount)

# Total number of observations
total_observations <- nrow(online_22)

# Calculate the percentage
percentage_higher_discount <- (count_higher_discount / total_observations) * 100

# Print the result
print(paste("Percentage of observations where discount price is higher:", percentage_higher_discount, "%"))

#this code solve the problem that discount price is higher than original price, which does not make sense
#higher values of discount price equal to the original price
online_22$DIS_PRICE[online_22$DIS_PRICE > online_22$ORIGINAL_PRICE] <- online_22$ORIGINAL_PRICE[online_22$DIS_PRICE > online_22$ORIGINAL_PRICE]
summary(online_22$DIS_PRICE)

#now the outliers are considered as realistic
#DIS_PRICE
boxplot(online_22$DIS_PRICE, main = "online_22 - DIS_PRICE- after trimmed")
hist(online_22$DIS_PRICE,main = "online_22 - DIS_PRICE- after trimmed") 
boxplot.stats(online_22$DIS_PRICE)$out 
summary(online_22$DIS_PRICE)

# add new variable
online_22$REWARD_VALUE <- online_22$ORIGINAL_PRICE - online_22$DIS_PRICE
summary(online_22$REWARD_VALUE)

#PRICE_VALUE
boxplot(online_22$REWARD_VALUE, main = "online_22 - REWARD_VALUE")
hist(online_22$REWARD_VALUE) 
boxplot.stats(online_22$REWARD_VALUE)$out # outliers but are realistic
summary(online_22$REWARD_VALUE)

#Check if the outliers of REWARD_VALUE impact the item quantity
summary(lm(ITEM_QUANTITY ~ REWARD_VALUE, online_22))
ggplot(online_22, aes(x=ITEM_QUANTITY,y=REWARD_VALUE)) + stat_smooth(method="lm",col="red")

# AGE
boxplot(online_22$AGE, main = "demographics - AGE") #OUTLIERS
hist(online_22$AGE) 
boxplot.stats(online_22$AGE)$out # outliers, but they are realistic
summary(online_22$AGE)

#truncate the outliers of age_num
sum <- summary(online_22$AGE)
iqr <- sum[5] - sum[2]
llimit <- sum[2] - 1.5 * iqr
ulimit <- sum[5] + 1.5 * iqr

online_22$AGE <- ifelse(online_22$AGE > ulimit, ulimit, online_22$AGE)
online_22$AGE <- ifelse(online_22$AGE < llimit, llimit, online_22$AGE)
summary(online_22$AGE)

#check
multiple_order_num <- online_22 %>%
  group_by(CONTACT_KEY) %>%
  summarize(unique_order_num = n_distinct(ORDER_NUM)) %>%
  filter(unique_order_num > 1)
# Calculate the percentage of ORDER_NUM with multiple CONTACT_KEYs
percentage <- (nrow(multiple_order_num) / n_distinct(online_22$CONTACT_KEY)) * 100

cat("Percentage of ORDER_NUM with multiple CONTACT_KEYs: ", percentage, "%")

# Install corrplot package (if not installed)
install.packages("corrplot")

# Load corrplot package
library(corrplot)

#check correlations
colnames(online_22)
online_22 <- na.omit(online_22)
subset_df22 <- online_22[,c("ITEM_QUANTITY","REWARD_VALUE", "VOUCHER_USED","AGE")]
correlation_matrix <- cor(subset_df22)
corrplot(correlation_matrix, method = "circle", main = "\n Correlation plot - Year 2022", addCoef.col = "black")

# Check for correlation and multicollinearity
correlation_matrix <- cor(online_22[c( "ITEM_QUANTITY","REWARD_VALUE", "VOUCHER_USED","AGE")])
print(correlation_matrix)

#check for multicollinearity
library(car)
vif<- glm(ITEM_QUANTITY~ VOUCHER_USED+ AGE+REWARD_VALUE,data = online_22, family = "poisson")
vif(vif)


#obtain some descriptives
mean(online_22$ITEM_QUANTITY)
var(online_22$ITEM_QUANTITY)
#the mean > variance so we have underdispresion so we will use poisson regression

#Poisson regression
#test poisson for each variable without interaction terms
m1<- glm(ITEM_QUANTITY~REWARD_VALUE+ VOUCHER_USED+AGE,data = online_22, family = "poisson")
summary(m1)

#exponentiate coefs
exp(coef(m1))

#test poisson for each variable  interaction terms
m2<- glm(ITEM_QUANTITY~REWARD_VALUE*VOUCHER_USED+ REWARD_VALUE*AGE,data = online_22, family = "poisson")
summary(m2)

#exponentiate coefs
exp(coef(m2))


#as item quantity is by definition larger than 0, use truncated poisson model
install.packages("VGAM")
library(VGAM)

#estimated zero truncated poisson model
#with interaction term because we are interesting to see with the interaction term how it affect the dv
m3<-vglm(ITEM_QUANTITY~REWARD_VALUE*VOUCHER_USED+REWARD_VALUE*AGE,family=negbinomial(),data=online_22)
summary(m3)
exp(coef(m3))

#model fit and model selection
#calculate null model
nullmodel<-glm(ITEM_QUANTITY~1,data=online_22,family="poisson")
#LR test
anova(nullmodel,m1,test="Chisq")
#AIC and BIC
AIC(nullmodel,m1)
BIC(nullmodel,m1)
#m1 fits better

#LR test null model with model 2
anova(nullmodel,m2,test="Chisq")
#AIC and BIC
AIC(nullmodel,m2)
BIC(nullmodel,m2)
#m2 fits better

#LR test for model 1 and model 2
anova(m1,m2,test="Chisq")
#m2 model fits better than m1
#AIC and BIC
AIC(m1,m2)
#model 2 for AIC
BIC(m1,m2)
#model 1 for BIC


#LR test for model 1 and model 2
anova(nullmodel,m3,test="Chisq")
anova(m1,m3,test="Chisq")
anova(m2,m3,test="Chisq")

# AIC
AIC(m2)
AIC(m3)
#model 2 fits better than model 3
# BIC
BIC(m1)
BIC(m3)
#model 1 fits better than model 3
