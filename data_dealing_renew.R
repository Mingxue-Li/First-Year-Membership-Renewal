library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate) 


all = left_join(r_members, r_member_pos, by = 'MEMBERSHIP_ID')
all$date_max = ymd(all$date_max)
all$date_min = ymd(all$date_min)


all$sales[is.na(all$sales)] <- 0
all$qty[is.na(all$qty)] <- 0
all$cost[is.na(all$cost)] <- 0
all$num[is.na(all$num)] <- 0
all$num_visit[is.na(all$num_visit)] <- 0
all$num_item[is.na(all$num_item)] <- 0
all$num_category[is.na(all$num_category)] <- 0
all$num_sub_category[is.na(all$num_sub_category)] <- 0


all_sel= all%>%
  select( -COHORT_MONTH, 
         -MEMBERSHIP_TYPE_DESC, -TENURE_GRP, -LAST_RENEW_DATE,
         -RENEW_DATE, -NEXT_RENEW_DATE, -PLUS_STATUS_BEFORE_REN, 
         -PLUS_STATUS_AFTER_REN, -PLUS_UPGRADE_DATE)

all_sel$marital_status_desc = ifelse(all_sel$marital_status_desc == 'Married',1,0)
all_sel$ethnic_desc = ifelse(all_sel$ethnic_desc == 'African Americn', 0,
                             ifelse(all_sel$ethnic_desc == 'Asian Other', 1,
                                    ifelse(all_sel$ethnic_desc == 'Chinese', 2,
                                           ifelse(all_sel$ethnic_desc == 'Eastrn European', 3,
                                                  ifelse(all_sel$ethnic_desc == 'Greek', 4,
                                                         ifelse(all_sel$ethnic_desc == 'Hispanic', 5,
                                                                ifelse(all_sel$ethnic_desc == 'Jewish', 6,
                                                                       ifelse(all_sel$ethnic_desc == 'Korean', 7,
                                                                              ifelse(all_sel$ethnic_desc == 'Middle Eastern', 8,
                                                                                    ifelse(all_sel$ethnic_desc == 'Native Americn', 9,
                                                                                           ifelse(all_sel$ethnic_desc == 'Polynesian', 10,
                                                                                                  ifelse(all_sel$ethnic_desc == 'Scandinavian', 11,
                                                                                                         ifelse(all_sel$ethnic_desc == 'Unknown', 12,
                                                                                                                ifelse(all_sel$ethnic_desc == 'Vietnamese', 13, 14))))))))))))))




all_sel$RENEW_IND = ifelse(all_sel$RENEW_IND == 'UNRENEWED', 0,
                           ifelse(all_sel$RENEW_IND == 'RENEWED BASE', 1,2))
all_sel$PLUS_MEMBERSHIP_IND = ifelse(all_sel$PLUS_MEMBERSHIP_IND == 'N',0,1)

all_sel$autorenew_ind = ifelse(all_sel$autorenew_ind == 'N',0,1)
all_sel$payroll_deduct_ind = ifelse(all_sel$payroll_deduct_ind == 'N',0,1)


write.csv(all_sel, file = 'all_member.csv')

data = read.csv('all_member.csv')
data$days_dif = ymd('2018-1-31') - ymd(data$date_max)
data$freq = (ymd(data$date_max) - ymd(data$date_min))/data$num_visit
write.csv(data, file = 'all_member_segment.csv')

#### use renew ID as 012 to do the segmentation, and also include the days_diff



#############
data$RENEW_IND= ifelse(data$RENEW_IND == 0, 0,1)

#### use renew ID as 01 to increase the accuracy of the regression model


write.csv(data, file = 'all_member01.csv')












