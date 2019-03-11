
library(data.table)
library(dplyr)
library(lubridate)


r_dmm_gmm = fread('r_dmm_gmm.txt',sep = '|')
r_pos = fread('r_pos.txt', sep = '|')
r_tender_type = fread('r_tender_type.txt',sep = '|')


r_member_pos = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(sales = sum(RETAIL_PRICE))

r_member_pos_qty = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(qty = sum(UNIT_QTY)) 

r_member_pos_cost = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(cost = sum(UNIT_QTY*UNIT_COST)) 

r_member_pos_num = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(num = n())

r_member_pos_num_visit = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(num_visit = n_distinct(VISIT_NBR))

r_member_pos_date_min = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(date_min = min(VISIT_DATE))

r_member_pos_date_max = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(date_max = max(VISIT_DATE))

r_member_pos_num_item = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(num_item = n_distinct(SCAN_ID))

r_member_pos_num_category = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(num_category = n_distinct(CATEGORY_NBR))

r_member_pos_num_sub_category = r_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(num_sub_category = n_distinct(SUB_CATEGORY_NBR))



r_member_pos$qty = r_member_pos_qty$qty
r_member_pos$cost = r_member_pos_cost$cost
r_member_pos$num = r_member_pos_num$num
r_member_pos$date_min = r_member_pos_date_min$date_min
r_member_pos$date_max = r_member_pos_date_max$date_max
r_member_pos$num_visit = r_member_pos_num_visit$num_visit
r_member_pos$num_item = r_member_pos_num_item$num_item
r_member_pos$num_category = r_member_pos_num_category$num_category
r_member_pos$num_sub_category = r_member_pos_num_sub_category$num_sub_category

save(r_member_pos, file= 'r_member_pos.rda')


#### join tender_type

r_tender_type$TENDER_TYPE_DESC = factor(r_tender_type$TENDER_TYPE_DESC)
r_tender_type$TENDER_TYPE = factor(r_tender_type$TENDER_TYPE)

levels(r_tender_type$TENDER_TYPE_DESC)

pos_small = r_pos %>%
  select(MEMBERSHIP_ID, VISIT_NBR) 

tender_small = r_tender_type %>%
  select(VISIT_NBR, TENDER_TYPE_DESC, TENDER_AMT)



r_join = left_join(pos_small, tender_small, by = 'VISIT_NBR')
r_join_group = r_join %>%
  group_by(MEMBERSHIP_ID, TENDER_TYPE_DESC) %>%
  summarise(sum = sum(TENDER_AMT))


x = r_join_group 
y = x %>%
  select(MEMBERSHIP_ID, TENDER_TYPE_DESC,sum)
z = reshape(y, idvar = 'MEMBERSHIP_ID', timevar ='TENDER_TYPE_DESC',direction="wide" )

c(colnames(z))

write.csv(z, file = 'member_join_tender.csv')

