# Install Packages
# Data Import
install.packages("readr", dependencies = c("Depends", "Suggests"))
install.packages("data.table", dependencies = c("Depends", "Suggests"))
install.packages("readxl", dependencies = c("Depends", "Suggests"))
install.packages("gdata", dependencies = c("Depends", "Suggests"))
install.packages("XLConnect", dependencies = c("Depends", "Suggests"))
install.packages("RMySQL", dependencies = c("Depends", "Suggests"))
install.packages("httr", dependencies = c("Depends", "Suggests"))
# Data Manipulation
install.packages("dplyr", dependencies = c("Depends", "Suggests"))
install.packages("ggplot2", dependencies = c("Depends", "Suggests"))
# Data Visualization
install.packages("ggvis", dependencies = c("Depends", "Suggests"))



library(dplyr)
library(data.table)
library(ggplot2)
set.seed(1986)


# 원천 데이터(제공됌)
raw.data <- 
  data.frame(
    product = sample(rep(LETTERS[c(1:4)],c(30,40,30,20))))%>%
  mutate(
    making.year = sample(rep(2017:2014,c(30,40,30,20))),
    repair.year = making.year+sample(1:4)) %>%
  arrange(product, making.year, repair.year)

head(tbl_df(raw.data))
tail(tbl_df(raw.data))


# 상품별 집계(table 함수)후 data.frame화 -> 컬럼명 바꿔주고 정렬 
data <-
  raw.data %>%
  table(.) %>%
  as.data.frame(.) %>%
  setnames(c(names(.[,1:(ncol(.)-1)]),'count')) %>%
  arrange(product, repair.year, making.year)


# 비율 계산을 위한 total
total <-
  data %>%
  group_by(product, making.year) %>%
  summarise(total = sum(count)) %>%
  arrange(product, making.year)

head(total)
head(data)

# 비율
ratio.data <-
  data %>%
  inner_join(total, by=c('product','making.year')) %>%
  mutate(ratio=count/total)


# % 라벨 포맷
format.ratio <- function(x) paste(floor(x*100),'%',sep='')


# chart : legend title에 대한 vjust가 먹히지 않아서 줄바꿈 문자 '\n' 으로 해결
ratio.data %>%
  ggplot(aes(x=making.year, y=repair.year, fill=ratio)) +
  geom_tile() +
  geom_text(
    aes(label=case_when(
      ratio==0 ~ '', 
      TRUE ~ format.ratio(ratio)))) +
  scale_fill_continuous(
    high='red', 
    low='white', 
    label=format.ratio) +
  facet_grid(~ paste(product,'제품')) +
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    axis.title.y=element_text(angle=0))