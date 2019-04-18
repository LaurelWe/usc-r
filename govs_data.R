library(tidyverse)
library(rvest)
library(lubridate)
library(ggalt)

url <- "https://en.wikipedia.org/wiki/List_of_Governors_of_California_by_age"

govs <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(header = NA) %>%
  as.data.frame()

govs$Age.at.inauguration <- NULL
govs$Age.at.endof.term <- NULL
govs$Length.ofretirement <- NULL
govs$Lifespan <- NULL

colnames(govs)[1] <- c("rank_by_age")
colnames(govs)[3] <- c("num_in_office")

govs <- head(govs,40)

govs$Date.of.birth <- gsub("\\[.*","",govs$Date.of.birth)

govs$num_in_office <- as.numeric(govs$num_in_office)

govs$Date.of.birth <- mdy(govs$Date.of.birth)
govs$Date.of.inauguration <- mdy(govs$Date.of.inauguration)
govs$End.ofterm <- mdy(govs$End.ofterm)
govs$Date.of.death <- mdy(govs$Date.of.death)

govs$Date.of.death <- case_when(is.na(govs$Date.of.death) == TRUE ~ Sys.Date(), TRUE ~ govs$Date.of.death)



govs %>%
  ggplot(aes(y = reorder(Governor, num_in_office), x = Date.of.birth, xend = Date.of.death, color = num_in_office)) +
  geom_dumbbell( size_x = 2, size_xend = 2, size = 1.35) +
  labs(title = "The overlapping lives of California governors",
       x = "Year",
       y = "") +
  theme_classic(base_family = "Helvetica") + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))

govs %>% filter(Date.of.birth<mdy("01-01-1900")&Date.of.death>mdy("01-01-1900")) %>% nrow()

alive_stats<- data.frame()

for (i in 1806:2018){
  begin <- mdy(paste("01-01",i,sep=""))
  end <- mdy(paste("01-01",(i+1), sep=""))
  count_alive<-govs %>% filter(Date.of.birth<begin & Date.of.death>end) %>% nrow() %>% as.numeric()
  alive_stats <- rbind(count_alive, alive_stats)
  alive_stats$yr[1]<-i
}


colnames(alive_stats)[1]<-c("count")

alive_stats %>% ggplot(aes(x=yr, y=count))+geom_bar(stat="identity")

cases<-tribble(
  ~Country, ~"2011",~"2012", ~"2013",
  "FR", 7000, 6900, 7000,
  "DE", 5800, 6000, 6200,
  "US", 15000, 14000, 13000
)
head(cases)

cases <- cases %>% gather(key = "year", value ="n", 2:4)
cases %>% ggplot(aes(x=year, y=n, group=Country, color=Country))+geom_line(lwd=3)

pollution <- tribble(
  ~city,   ~size, ~amount,
  "New York", "large",      23,
  "New York", "small",      14,
  "London", "large",      22,
  "London", "small",      16,
  "Beijing", "large",     121,
  "Beijing", "small",     56
)

head(pollution)

pollution %>% spread(key=size, value=amount) %>% mutate(pct_large=large/(large + small)*100)

