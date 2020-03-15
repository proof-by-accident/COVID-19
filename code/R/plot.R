library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

dat.dir = '/home/peter/Junkspace/covid19/csse_covid_19_data/csse_covid_19_time_series/'
dat = read.csv(paste0(dat.dir,'time_series_19-covid-Confirmed.csv'))
names(dat)[-(1:4)] = names(dat)[-(1:4)] %>% sapply(function(x){strsplit(x,'X')[[1]][2]}) %>% as.character

dat %<>% melt(id.vars=names(dat)[1:4])
names(dat)[5:6] = c('date','counts')
dat$date %<>%mdy
us.dat = dat %>% dplyr::filter(Country.Region=='US')
us.dat.tots = us.dat %>% group_by(date) %>% summarize(sum(counts))
names(us.dat.tots)[2] = 'counts'

states = (us.dat$Province.State %>% unique)[1:52]
states = states[states!="Grand Princess"]
states = states[states!="Diamond Princess"]

state.dat = us.dat %>% filter(Province.State %in% states)
worst.states = state.dat %>% group_by(Province.State) %>%
  summarize(tot=sum(counts)) %>%
  arrange(desc(tot)) %>%
  filter(tot > quantile(tot,.9)) %>%
  select(Province.State) %>%
  unlist


#fit log-model
mod = lm(log10(counts) ~ as.numeric(date), data = us.dat.tots %>% filter(month(date)>=3))
log.r2 = summary(mod)$r.squared
log.coef = summary(mod)$coefficients[2,1]
preds = mod %>% predict
us.dat.tots$estim = c(rep(NA,nrow(us.dat.tots)-length(preds)),preds)

ti = paste0('Daily growth rate: ',
            as.character(100*round(10^(log.coef)-1,2)),'%')
            #', Regression R^2: ',
            #as.character(round(log.r2,2)))
plt.us = ggplot(us.dat.tots%>% filter(date>ymd('2020-02-15')),aes(x=date)) +
  geom_point(aes(y=log10(counts))) +
  geom_line(aes(y=estim),color='red') +
  labs(x='Date',y='log10(Cases)',title=ti) +
  theme(text=element_text(size=16))


plt.states = ggplot(state.dat%>%filter(date>ymd('2020-03-06') & Province.State %in% worst.states), aes(x=date,y=counts,group=Province.State,color=Province.State)) +
  geom_line() + 
  theme(legend.position=c(.3,.6),
  legend.text=element_text(size=16),
  text=element_text(size=16)) +
  labs(x='Date',y='Confirmed Cases',colour='State')

