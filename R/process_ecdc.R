process_ecdc <- function() {
#Define ST categories
NOVAX= 'NOVAX';
pcv7stmac=c('4', '14' ,'18C', '19F', '6B' ,'6A/C' ,'6A', '9V', '23F')
pcv13stmac=c(pcv7stmac, '1' ,'3' ,'5', '7F' ,'19A','6C')
pcv15stmac=c(pcv13stmac, '22F' ,'33F')

##EU
eu1a <- read.csv('./Data/ECDC/ECDC_surveillance_data_Invasive_pneumococcal_disease u12m.csv') %>%
  mutate(agegrp=1) 
eu1b <- read.csv('./Data/ECDC/ECDC_surveillance_data_Invasive_pneumococcal_disease 1_4y.csv')%>%
  mutate(agegrp=1)
eu1c <- read.csv('./Data/ECDC/ECDC_surveillance_data_Invasive_pneumococcal_disease age65plus.csv')%>%
  mutate(agegrp=5)
eu1 <- bind_rows(eu1a, eu1b,eu1c) %>%
  filter(Distribution=="Distribution by serotype") %>%
  rename(percent=Value)

eu2a <- read.csv('./Data/ECDC/ECDC_N_country_year_u1y.csv')
eu2b <- read.csv('./Data/ECDC/ECDC_N_country_year_1-4y.csv')
eu2c <- read.csv('./Data/ECDC/ECDC_N_country_year_65plus.csv')
eu2 <- bind_rows(eu2a, eu2b, eu2c) %>%
  select(Population, Time, RegionCode,NumValue) %>%
  rename(Total_cases = NumValue)

#combine by age group
eu3 <- merge(eu1, eu2, by=c('Population', 'Time','RegionCode')) %>%
  rename(year=Time, st=Category, country=RegionName) %>%
  filter(year>=2017 & country %in% c("EU/EEA","United Kingdom",'France','Spain','Italy','Belgium','Denmark','Finland' ,'Austria','Netherlands','Norway','Sweden')) %>%
  mutate(percent=as.numeric(percent),
         N_st= Total_cases*percent/100, 
         period=5,
         st = if_else(st =='15','15B/C', st), #guess...
         PCV7st = if_else(st %in% pcv7stmac,1,0),
         PCV13st = if_else(st %in% pcv13stmac,1,0),
         PCV15st = if_else(st %in% pcv15stmac,1,0)) %>%
  group_by(country,agegrp, st, period) %>%
  summarize(N_cases=sum(N_st, na.rm = T)) %>%
  ungroup() %>%
  group_by( country, agegrp,period) %>%
  mutate(pct= N_cases/sum(N_cases), total_cases=sum(N_cases)) %>%
  ungroup() 
  

return(eu3)
}
