process_ecdc3 <- function() {
  a1 <- read_csv('./Data/ECDC/ecdc_all_distributions.csv')
  
  b1 <- read_csv('./Data/ECDC/ecdc_total.csv')
  
  all_ages_N <- b1 %>%
    filter(Unit=='N' & Population == 'Confirmed cases' & Indicator == 'Reported cases' ) %>%
    rename(IPD_total = NumValue,
           year = Time,
           country = RegionName) %>%
    dplyr::select(year, country, IPD_total)
 
  all_ages <- a1 %>%
    filter(Population == 'Confirmed cases' &
             Distribution == 'Distribution by serotype')
  
  
  all_ages <- a1 %>%
    filter(Population == 'Confirmed cases' &
             Distribution == 'Distribution by serotype') %>%
    rename(percent = Value,
           year = Time,
           country = RegionName) %>%
    mutate(
      st = gsub('STRPNE_', '', Category),
      st = if_else(st == '15', '15B/C', st),#guess
      agegrp = 'Total',
      percent   = as.numeric(percent)
    ) %>%
    dplyr::select(Population, st, agegrp, country, year, percent) %>%
    left_join(all_ages_N , by=c( 'country', 'year')
              )
  
  return(all_ages )
}