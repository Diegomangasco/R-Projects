## Converte serie storiche occupazione da XLS in data frame

library(tidyverse)
library(readxl)

url = 'https://www.istat.it/it/files//2021/04/202102_serie-storiche.xls'
file = "202102_serie-storiche.xls"
if( ! file.exists(file) ){
  download.file(url,file,mode="wb")
}

colonne <- rep(c("MF","M","F"),each=6) %>%  ## ripete 6 volte ciascun prefisso
  paste(                              ## concatenato con
    rep(c("anno","mese",          ## ripete 3 volte le sei informazioni
          "attivita","occupazione",
          "disoccupazione",
          "disoccupazione.giovani"),3),
    sep="_") %>% 
  .[c(-12,-18)] # separatore di concatenazione

last_day_of_month <- function(year,month){
  month <- as.integer(month)
  as.Date(paste(year+(month==12),(month%%12)+1,1,sep="-"))-1
}

istat <- read_xls(file,"Tab1",skip=8,col_names = colonne) %>%
  select(-7,-8,-12,-13) %>%   ## rimuove colonne duplicate
  rename(anno=1, mese=2) %>%  ## rinomina
  fill(anno) %>%              ## completa colonna anno
  pivot_longer(3:12, names_sep="_", ## fonde assieme le variabili sparse su colonne diverse
               names_to = c("Sesso","Indicatore"), 
               values_to = "Valore" ) %>%
  mutate(anno = as.integer(anno),
         mese = as_factor(mese),
         Sesso = as.factor(Sesso)) %>%
  mutate( data = last_day_of_month(anno,mese)) %>% 
  mutate(Valore = Valore / 100) %>% ## converte percentuali in numeri < 1
  pivot_wider(names_from="Indicatore",values_from="Valore") ## separa variabili diverse

#write_csv(istat,"istat_occupazione.csv")

write_rds(istat,"istat_occupazione.RDS")
