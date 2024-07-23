# pilota
# nascita
# nazionalità
# posizione camp.piloti
# scuderia
# posizione camp.scud
# pista
# velocità media pista
# long
# lat
# alt
# anno
# lap_quali
# posiz. griglia
# elapse.pit.time
# 
rm(list = ls())
# creo le variabili coord e quali_lap
library(tidyverse)
circuits <- read_csv("f1db_csv/circuits.csv")
quali_time <- read_csv("f1db_csv/qualifying.csv")
qualifying = quali_time
driver_standings <- read_csv("f1db_csv/driver_standings.csv")
drivers <- read_csv("f1db_csv/drivers.csv")
races <- read_csv("f1db_csv/races.csv")
constructors <- read_csv("f1db_csv/constructors.csv")
constructor_standings <- read_csv("f1db_csv/constructor_standings.csv")
pit_stops <- read_csv("f1db_csv/pit_stops.csv")
podio_puig <- read.delim("C:/Users/giaco/Desktop/uni/metodi per B.D/progetto/puig.txt")



coord <- circuits %>% 
  select(-c(2,3,4,5,9))
quali_time <- quali_time %>% 
  select(-c(1,5))
library(lubridate)
library(stringr)
quali_time$q1 <- str_replace(quali_time$q1, "\\.", ":")
quali_time$q2 <- str_replace(quali_time$q2, "\\.", ":")
quali_time$q3 <- str_replace(quali_time$q3, "\\.", ":")
quali_time <- quali_time %>% 
  mutate(tempo_millesimi_q1 = sapply(strsplit(q1, ":"), function(x) (as.integer(x[1]) * 60 * 1000) + (as.integer(x[2]) * 1000) + as.integer(x[3])))
quali_time <- quali_time %>% 
  mutate(tempo_millesimi_q2 = sapply(strsplit(q2, ":"), function(x) (as.integer(x[1]) * 60 * 1000) + (as.integer(x[2]) * 1000) + as.integer(x[3])))
quali_time <- quali_time %>% 
  mutate(tempo_millesimi_q3 = sapply(strsplit(q3, ":"), function(x) (as.integer(x[1]) * 60 * 1000) + (as.integer(x[2]) * 1000) + as.integer(x[3])))
quali_time <- quali_time %>% 
  mutate(best_time = pmin(tempo_millesimi_q1, tempo_millesimi_q2, tempo_millesimi_q3, na.rm=T))

quali_time$best_time <- quali_time$best_time/1000

quali_time <- quali_time %>% 
  mutate( quali_lap = best_time
  )
quali_time <- quali_time %>% 
  select(-c(3:11))
# creo il dataset merged
dataset_merged <- merge(driver_standings, drivers, by = 'driverId', all.x = TRUE)[,-c(2,6,9,10,11,12,15)]
dataset_merged <- merge(dataset_merged, races, by = 'raceId', all.x = TRUE)[,-c(15:23)]
dataset_merged = dataset_merged[,-c(13,15,16)]
dataset_merged <- merge(dataset_merged, coord, by = 'circuitId', all.x = TRUE)
dataset_merged$podium = rep(NA, length(dataset_merged$circuitId))
dataset_merged$podium = ifelse(dataset_merged$position <= 3, 1, 0)
dataset_merged <- merge(dataset_merged, qualifying, by = c('driverId','raceId'), all.x = TRUE)
dataset_merged[dataset_merged$raceId == 918 & dataset_merged$driverId == 817,]$constructorId = 9
dataset_merged[dataset_merged$raceId == 918 & dataset_merged$driverId == 20,]$constructorId = 9
dataset_merged[dataset_merged$raceId == 918 & dataset_merged$driverId == 817,]$position.y = 19
dataset_merged[dataset_merged$raceId == 918 & dataset_merged$driverId == 20,]$position.y = 20
dataset_merged <- merge(dataset_merged, constructors, by = 'constructorId', all.x = TRUE)[,-c(20,22,23,24,26,27,28)]
#constructor_standings = constructor_standings[,2:4]
dataset_merged <- merge(dataset_merged, constructor_standings, by = c('raceId','constructorId'), all.x = TRUE)[,-c(19,22,24,25,26)]
#dataset_merged = dataset_merged[,-20]
colnames(dataset_merged)[c(5,6,7,9,10,13,19,20,21)] = c('points.p', 'race_res','wins', 'Y.ob', 'nationality', 'circuit', 'grid', 'constructor', 'points.c')
dataset_merged <- merge(dataset_merged, quali_time, by = c('raceId','driverId'), all.x = TRUE)
dataset_merged[dataset_merged$raceId == 918 & dataset_merged$driverId == 817,]$quali_lap = '01:42:207'
dataset_merged[dataset_merged$raceId == 918 & dataset_merged$driverId == 20,]$quali_lap = '01:42:207'
# fin qui ok
dataset_cut = dataset_merged[dataset_merged$year>=2013 & dataset_merged$year!=2023,]
nas1 = which(is.na(dataset_cut$quali_lap))
dataset_cut[nas1,]$quali_lap = 120
dataset_cut = na.omit(dataset_cut)
dataset_cut$Y.ob = year(dataset_cut$Y.ob)
dataset_senzacaterham_2 <- subset(dataset_cut, !(dataset_cut$constructor=="caterham" & dataset_cut$year=="2014"))
nrow(dataset_senzacaterham_2)
dataset_senzaentrambe <- subset(dataset_senzacaterham_2, !(dataset_senzacaterham_2$constructor=="marussia" & dataset_senzacaterham_2$year=="2014"))
nrow(dataset_senzaentrambe)
dataset_cut = dataset_senzaentrambe
table(dataset_cut$round,dataset_cut$year)


# robe ciuce 1

#questo codice crea due variabili pit_stop_medio (per pilota) e pit_stop_scud
#(per scuderia) che sono riferite ai tempi medi standardizzati dei piloti e delle
#scuderie per l'anno precedente. Per i piloti "rookie" viene sostituito al posto
#del valore mancante il valore medio dei pit stop di quell'anno.

races <- read_csv("f1db_csv/races.csv")
dataset_pit1 <- merge(pit_stops, races, by = 'raceId', all.x = TRUE)
#View(dataset_pit1)
dataset_pit1 = dataset_pit1[,-c(12:24)]
dataset_pit1 <- merge(dataset_pit1, drivers, by = 'driverId', all.x = TRUE)
dataset_pit1 = dataset_pit1[,-c(13:19)]
dataset_pit1 = dataset_pit1[dataset_pit1$year>=2013 & dataset_pit1$year!=2023,]
dim(dataset_pit1)

dataset_pit2 = dataset_pit1[,c(1,2,3,6,8)]
#View(dataset_pit2)

dataset_pit3 = dataset_pit2 %>%  #creo una colonna di 1 per calcolare le medie
  mutate(sosta = 1)
dataset_pit3$duration = as.numeric(dataset_pit3$duration)
dataset_pit3$sosta = as.numeric(dataset_pit3$sosta)
dim(dataset_pit3)
dataset_pit3 = na.omit(dataset_pit3)


dataset_pit3 <- dataset_pit3 %>%  #media pit_stop per gara
  group_by(raceId) %>%
  mutate(pit_medio_gara = sum(duration) / sum(sosta))

dataset_pit3 <- dataset_pit3 %>%  #media pit_stop pilota per gara
  group_by(raceId, driverId) %>%
  mutate(pit_medio_pilota = sum(duration) / sum(sosta))

dataset_pit3 <- dataset_pit3 %>%  #media pilota standardizzata per gara
  mutate(standard_pit_gara = pit_medio_pilota - pit_medio_gara)

dataset_pit3 <- dataset_pit3 %>%  #somma per anno degli standard_pit dei piloti
  group_by(year, driverId) %>%
  mutate(standard_pit_anno = sum(standard_pit_gara))

#dataset_PIT = dataset_pit3
#View(dataset_PIT)

#vettore col numero di gare corse per anno
gare = c(19,19,19,21,20,21,21,17,22,22)

dataset_pit3$pit_medio_anno = NA

for (i in 2013:2022) {  #variabile finale pit_stop standardizzato medio per anno
  #per pilota
  dataset_pit3$pit_medio_anno <- ifelse(dataset_pit3$year == i, 
                                        dataset_pit3$standard_pit_anno/gare[i-2012],
                                        dataset_pit3$pit_medio_anno)
}

pit_stop_ = dataset_pit3[,-c(3,4,6,7,8,9,10)]
pit_stop_def = distinct(pit_stop_, driverId, raceId,
                        pit_medio_anno)
#View(pit_stop_def)

#creo il dataset con i valori traslati di un anno e per ogni riga driverId/pit/anno
#righe uniche senza duplicati
data_pit = pit_stop_def[,-3]
data_pit = data_pit %>% 
  mutate(yearn = year+1)
data_pit = data_pit[,-1]
data_pit <- data_pit %>%
  rename(year = yearn)
data_pit <- data_pit %>%
  distinct(driverId, year, pit_medio_anno, .keep_all = TRUE)
#View(data_pit)

#merge con dataset_cut
dataset_agg <- merge(dataset_cut, data_pit, by = c('driverId', 'year'), all.x = TRUE)
#View(dataset_agg)

dataset_agg = dataset_agg[dataset_agg$year > 2013,]
table(is.na(dataset_agg$pit_medio_anno))

#dataset senza NA
dataset_agg1 = na.omit(dataset_agg)

#media tempi pit per anno
dataset_agg1 = dataset_agg1 %>% 
  mutate(count = 1)
#View(dataset_agg1)

dataset_agg1 = dataset_agg1 %>% 
  group_by(year) %>% 
  mutate(media = sum(pit_medio_anno) / sum(count))

medie_annue = dataset_agg1$media  

dataset_agg[dataset_agg$year == 2014 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = medie_annue[1]
dataset_agg[dataset_agg$year == 2015 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.6471260
dataset_agg[dataset_agg$year == 2016 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.346012
dataset_agg[dataset_agg$year == 2017 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.2079641
dataset_agg[dataset_agg$year == 2018 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.2438709
dataset_agg[dataset_agg$year == 2019 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.2683319
dataset_agg[dataset_agg$year == 2020 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = 0.04934847
dataset_agg[dataset_agg$year == 2021 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.1767974
dataset_agg[dataset_agg$year == 2022 & is.na(dataset_agg$pit_medio_anno),]$pit_medio_anno = -0.08015459

table(is.na(dataset_agg))

dataset_agg = dataset_agg %>% 
  mutate(count = 1)

dataset_agg = dataset_agg %>% 
  group_by(constructorId, year) %>% 
  mutate(pit_medio_anno_scud = sum(pit_medio_anno) / sum(count))

pit_stop_final = dataset_agg[-24]
pit_stop_final = pit_stop_final[,c(1,3,4,23,24)]

# fine robe ciuce

brutti = NA
for (i in seq(1:length(levels(as.factor(dataset_cut$driverRef))))) {
  if (nrow(dataset_cut[dataset_cut$driverRef == levels(as.factor(dataset_cut$driverRef))[i],]) < 10) {
    brutti = cbind(brutti,levels(as.factor(dataset_cut$driverRef))[i])
    print(dataset_cut[dataset_cut$driverRef == levels(as.factor(dataset_cut$driverRef))[i],][,c(1,3,4,5,14)])
  }
}

dataset_cut_origin = dataset_cut
# 2012 classifica per 2013
f <- dataset_merged[dataset_merged$raceId==879,]
f$Y.ob = year(f$Y.ob)
f <- na.omit(f)
f <- f %>% 
  arrange(desc(points.p))
f <- f[,c(1,2,5,8,11,12)]
rank_4 <- 1:24
f <- f %>% 
  mutate(posizione = rank_4)

anno_2013 <- dataset_merged[dataset_merged$year==2013,]
anno_2013$Y.ob = year(anno_2013$Y.ob)
ultimo_gp <- anno_2013[anno_2013$round==1,]
classifica_2013 <- merge(f, ultimo_gp, by = c("driverId", "driverRef"), all.x = TRUE)

posizione <- c(4,9,2,3,7,NA,6,5,1,8,11,15,14,10,18,17,21,NA,NA,NA,NA,NA)

primo_gp_13 <- cbind(ultimo_gp,posizione)

# classifica
pilota_classifica <- data.frame()

# Ottenere la lista degli anni unici
anni_unici <- 2013:2022

# Ciclo for per calcolare la classifica per ogni anno precedente
for (i in 2:length(anni_unici)) {  # Parto dal secondo anno
  
  # Anno corrente
  anno_corrente <- anni_unici[i]
  
  # Anno precedente
  anno_precedente <- anni_unici[i-1]
  
  # Filtro il dataset per l'anno precedente
  data_anno_precedente <- dataset_cut[dataset_cut$year == anno_precedente, ]
  
  # Ottengo l'ultimo round dell'anno precedente
  ultimo_round <- max(data_anno_precedente$round)
  
  # Seleziono i record corrispondenti all'ultimo round dell'anno precedente
  data_ultimo_round <- data_anno_precedente[data_anno_precedente$round == ultimo_round, ]
  
  # Ordino i dati per i punti cumulati in ordine decrescente
  data_ultimo_round <- data_ultimo_round[order(-data_ultimo_round$points.p), ]
  
  # Assegno la posizione in base all'ordine dei punti cumulati
  data_ultimo_round$posizione <- 1:nrow(data_ultimo_round)
  
  # Aggiungo l'anno corrente al dataframe dei punti cumulati
  data_ultimo_round$year <- anno_corrente
  
  # Aggiungo i risultati al dataframe pilota_classifica
  pilota_classifica <- rbind(pilota_classifica, data_ultimo_round)
}

# Ordino il dataframe pilota_classifica per anno e posizione
pilota_classifica <- pilota_classifica[order(pilota_classifica$year, pilota_classifica$posizione), ]
str(pilota_classifica)
str(primo_gp_13)

classifica_finale <- rbind(primo_gp_13,pilota_classifica)
classifica_finale <- classifica_finale[,c(8,11,23)]
#View(classifica_finale)
classifica_finale <- classifica_finale %>% 
  rename(last_position = posizione)
dd <- merge(dataset_cut, classifica_finale, by = c("driverRef", "year"), all.x = TRUE)


scuderia_classifica <- data.frame()
# scuderie
# Ottenere la lista degli anni unici
anni_unici <- 2013:2022

for (i in 2:length(anni_unici)) {  # Parto dal secondo anno
  
  # Anno corrente
  anno_corrente <- anni_unici[i]
  
  # Anno precedente
  anno_precedente <- anni_unici[i-1]
  
  # Filtro il dataset per l'anno precedente
  data_anno_precedente <- dataset_cut[dataset_cut$year == anno_precedente, ]
  
  # Ottengo l'ultimo round dell'anno precedente
  ultimo_round <- max(data_anno_precedente$round)
  
  # Seleziono i record corrispondenti all'ultimo round dell'anno precedente
  data_ultimo_round <- data_anno_precedente[data_anno_precedente$round == ultimo_round, ]
  
  # Calcolo i punti cumulati delle scuderie
  punti_cumulati <- aggregate(points.c ~ constructor, data = data_ultimo_round, sum)
  
  # Seleziono solo le righe uniche basate sulla colonna "constructor"
  punti_cumulati <- unique(punti_cumulati)
  
  # Ordino i dati per i punti cumulati delle scuderie in ordine decrescente
  punti_cumulati <- punti_cumulati[order(-punti_cumulati$points.c), ]
  
  # Assegno la posizione in base all'ordine dei punti cumulati
  punti_cumulati$posizione <- 1:nrow(punti_cumulati)
  
  # Aggiungo l'anno corrente al dataframe dei punti cumulati
  punti_cumulati$year <- anno_corrente
  
  # Aggiungo i risultati al dataframe scuderia_classifica
  scuderia_classifica <- rbind(scuderia_classifica, punti_cumulati)
  
}
scuderia_classifica$points.c <- scuderia_classifica$points.c/2
# Ordino il dataframe scuderia_classifica per anno e posizione
scuderia_classifica <- scuderia_classifica[order(scuderia_classifica$year, scuderia_classifica$posizione), ]
classifica_scuderie = scuderia_classifica

scuderia_classifica <- data.frame()
# scuderie
# Ottenere la lista degli anni unici, uso dal 2010 al 2014 per ottenere classifica dell'anno 2012 
anni_unici <- 2010:2014

for (i in 2:length(anni_unici)) {  # Parto dal secondo anno
  
  # Anno corrente
  anno_corrente <- anni_unici[i]
  
  # Anno precedente
  anno_precedente <- anni_unici[i-1]
  
  # Filtro il dataset per l'anno precedente
  data_anno_precedente <- dataset_merged[dataset_merged$year == anno_precedente, ]
  
  # Ottengo l'ultimo round dell'anno precedente
  ultimo_round <- max(data_anno_precedente$round)
  
  # Seleziono i record corrispondenti all'ultimo round dell'anno precedente
  data_ultimo_round <- data_anno_precedente[data_anno_precedente$round == ultimo_round, ]
  
  # Calcolo i punti cumulati delle scuderie
  punti_cumulati <- aggregate(points.c ~ constructor, data = data_ultimo_round, sum)
  
  # Seleziono solo le righe uniche basate sulla colonna "constructor"
  punti_cumulati <- unique(punti_cumulati)
  
  # Ordino i dati per i punti cumulati delle scuderie in ordine decrescente
  punti_cumulati <- punti_cumulati[order(-punti_cumulati$points.c), ]
  
  # Assegno la posizione in base all'ordine dei punti cumulati
  punti_cumulati$posizione <- 1:nrow(punti_cumulati)
  
  # Aggiungo l'anno corrente al dataframe dei punti cumulati
  punti_cumulati$year <- anno_corrente
  
  # Aggiungo i risultati al dataframe scuderia_classifica
  scuderia_classifica <- rbind(scuderia_classifica, punti_cumulati)
  
}
scuderia_classifica$points.c <- scuderia_classifica$points.c/2
# Ordino il dataframe scuderia_classifica per anno e posizione
scuderia_classifica <- scuderia_classifica[order(scuderia_classifica$year, scuderia_classifica$posizione), ]


posizione_scuderia <- rbind(scuderia_classifica[scuderia_classifica$year==2013,],classifica_scuderie)

posizione_scuderia <- posizione_scuderia %>% 
  rename(last_team_position = posizione, points.constructor = points.c)

dati_uniti <- merge(dd, posizione_scuderia, by = c("constructor", "year"), all.x = TRUE)
#View(dati_uniti)


# Ottieni l'ordine degli indici dei punti in base all'ordine crescente dei punti

races = dataset_cut[0, c(1,2,5,8,12)]
races$classifica_gara = rep(NA, nrow(races))
races$podio = rep(NA, nrow(races))
races$a.punti = rep(NA, nrow(races))
raceIds = levels(as.factor(dataset_cut$raceId))
for (i in raceIds[20:length(raceIds)]) {
  #i = 900
  race = dataset_cut[dataset_cut$raceId == as.numeric(i), c(1,2,5,8,12)]
  if (race$round[1] == 1) {
    race$classifica_gara <- rank(-race$points.p, ties.method = "min")
  } else {
      prev_race = dataset_cut[dataset_cut$raceId == as.numeric(i)-1, c(1,2,5,8,12)]
      race = race[order(race$driverRef),]
      prev_race = prev_race[order(prev_race$driverRef),]
      race$classifica_gara = rep(NA, nrow(race))
        if (nrow(race) == nrow(prev_race)) {
          race$classifica_gara = race$points.p-prev_race$points.p
          race$classifica_gara <- rank(-race$classifica_gara, ties.method = "min")
        } 
  }
  race$podio = rep(NA, nrow(race))
  race$a.punti = rep(NA, nrow(race))
  for (i in seq(1:nrow(race))) {
    if (is.na(race$classifica_gara[i])==F) {
      if (race$classifica_gara[i] <=3) race$podio[i] = 1
      else race$podio[i] = 0
      if (race$classifica_gara[i] <=10) race$a.punti[i] = 1
      else race$a.punti[i] = 0
    }
  }
  races = rbind(races, race)
}
races = races[, c(1,4,7,8)] # aggiungere colonna 6 per la classifica della gara.
which(is.na(races$classifica_gara))
dataset_cut <- merge(dati_uniti, races, by = c('raceId','driverRef'), all.x = TRUE)
dataset_cut = dataset_cut[,-19]


dati_finale = dataset_cut[dataset_cut$year>2013,]
#view(dati_finale)
table(is.na(dati_finale$podio), dati_finale$year)
a = dati_finale[dati_finale$driverRef=='massa',]
nrow(dati_finale)
dati_finale2 = pit_stop_final

brutti = NA
for (i in seq(1:length(levels(as.factor(dati_finale$driverRef))))) {
  if (nrow(dati_finale[dati_finale$driverRef == levels(as.factor(dati_finale$driverRef))[i],]) < 10) {
    brutti = cbind(brutti,levels(as.factor(dati_finale$driverRef))[i])
    print(dati_finale[dati_finale$driverRef == levels(as.factor(dati_finale$driverRef))[i],][,c(1,3,4,5,14)])
  }
}

Nas = which(is.na(dati_finale$podio))
dati_finale$podio[Nas] = 0
table(is.na(dati_finale$podio))
dati_finale$raceId[Nas]
dati_finale[dati_finale$raceId==927, c(1,3,5,14,25)]


dati_finale$podio[dati_finale$raceId == 927 & 
                    dati_finale$driverRef %in% c('vettel', 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==928, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 928 & 
                    dati_finale$driverRef %in% c('vettel', 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==929, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 929 & 
                    dati_finale$driverRef %in% c("raikkonen", 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==930, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 930 & 
                    dati_finale$driverRef %in% c('vettel', 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==932, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 932 & 
                    dati_finale$driverRef %in% c('bottas', 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==933, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 933 & 
                    dati_finale$driverRef %in% c('massa', 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==936, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 936 & 
                    dati_finale$driverRef %in% c('vettel', 'kvyat', 'ricciardo')] <- 1
dati_finale[dati_finale$raceId==937, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 937 & 
                    dati_finale$driverRef %in% c('grosjean', 'hamilton', 'rosberg')] <- 1
dati_finale[dati_finale$raceId==938, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 938 & 
                    dati_finale$driverRef %in% c('vettel', 'hamilton', 'massa')] <- 1
dati_finale[dati_finale$raceId==941, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 941 & 
                    dati_finale$driverRef %in% c('vettel', 'hamilton', 'perez')] <- 1
dati_finale[dati_finale$raceId==942, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 942 & 
                    dati_finale$driverRef %in% c('vettel', 'hamilton', 'rosberg')] <- 1
levels(as.factor(dati_finale$driverRef))

dati_finale[dati_finale$raceId==977, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 977 &
                    dati_finale$driverRef %in% c('bottas', 'vettel',
                                                           'ricciardo')] <- 1
dati_finale[dati_finale$raceId==986, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 986] <- ifelse(dati_finale$raceId == 986 &
                              dati_finale$driverRef %in% c('bottas', 'raikkonen',
                                                           'max_verstappen'), 1, 0)[dati_finale$raceId == 986]

dati_finale[dati_finale$raceId==987, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 987] <- ifelse(dati_finale$raceId == 987 &
                              dati_finale$driverRef %in% c('bottas', 'vettel',
                                                           'raikkonen'), 1, 0)[dati_finale$raceId == 987]

dati_finale[dati_finale$raceId==1012, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1012] <- ifelse(dati_finale$raceId == 1012 &
                              dati_finale$driverRef %in% c('bottas', 'hamilton',
                                                           'vettel'), 1, 0)[dati_finale$raceId == 1012]

dati_finale[dati_finale$raceId==1013, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1013] <- ifelse(dati_finale$raceId == 1013 &
                              dati_finale$driverRef %in% c('bottas', 'vettel',
                                                           'hamilton'), 1, 0)[dati_finale$raceId == 1013]

dati_finale[dati_finale$raceId==1051, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1051] <- ifelse(dati_finale$raceId == 1051 &
                              dati_finale$driverRef %in% c('hamilton', 'alonso',
                                                           'max_verstappen'), 1, 0)[dati_finale$raceId == 1051]

dati_finale[dati_finale$raceId==1056, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1056] <- ifelse(dati_finale$raceId == 1056 &
                              dati_finale$driverRef %in% c('norris', 'sainz',
                                                           'max_verstappen'), 1, 0)[dati_finale$raceId == 1056]

dati_finale[dati_finale$raceId==1057, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1057] <- ifelse(dati_finale$raceId == 1057 &
                              dati_finale$driverRef %in% c('perez', 'vettel',
                                                           'gasly'), 1, 0)[dati_finale$raceId == 1057]

dati_finale[dati_finale$raceId==1069, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1069] <- ifelse(dati_finale$raceId == 1069 &
                              dati_finale$driverRef %in% c('hamilton', 'perez',
                                                           'max_verstappen'), 1, 0)[dati_finale$raceId == 1069]

dati_finale[dati_finale$raceId==1091, c(1,3,5,14,25)]
dati_finale$podio[dati_finale$raceId == 1091] <- ifelse(dati_finale$raceId == 1091 &
                              dati_finale$driverRef %in% c('perez', 'leclerc',
                                                           'sainz'), 1, 0)[dati_finale$raceId == 1091]

safe = dati_finale
dati_finale = safe

for (i in 1:nrow(dati_finale)){
  for (j in 1:nrow(podio_puig)){
    if (dati_finale$raceId[i] == podio_puig$raceId[j] & dati_finale$driverRef[i] == podio_puig$driverRef[j]) dati_finale[i,]$podio = podio_puig[j,]$podio
  }
}

dati_finale <- dati_finale %>%
  mutate(points.constructor = ifelse(constructor == "manor" & year == 2015, 
                                     ifelse(is.na(points.constructor), 2, points.constructor),
                                     points.constructor))

dati_finale <- dati_finale %>%
  mutate(last_team_position = ifelse(constructor == "manor" & year == 2015, 
                                     ifelse(is.na(last_team_position), 10, last_team_position),
                                     last_team_position))


# sostituisce gli NA di last_position dei piloti con valore 21
dati_finale$last_position <- ifelse(is.na(dati_finale$last_position), 21, dati_finale$last_position)
#View(dati_finale)
table(is.na(dati_finale$last_position))  # 0 NA

# sostituisce gli NA di last_team_position dei piloti con valore 11
dati_finale$last_team_position <- ifelse(is.na(dati_finale$last_team_position), 11, dati_finale$last_team_position)
#View(dati_finale)
table(is.na(dati_finale$podio))
table(dati_finale$podio)

valori_da_sostituire <- c("de_vries", "pietro_fittipaldi", "resta", "rossi")
nuovi_valori <- c('albon', 'grosjean', 'massa', 'merhi')

dati_finale$driverRef <- ifelse(dati_finale$driverRef %in% valori_da_sostituire, 
                                nuovi_valori[match(dati_finale$driverRef, valori_da_sostituire)],
                                dati_finale$driverRef)
dati_finale[dati_finale$raceId == 1046 & dati_finale$driverRef == 'russell', ]$driverRef = "hamilton"
dati_finale[dati_finale$raceId == 1046 & dati_finale$driverRef == 'aitken', ]$driverRef = "russell"


table(dati_finale$podio)
#for (i in dati_finale$raceId) {
#  if (table(dati_finale[dati_finale$raceId==i,]$podio)[2]!=3) cat(table(dati_finale[dati_finale$raceId==i,]$podio)[2], '           ',i, '\n')
#}
dati_finale[dati_finale$raceId == 1058,]$podio = 0
dati_finale[dati_finale$raceId == 1058 & dati_finale$driverRef %in% c('hamilton','bottas','max_verstappen'),]$podio = 1
dati_finale[dati_finale$raceId==1058, c(1,3,5,14,25)]

dati_finale[dati_finale$raceId == 1059,]$podio = 0
dati_finale[dati_finale$raceId == 1059 & dati_finale$driverRef %in% c('hamilton','perez','max_verstappen'),]$podio = 1
dati_finale[dati_finale$raceId==1059, c(1,3,5,14,25)]

dati_finale[dati_finale$raceId == 1076,]$podio = 0
dati_finale[dati_finale$raceId == 1076 & dati_finale$driverRef %in% c('leclerc','russell','perez'),]$podio = 1
dati_finale[dati_finale$raceId==1076, c(1,3,5,14,25)]

dati_finale[dati_finale$raceId == 950,]$podio = 0
dati_finale[dati_finale$raceId == 950 & dati_finale$driverRef %in% c('kvyat','vettel','rosberg'),]$podio = 1
dati_finale[dati_finale$raceId==950, c(1,3,5,14,25)]

for (i in 1:nrow(dati_finale)) {
  if (dati_finale$wins[i] != 0) dati_finale$wins[i] = dati_finale$wins[i]-1
}

dati_finale$time <- strptime(dati_finale$time, format = "%H:%M:%S")$hour

dati_finale <- merge(dati_finale, pit_stop_final, by = c('raceId','driverId','constructorId'), all.x = TRUE)[,-23]

dati_safe = dati_finale
dati_finale = dati_safe

dati_num = dati_finale %>% 
  mutate(across(c(driverRef, constructor, nationality, circuit), as.factor)) %>%
  mutate(across(c(alt, quali_lap), as.numeric)) %>% 
  mutate(across(c(driverRef, constructor, nationality, circuit), as.numeric))
xprov <- dati_num %>% 
  select(-c(podio, driverId, constructorId, circuitId, pit_medio_anno, pit_medio_anno_scud))

nas2 = which(is.na(xprov$alt))
dati_finale[nas2,]$alt = 249

table(is.na(dati_finale$a.punti))
nas3 = which(is.na(dati_finale$a.punti))
dati_finale[nas3,]$a.punti = 0

dati_finale <- dati_finale %>%
  arrange(year, raceId) %>%
  group_by(year, driverRef) %>%
  mutate(
    podiums = cumsum(lag(podio == "1", default = FALSE)),
    podiums = ifelse(row_number() == 1, 0, podiums)
  )
dati_safe$podiums = dati_finale$podiums
dati_safe$quali_lap = as.numeric(dati_finale$quali_lap)
nas4 = which(is.na(dati_safe$quali_lap))
dati_finale[nas4,]$quali_lap[1] = 120
dati_finale[nas4,]$quali_lap[2] = 120
dati_safe$quali_lap = as.numeric(dati_finale$quali_lap)
dati_safe$alt = as.numeric(dati_finale$alt)

dati <- dati_finale


rm(list = c("anno_2013", "brutti", "circuits", "classifica_2013", "classifica_finale", "classifica_scuderie", "constructor_standings", "constructors",
            "coord", "data_anno_precedente", "data_ultimo_round", "dataset_cut", "dataset_cut_origin", "dataset_merged",'pit_stop_final','dataset_agg','dataset_agg1','data_pit',
            "dataset_senzacaterham_2", "dataset_senzaentrambe", "dati_uniti", "dd", "driver_standings", "drivers", "f", "pilota_classifica", "pit_stops",
            "podio_puig", "posizione_scuderia", "prev_race", "primo_gp_13", "punti_cumulati", "quali_time", "qualifying", "race", "races", "scuderia_classifica", "ultimo_gp",
            'dataset_cut_origin2', 'dataset_cut1','dataset_pit1','dataset_pit2','dataset_pit3','dataset_pit4','pit_stop_','pit_stop_def','a','dati_finale2','safe','dati_finale'))
dati_num = dati_safe %>% 
  mutate(across(c(driverRef, constructor, nationality, circuit), as.factor)) %>%
  mutate(across(c(alt, quali_lap), as.numeric)) %>% 
  mutate(across(c(driverRef, constructor, nationality, circuit), as.numeric))
#### MODELLAZIONE ----
xprov <- dati_num %>% 
  select(-c(a.punti, podio, driverId, constructorId, circuitId,
            driverRef, constructor, nationality, circuit))

library(caret)
# Creazione delle variabili dummy
dati_d = dati_num[,-c(2,3,4,5,7,8,9,10,12,14,20,25)]
podi.risp = dati_d$podio
dati_d = dati_d[,-c(1,5,6,7,8,13)]

dummy_variabili <- dummyVars(~ driverRef, data = dati_safe)
dati_dummy <- predict(dummy_variabili, newdata = dati_safe)
dummy_variabili2 <- dummyVars(~ constructor, data = dati_safe)
dati_dummy2 <- predict(dummy_variabili2, newdata = dati_safe)
dummy_variabili3 <- dummyVars(~ circuit, data = dati_safe)
dati_dummy3 <- predict(dummy_variabili3, newdata = dati_safe)
driv_constr = model.matrix(~ as.matrix(dati_dummy):as.matrix(dati_dummy2))[,-1]
year_constr = model.matrix(~ as.matrix(dati_d$year):as.matrix(dati_dummy2))[,-1]
driv_circ = model.matrix(~ as.matrix(dati_dummy):as.matrix(dati_dummy3))[,-1]
round_constr = model.matrix(~ as.matrix(dati_d$round):as.matrix(dati_dummy2))[,-1]
year_driv = model.matrix(~ as.matrix(dati_d$year):as.matrix(dati_dummy))[,-1]
scud_grid = model.matrix(~ as.matrix(dati_d$grid):as.matrix(dati_dummy2))[,-1]
driv_grid = model.matrix(~ as.matrix(dati_d$grid):as.matrix(dati_dummy))[,-1]
circ_grid = model.matrix(~ as.matrix(dati_d$grid):as.matrix(dati_dummy3))[,-1]
scud_alt = model.matrix(~ as.matrix(dati$alt):as.matrix(dati_dummy2))[,-1]
podiums_round = dati$podiums/dati$round
dati_d = dati_d[,-c(1,2,3)]

dati_d = cbind(podi.risp, dati_d, dati_dummy, dati_dummy2, driv_constr, year_constr, driv_circ, round_constr)
dati_d = cbind(podi.risp, dati_d, dati_dummy, dati_dummy2, driv_constr, year_constr, driv_circ, round_constr, podiums_round, scud_grid)

colnames(dati_d)[1] <- "podio"

library(glmnet)
library(tidyverse)
library(tidymodels)
set.seed(123)
dati_split <- initial_split(dati_d, prop = 0.8)
dati_train <- training(dati_split)
dati_test <- testing(dati_split)
attach(dati_d)
barplot(prop.table(table(podio,podiums),1), beside = T,
        xlab = 'numero podi stagionali fino alla gara in questione',
        ylab = 'proporzione podi/non podi')
barplot(prop.table(table(podio,grid),2), beside = T,
        xlab = 'posizione di partenza in griglia',
        ylab = 'proporzione podi/non podi', col = c('red', 'green'))
legend('topright', legend = c('non podi', 'podi'), col = c('red', 'green'), pch = c(15,15))
barplot(prop.table(table(podio,last_position),1), beside = T,
        xlab = 'posizione di partenza in griglia',
        ylab = 'proporzione podi/non podi')
barplot(prop.table(table(podio,last_team_position),1), beside = T,
        xlab = 'risultato della scuderia nel campionato costruttori scorso',
        ylab = 'probabilità di andare a podio')
barplot(prop.table(table(podio,Y.ob),1), beside = T,
        xlab = 'risultato della scuderia nel campionato costruttori scorso',
        ylab = 'probabilità di andare a podio')

RL1 <- glmnet(y = dati_train$podio,
              x = as.matrix(dati_train[,-1]), alpha = 0.5) # 2479
previsione <- predict(RL1, newx = as.matrix(dati_test[,-1]))
errori <- (dati_test$podio - previsione)^2 %>% apply(2,mean)
lambda_opt <- errori %>% which.min()
lambda_choice = exp(-3)
test_pred <- predict(RL1, type="response", newx = as.matrix(dati_test[,-1]), s=RL1$lambda[lambda_opt])
test_pred_class <- ifelse(test_pred > 0.4, 1, 0)
t = table(dati_test$podio, test_pred_class)
sum(diag(table(dati_test$podio, test_pred_class)))/sum(table(dati_test$podio, test_pred_class))
plot(RL1, xvar="lambda", label=T) # guardo la velocità con cui le var vanno a zero.
coef_cutoff <- coef(RL1, s = RL1$lambda[lambda_opt])
significant_vars <- which(coef_cutoff != 0)
colnames(dati_d[,-1])[significant_vars-1]
library(pROC)
plot(roc(dati_test$podio, as.vector(test_pred)), print.auc=T)

lambda_choice = exp(-3)
test_pred <- predict(RL1, type="response", newx = as.matrix(dati_test[,-1]), s=lambda_choice)
test_pred_class <- ifelse(test_pred > 0.4, 1, 0)
t = table(dati_test$podio, test_pred_class)
sum(diag(table(dati_test$podio, test_pred_class)))/sum(table(dati_test$podio, test_pred_class))
plot(RL1, xvar="lambda", label=T) # guardo la velocità con cui le var vanno a zero.
coef_cutoff <- coef(RL1, s = lambda_choice)
significant_vars <- which(coef_cutoff != 0)
colnames(dati_d)[significant_vars-1]
significant_vars-1
plot(roc(dati_test$podio, as.vector(test_pred)), print.auc=T)

# Dati dalla tabella di confusione t
VP <- t[4]
FP <- t[3]
FN <- t[2]
VN <- t[1]
accuracy <- (VP + VN) / (VP + FP + FN + VN)
precision <- VP / (VP + FP)
recall <- VP / (VP + FN)
specificity <- VN / (VN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuratezza:", accuracy, "\n",
    "Precisione:", precision, "\n",
    "Richiamo (Recall):", recall, "\n",
    "Specificità:", specificity, "\n",
    "F1-Score:", f1_score, "\n")

colnames(dati_train)[286] <- "Vettel_ferrari"
colnames(dati_test)[286] <- "Vettel_ferrari"
colnames(dati_train)[519] <- "Hamilton_mercedes"
colnames(dati_test)[519] <- "Hamilton_mercedes"
colnames(dati_train)[540] <- "Rosberg_mercedes"
colnames(dati_test)[540] <- "Rosberg_mercedes"
colnames(dati_train)[618] <- "Verstappen_redbull"
colnames(dati_test)[618] <- "Verstappen_redbull"
colnames(dati_train)[821] <- "Ferrari_year"
colnames(dati_test)[821] <- "Ferrari_year"
colnames(dati_train)[827] <- "Mercedes_year"
colnames(dati_test)[827] <- "Mercedes_year"
colnames(dati_train)[829] <- "Redbull_year"
colnames(dati_test)[829] <- "Redbull_year"
colnames(dati_train)[2474] <- "Redbull_round"
colnames(dati_test)[2474] <- "Redbull_round"


# glm_finale <- glm(dati_train$podi ~ data[, 4] + data[, 6] + data[,9] + data[, 21] + data[, 59] + data[, 65] + data[,326] + data[, 658] +
#                     data[, 861] + data[, 867] + data[,2514], family = binomial, data = dati_train)
# summary(glm_finale)

final.glm <- glm(formula = podio ~ grid + last_position +last_team_position +
                   pit_medio_anno_scud + driverRefhamilton +driverRefrosberg
                 + constructorferrari + constructormercedes + constructorred_bull 
                 + Vettel_ferrari + Hamilton_mercedes +  Rosberg_mercedes + 
                   Verstappen_redbull + Ferrari_year + Mercedes_year + Redbull_year + Redbull_round,
                 family = binomial, data = dati_train)
summary(final.glm)
library(MASS)
stepAIC(final.glm)


glm_finale <- glm(podio ~ grid +  pit_medio_anno_scud + 
                    driverRefhamilton + Vettel_ferrari + Verstappen_redbull + 
                    Ferrari_year + constructormercedes + Redbull_year, family = binomial, 
                  data = dati_train)
summary(glm_finale)

p.glm1 <- glm_finale %>% predict(newdata = dati_test, type = "response")
library(yardstick)
library(gridExtra)
prediction <- tibble(truth = as.factor(dati_test$podio))
prediction <- prediction %>%
  mutate(pred = p.glm1) %>%
  mutate(model = "GLM")

lift <- prediction %>%
  lift_curve(truth, pred, event_level="second") %>%
  autoplot()

roc <- prediction %>% 
  roc_curve(truth, pred, event_level="second") %>%
  autoplot()
grid.arrange(roc, lift, ncol= 2)

pred <- predict(glm_finale, newdata = dati_test, type = "response")
pred2 <- ifelse(pred > 0.3, 1, 0)
tab_class_glm <- table(dati_test$podio, pred2)
tab_class_glm
sum(diag(tab_class_glm))/sum(tab_class_glm)
dim(dati_train)

# Dati dalla tabella di confusione t
VP <- tab_class_glm[4]
FP <- tab_class_glm[3]
FN <- tab_class_glm[2]
VN <- tab_class_glm[1]
accuracy <- (VP + VN) / (VP + FP + FN + VN)
precision <- VP / (VP + FP)
recall <- VP / (VP + FN)
specificity <- VN / (VN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuratezza:", accuracy, "\n")
cat("Precisione:", precision, "\n")
cat("Richiamo (Recall):", recall, "\n")
cat("Specificità:", specificity, "\n")
cat("F1-Score:", f1_score, "\n")


# ALBERI ####
# INDICE DI GINI
library(rpart)
colnames(dati_train)[2472] <- "Mercedes_round"
colnames(dati_test)[2472] <- "Mercedes_round"
colnames(dati_train)[827] <- "Mercedes_year"
colnames(dati_test)[827] <- "Mercedes_year"
colnames(dati_train)[2474] <- "Redbull_round"
colnames(dati_test)[2474] <- "Redbull_round"
t0 = rpart(podio ~ ., method = "class", control  = rpart.control(minbucket = 5, cp = 0.0001), data = dati_train)
plot(t0)
text(t0, cex = 0.4)
printcp(t0)
plotcp((t0))
t2 = prune.rpart(t0, cp = t0$cptable[6,1])
t3 = prune.rpart(t0, cp = t0$cptable[9,1])


pred1 = predict(t0, newdata = dati_test)[,2]
pred2 = predict(t2, newdata = dati_test)[,2]
pred3 = predict(t3, newdata = dati_test)[,2]

er1 = (pred1 - dati_test$podio)^2 %>% mean()
er2 = (pred2 - dati_test$podio)^2 %>% mean()
er3 = (pred3 - dati_test$podio)^2 %>% mean()


cat("Errori: ", cbind(er1,er2,er3), "\nErrore minimo : ", min(er1,er2,er3))
pred5 = ifelse(pred3 > 0.3, 1,0)
t = table(dati_test$podio, pred5)
sum(diag(t))/sum(t)  # circa 0.89
# Dati dalla tabella di confusione t
VP <- t[4]
FP <- t[3]
FN <- t[2]
VN <- t[1]
accuracy <- (VP + VN) / (VP + FP + FN + VN)
precision <- VP / (VP + FP)
recall <- VP / (VP + FN)
specificity <- VN / (VN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuratezza:", accuracy, "\nPrecisione:", precision, "\nRichiamo (Recall):", recall, "\nSpecificità:", specificity, "\nF1-Score:", f1_score, "\n")

library(rplot)
rpart.plot(t3, cex=0.7, type = 2)


# ENTROPIA
ct0  <- rpart(podio ~ ., method = "class",
              parms = list(split="information"),
              control = rpart.control(minbucket = 5, cp = 0.0001), data = dati_train)
plot(ct0)
text(ct0, cex = 0.6)
printcp(ct0)
plotcp((ct0))
ct2 = prune.rpart(ct0, cp = ct0$cptable[4,1])
ct3 = prune.rpart(ct0, cp = ct0$cptable[5,1])
pred11 = predict(ct0, newdata = dati_test)[,2]
pred22 = predict(ct2, newdata = dati_test)[,2]
pred33 = predict(ct3, newdata = dati_test)[,2]
er11 = (pred11 - dati_test$podio)^2 %>% mean()
er22 = (pred22 - dati_test$podio)^2 %>% mean()
er33 = (pred33 - dati_test$podio)^2 %>% mean()
cbind(er11,er22, er33)
pred55 = ifelse(pred33 > 0.3, 1,0)
tt = table(dati_test$podio, pred55)
sum(diag(tt))/sum(tt)

VP <- tt[4]
FP <- tt[3]
FN <- tt[2]
VN <- tt[1]
accuracy <- (VP + VN) / (VP + FP + FN + VN)
precision <- VP / (VP + FP)
recall <- VP / (VP + FN)
specificity <- VN / (VN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuratezza:", accuracy, "\nPrecisione:", precision, "\nRichiamo (Recall):", recall, "\nSpecificità:", specificity, "\nF1-Score:", f1_score, "\n")


library(yardstick)
library(gridExtra)
prediction_tree <- tibble(truth = as.factor(dati_test$podio))
prediction_tree <- prediction_tree %>%
  mutate(pred = pred1) %>%
  mutate(model = "Full Tree (Gini)") %>%
  add_row(truth = as.factor(dati_test$podio), pred = pred3, model = "Pruned Tree (Gini)") %>%
  add_row(truth = as.factor(dati_test$podio), pred = pred11, model = "Full Tree (Entropy)") %>%
  add_row(truth = as.factor(dati_test$podio), pred = pred33, model = "Pruned Tree (Entropy)")


curve_tree <- prediction_tree %>% group_by(model) %>%
  roc_curve(truth, pred, event_level="second") %>%
  autoplot()
plot(curve_tree)