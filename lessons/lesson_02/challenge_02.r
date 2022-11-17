"Utilizando el DataFrame del Reto01 de esta sesión, crea una tabla que muestre sólo 
aquellos equipos que, en total de la liga, hayan metido más del 85% de los goles 
jugando como local. Muestra sólo las variables HomeTeam y tu variable de proporción 
y arregla los datos de forma descendente respecto a la proporción de goles."

library(dplyr)

# Setup -------------------------------------------------------------------

u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"


lista.archivos <- list(u1011, u1112, u1213, u1314)
lista.csv <- lapply(lista.archivos, read.csv, header = TRUE)
lista.datos <- lapply(lista.csv, select, Date:FTR)
ul_df <- do.call(rbind, lista.datos)
head(ul_df)


# Challenge ---------------------------------------------------------------

#View(ul_df)

# Porcentaje de diferencia de goles entre local y visitantte
es.teams <- ul_df %>%
           select(HomeTeam, FTHG, FTAG) %>%
           group_by(HomeTeam) %>%
           summarize(hg = sum(FTHG), 
                     ag = sum(FTAG)) %>%
           mutate(HGP = (hg - ag) / hg * 100 ) %>%
           filter(HGP >= 50.0) %>%
           arrange(desc(HGP)) %>%
           select(HomeTeam, HGP)

# Solucion

ul_df %>% filter(FTR == "H") %>%
  group_by(HomeTeam) %>%
  summarize(FTHG = sum(FTHG), FTAG = sum(FTAG)) %>%
  mutate(FTTOT = FTHG + FTAG, FT_prop = FTHG / FTTOT) %>%
  arrange(desc(FT_prop)) %>%
  select(HomeTeam, FT_prop) %>%
  filter(FT_prop >= 0.85)

