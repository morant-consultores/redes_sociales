
# PAQUETERIA

library(readr)
library(tidyverse)

# LEER  BDS

bd_sismos <- read_csv("sismos/data/SSNMX_catalogo_19720101_20220926_m60_99.csv",
                      skip = 4, n_max = 184) %>% janitor::clean_names() %>%
  separate(fecha, into = c("ano", "mes", "dia"), sep = "-") %>%
  mutate(fecha = paste(ano, mes, dia, sep = "-"))

#Intentos cielos

bd_sismos %>% names


# dias que ha temblado dos veces

bd_sismos %>% count(mes, sort = T) %>% mutate(prob_mes = n/sum(n))

# cuants días se han repetido desde 1985
bd_sismos %>% filter(ano > 1985) %>%  count(dia, mes, sort = T)

total=1;
for(i in 2) {
  total=total*((366-i)/365)
  cat("La probabilidad de que en un grupo de ", i, " personas, al menos dos cumplan años el mismo día es de: ", 1-total,"\n")
}


total=1;
for(i in 37) {
  total=total*((366-i)/365)
  cat("La probabilidad de que en un grupo de ", i, " años, al menos dos terremotos acurran el mismo día es de: ", 1-total,"\n")
}

1-(364/365)^(1)


# modelo

set.seed(5793645)

bd_sismos %>% count(ano) %>% mutate(promedio = mean(n), sd(n))

n_sismos <- if_else(round(rnorm(37, mean = 3.84, sd = 2.44)) < 0, 0, n_sismos)


pelotita_verde <- function(n_simulacion) {


  simulacion <- map(n_sismos, ~ {

    sample(1:365, size = .x, replace = T)

  })


  res = simulacion %>% flatten_int() %>% as_tibble() %>%
    count(value) %>%
    mutate(mayor_2 = n>=2) %>%
    summarise(prob = sum(mayor_2)/n()) %>%
    mutate(n_simulacion)

  return(res)


}


simulacion_final <- map_df(seq(1:10000), ~ pelotita_verde(.x))




simulacion_final %>% summarise( mean(prob))


if_else(is.integer(sample(1:365, size = 0, replace = T)), 0, sample(1:365, size = 200, replace = T))


sample(1:365, size = 10, replace = T)

a <- c(1,2)

c(a, b)



bd_sismos %>% count(ano) %>% count(n)

bd_sismos %>%
  filter(ano >= 1985) %>%
  count(mes, dia)  %>%
  mutate(mayor_2 = n>=2) %>%
  summarise(prob = sum(mayor_2)/n())


bd_sismos %>%
  filter(ano >= 1985) %>%  count(mes, dia, sort = T) %>% count(n>=2)



rnorm(3, 3.8, 2.4) %>% as_tibble() %>%  ggplot(aes(x = value)) + geom_histogram()

