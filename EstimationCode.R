# Lendo os pacotes necessarios
library(PandemicLP)
library(dplyr)
library(stringr)
library(pracma)

# Definindo quantidades importantes
country = "Russia"

### CASOS
extension = "n"
inits = readRDS(url(paste0("https://github.com/CovidLP/Methodology/raw/main/AppChains/", country, "_", extension, ".rds")))
dados = inits$Y
(last.date = tail(dados$data$date, 1))

# Definindo os valores iniciais
source("inits.R") # gr?fico para escolher as ondas
n_waves = inits$n_waves # n?mero de ondas a serem considerados
ondas = 1:n_waves # vetor com os n?meros das ondas (antes da linha azul)

temp_init = list()
temp_init$a <- a.old[ondas]
temp_init$b <- b.old[ondas]
temp_init$c <- c.old[ondas]
temp_init$alpha <- alpha.old[ondas]
temp_init$delta <- delta.old[ondas]

# Ajustando a Binomial Negativa
mod <- pandemic_model(dados, case_type = "confirmed", p = 0.08,
                      family = "negbin", n_waves = n_waves,
                      warmup = 15e3, thin = 1, sample_size = 5e3,
                      init = list(temp_init, temp_init), covidLPconfig = FALSE,
                      chains = 1)
save(mod, file = paste0(str_replace_all(country, " ", "-"), "_", extension, "_negbin.RData"))

### MORTES
extension = "d"
inits = readRDS(url(paste0("https://github.com/CovidLP/Methodology/raw/main/AppChains/", country, "_", extension, ".rds")))
dados = inits$Y
(last.date = tail(dados$data$date, 1))

# Definindo os valores iniciais
source("inits.R") # gr?fico para escolher as ondas
n_waves = inits$n_waves # n?mero de ondas a serem considerados
ondas = 1:n_waves # vetor com os n?meros das ondas (antes da linha azul)

temp_init = list()
temp_init$a <- a.old[ondas]
temp_init$b <- b.old[ondas]
temp_init$c <- c.old[ondas]
temp_init$alpha <- alpha.old[ondas]
temp_init$delta <- delta.old[ondas]

# Ajustando a Binomial Negativa
mod <- pandemic_model(dados, case_type = "deaths", p = 0.08*0.25,
                      family = "negbin", n_waves = n_waves,
                      warmup = 15e3, thin = 1, sample_size = 5e3,
                      init = list(temp_init, temp_init), covidLPconfig = FALSE,
                      chains = 1)
save(mod, file = paste0(str_replace_all(country, " ", "-"), "_", extension, "_negbin.RData"))
