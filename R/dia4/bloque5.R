pacman::p_load(tidyverse, lubridate, modelsummary)
# Las coordenadas de Montevideo
latitude <- -34.9033
longitude <- -56.1882

# El rango de fechas
start_date <- "2023-01-01"
end_date <- "2025-01-01"

# Bajamos los datos de temperatura de Open Meteo
temp_data_MVD <- read.table(paste0(
  "https://archive-api.open-meteo.com/v1/archive?latitude=",
  latitude, "&longitude=", longitude,
  "&start_date=", start_date,
  "&end_date=", end_date,
  "&hourly=temperature_2m&timezone=auto&format=csv"),
  skip =3, header = T, sep = ",") %>%
  # reformat dates
  mutate(time = gsub("T", " ", time)) %>%
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M", tz="America/Argentina/Buenos_Aires")) %>%
  rename(temp = temperature_2m...C.)

# Calculamos la media diaria de temperatura
dayly_temp_data_MVD <- temp_data_MVD %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE)) %>%
  mutate(ciudad = "Montevideo")

# Y la graficamos
dayly_temp_data_MVD %>%
  ggplot(aes(x = date, y = avg_temp)) +
  geom_line(color = "darkorange") +
  labs(title = "Temperatura promedio por día en Montevideo",
       x = "Fecha",
       y = "Temperatura (°C)") +
  theme_minimal()

# Ajustemos un cosinor para los datos de Montevideo
library(GLMMcosinor)

cosinor_MVD <- cglmm(
  avg_temp ~ amp_acro(date, period = 365.25),
  data = dayly_temp_data_MVD
)

dayly_temp_data_MVD <- dayly_temp_data_MVD %>%
  mutate(day_number = as.numeric(difftime(date, min(date), units = "days")))

cosinor_MVD <- cglmm(
  avg_temp ~ amp_acro(day_number, period = 365.25),
  data = dayly_temp_data_MVD
)

summary(cosinor_MVD)

autoplot(cosinor_MVD, superimpose.data = TRUE) 

# Ahora bajamos los datos de Mendoza
latitude <- -32.8895
longitude <- -68.8458

# Bajamos los datos de temperatura de Open Meteo
temp_data_MEN <- read.table(paste0(
  "https://archive-api.open-meteo.com/v1/archive?latitude=",
  latitude, "&longitude=", longitude,
  "&start_date=", start_date,
  "&end_date=", end_date,
  "&hourly=temperature_2m&timezone=auto&format=csv"),
  skip =3, header = T, sep = ",") %>%
  # reformat dates
  mutate(time = gsub("T", " ", time)) %>%
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M", tz="America/Argentina/Buenos_Aires")) %>%
  rename(temp = temperature_2m...C.)

# Calculamos la media diaria de temperatura
dayly_temp_data_MEN <- temp_data_MEN %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE)) %>%
  mutate(ciudad = "Mendoza")

# Combinamos ambos datos
dayly_temp_MVD_MEN <- dayly_temp_data_MVD %>%
  bind_rows(dayly_temp_data_MEN)
  
# Y la graficamos
dayly_temp_MVD_MEN %>%
  ggplot(aes(x = date, y = avg_temp, color = ciudad)) +
  geom_line() +
  labs(title = "Temperatura promedio por día en Montevideo y Mendoza",
       x = "Fecha",
       y = "Temperatura (°C)",
       color = NULL) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

# Ahora ajustemos dos cosinores y veamos la diferencia entre ambos 

dayly_temp_MVD_MEN <- dayly_temp_MVD_MEN %>%
  mutate(day_number = as.numeric(difftime(date, min(date), units = "days")))

cosinor_MVD_MEN <- cglmm(
  avg_temp ~ ciudad + amp_acro(day_number, group = ciudad, period = 365.25),
  data = dayly_temp_MVD_MEN
)

summary(cosinor_MVD_MEN)

autoplot(cosinor_MVD_MEN, x_str = "ciudad", superimpose.data = TRUE) 

test_cosinor_levels(cosinor_MVD_MEN, param = "amp", x_str = "ciudad")
test_cosinor_levels(cosinor_MVD_MEN, param = "acr", x_str = "ciudad")

# Ahora una ciudad con baja y alta amplitud térmica para hacer inferencia sobre la amplitud

# Ahorados ciudades en distintos hemisferios para ver diferencias de fase
#Montreal
latitude <- 45.5088
longitude <- -73.5878

# Bajamos los datos de temperatura de Open Meteo
temp_data_MTL <- read.table(paste0(
  "https://archive-api.open-meteo.com/v1/archive?latitude=",
  latitude, "&longitude=", longitude,
  "&start_date=", start_date,
  "&end_date=", end_date,
  "&hourly=temperature_2m&timezone=auto&format=csv"),
  skip =3, header = T, sep = ",") %>%
  # reformat dates
  mutate(time = gsub("T", " ", time)) %>%
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M", tz="America/Argentina/Buenos_Aires")) %>%
  rename(temp = temperature_2m...C.)

# Calculamos la media diaria de temperatura
dayly_temp_data_MTL <- temp_data_MTL %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE)) %>%
  mutate(ciudad = "Montreal")

# Combinamos con los datos de MVD
dayly_temp_MVD_MTL <- dayly_temp_data_MVD %>%
  bind_rows(dayly_temp_data_MTL)

dayly_temp_MVD_MTL %>%
  ggplot(aes(x = date, y = avg_temp, color = ciudad)) +
  geom_line() +
  labs(title = "Temperatura promedio por día en Montevideo y Montreal",
       x = "Fecha",
       y = "Temperatura (°C)",
       color = NULL) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

# Ahora ajustemos dos cosinores y veamos la diferencia entre ambos 
dayly_temp_MVD_MTL <- dayly_temp_MVD_MTL %>%
  mutate(day_number = as.numeric(difftime(date, min(date), units = "days")))

cosinor_MVD_MTL <- cglmm(
  avg_temp ~ ciudad + amp_acro(day_number, group = ciudad, period = 365.25),
  data = dayly_temp_MVD_MTL
)

summary(cosinor_MVD_MTL)

autoplot(cosinor_MVD_MTL, x_str = "ciudad", superimpose.data = TRUE) 

test_cosinor_levels(cosinor_MVD_MTL, param = "amp", x_str = "ciudad")
test_cosinor_levels(cosinor_MVD_MTL, param = "acr", x_str = "ciudad")

# Levantar los datos de tobas de 2023 y 2025
# Cargo los datos
# ¡abrir el archivo de metadata para conocer el contenido de los archivos!
# eventos de sueño
sleep <- read_csv("./data/toba_data/Sleep_data_Toba_Qom.csv")
head(sleep)

# metadata demográfica
demog <- read_csv("./data/toba_data/Demographics_Toba_Qom.csv") %>%
  mutate(ID = as.character(ID),  # para que matchee con el ID de sleep
         Group = factor(Group, levels = c("Rural no light",  # para ordenar los niveles
                                          "Rural limited light",
                                          "Urban")))
head(demog)

# creo un tibble unificado
toba_sleep <- sleep %>%
  right_join(demog) %>% # uno los datos demográficos
  drop_na() %>%
  mutate(Start_Date = as.Date(Start_Date, format = "%m/%d/%Y"))
head(toba_sleep)

rm(sleep, demog) # Borro del environment a sleep y demog

# Convertimos la fecha a algo numérico y la ploteamos
toba_sleep <- toba_sleep %>%
  mutate(day_number = as.numeric(difftime(Start_Date, min(Start_Date), units = "days")))

toba_sleep %>%
  ggplot(aes(x = Start_Date, y = Duration/60, color = Group)) +
  geom_point(alpha = 0.5) +
  labs(title = "Duración del sueño en comunidades Toba-Qom (2016-2018)",
       x = "Fecha",
       y = "Duración del sueño (horas)",
       color = "Comunidad") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

# Tratemos de ver todo junto en un año
toba_sleep <- toba_sleep %>%
  mutate(year_date = yday(as.Date(Start_Date, format = "%m/%d/%Y")))

toba_sleep %>%
  ggplot(aes(x = year_date, y = Duration/60, color = Group)) +
  geom_point(alpha = 0.5) +
  labs(title = "Duración del sueño en comunidades Toba-Qom (2016-2018)",
       x = "Fecha",
       y = "Duración del sueño (horas)",
       color = "Comunidad") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

toba_sleep_urban_rural <- toba_sleep %>%
  filter(Group != "Rural no light")

toba_sleep_urban_rural %>%
  ggplot(aes(x = year_date, y = Duration/60, color = Group)) +
  geom_point(alpha = 0.5) +
  labs(title = "Duración del sueño en comunidades Toba-Qom (2016-2018)",
       x = "Fecha",
       y = "Duración del sueño (horas)",
       color = "Comunidad") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

# Ahora ajustemos dos cosinores y veamos la diferencia entre ambos al período lunar
cosinor_toba_sleep <- cglmm(
  Duration ~ Group + amp_acro(year_date, group = Group, n_components = 1,
                              period = 29.53),
  data = toba_sleep_urban_rural
)

summary(cosinor_toba_sleep)

cosinor_toba_sleep_mixed <- cglmm(
  Duration ~ Group + amp_acro(year_date, group = Group, n_components = 1,
                              period = 29.53) + 
    (1 + amp_acro1 | ID),
  data = toba_sleep_urban_rural,
)

summary(cosinor_toba_sleep_mixed)

ggplot(cbind(toba_sleep_urban_rural, 
             pred = predict(cosinor_toba_sleep_mixed))) +
  geom_point(aes(x = year_date, y = Duration, color = Group), alpha = .1) +
  geom_line(aes(x = year_date, y = pred, color = Group, group = ID)) +
  theme_minimal() +
  labs(title = "Ajuste cosinor lunar",
       x = "Día del año",
       y = "Duración del sueño (minutos)",
       color = "Comunidad") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

test_cosinor_levels(cosinor_toba_sleep_mixed, param = "amp", x_str = "Group")
test_cosinor_levels(cosinor_toba_sleep_mixed, param = "acr", x_str = "Group")

ggplot(cbind(toba_sleep_urban_rural, 
             pred = predict(cosinor_toba_sleep_mixed))) +
  geom_line(aes(x = year_date, y = pred, color = Group, group = ID)) +
  theme_minimal() +
  labs(title = "Ajuste cosinor lunar",
       x = "Día del año",
       y = "Duración del sueño (minutos)",
       color = "Comunidad") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top")

# Los ejercicios ####
# La data Posisson
data_poisson <- simulate_cosinor(
  n = 100,
  mesor = 1,
  amp = 1,
  acro = 1.2,
  period = 12,
  beta.group = TRUE,
  beta.mesor = 0.4,
  beta.amp = 0.5,
  beta.acro = 0.2,
  n_period = 3,
  n_components = 1,
  family = "poisson"
) %>%
  mutate(group = factor(group),
         times = as.numeric(times))

# Graficar los datos en función del tiempo y el grupo
# Ajustar un modelo cosinor con el término de grupo
# Interpretar la salida del modelo
# Comprar las acrofases y amplitudes entre los grupos

# La data con efectos mixtos
f_sample_id <- function(id_num,
                        n = 30,
                        mesor,
                        amp,
                        acro,
                        family = "gaussian",
                        sd = 0.2,
                        period,
                        n_components,
                        beta.group = TRUE) {
  data <- simulate_cosinor(
    n = n,
    mesor = mesor,
    amp = amp,
    acro = acro,
    family = family,
    sd = sd,
    period = period,
    n_components = n_components
  )
  data$subject <- id_num
  data
}

dat_mixed <- do.call(
  "rbind",
  lapply(1:30, function(x) {
    f_sample_id(
      id_num = x,
      mesor = rnorm(1, mean = 0, sd = 1),
      amp = rnorm(1, mean = 3, sd = 0.5),
      acro = rnorm(1, mean = 1.5, sd = 0.2),
      period = 24,
      n_components = 1
    )
  })
)
dat_mixed$subject <- as.factor(dat_mixed$subject)

# Graficar los datos en función del tiempo y el sujeto
# Ajustar un modelo cosinor con efectows mixtos
# Interpretar la salida del modelo

# Solución Poisson ####
ggplot(data_poisson, aes(x = times, y = Y, color = group)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Datos simulados Poisson",
       x = "Tiempo",
       y = "Respuesta",
       color = "Grupo") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

cosinor_poisson <- cglmm(
  Y ~ group + amp_acro(times, group = group, period = 12),
  data = data_poisson,
  family = "poisson"
)
summary(cosinor_poisson)
autoplot(cosinor_poisson, x_str = "group", superimpose.data = TRUE)

test_cosinor_levels(cosinor_poisson, param = "amp", x_str = "group")
test_cosinor_levels(cosinor_poisson, param = "acr", x_str = "group")

# Solución efectos mixtos ####
ggplot(dat_mixed, aes(x = times, y = Y, color = subject)) +
  geom_point() +
  geom_line() +
  labs(title = "Datos simulados con efectos mixtos",
       x = "Tiempo",
       y = "Respuesta",
       color = "Sujeto") +
  theme_minimal() +
  theme(legend.position = "none")

mixed_mod <- cglmm(
  Y ~ amp_acro(times, n_components = 1, period = 24) + 
    (1 + amp_acro1 | subject),
  data = dat_mixed
)

autoplot(mixed_mod, superimpose.data = TRUE)

ggplot(cbind(dat_mixed, pred = predict(mixed_mod))) +
  geom_point(aes(x = times, y = Y, col = subject)) +
  geom_line(aes(x = times, y = pred, col = subject))
