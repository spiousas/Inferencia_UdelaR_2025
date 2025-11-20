pacman::p_load(tidyverse, latex2exp)

# Simulemos un experimento de Bernoulli ####
## Cómo evoluciona la media con n ####

# Con X1 y X2
n <- 200
medias <- rep(NA, n)
X1 <- c()
set.seed(123)
for (i in 1:n) {
  X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
  medias[i] <- mean(X1)
}

medias_tbl <- tibble(medias) %>%
  mutate(n = 1:n)

medias_tbl %>%
  ggplot(aes(x = n, y = medias)) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "darkred", linetype = "dashed") +
  geom_vline(xintercept = 100, color = "darkred", linetype = "dashed") +
  geom_line(linewidth = 1, color = "darkorange") +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("f2825b4f.png", width = 6, height = 4)

## Y varios experimentos al mismo tiemp ####
n_final <- 200
M <- 20 # 100 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))

medias_tbl %>%
  ggplot(aes(x = n, y = medias, group = experiment)) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "darkred", linetype = "dashed") +
  geom_vline(xintercept = 100, color = "darkred", linetype = "dashed") +
  geom_line(linewidth = .5, alpha = .5) +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")

## La distribución de la media muestral para n25 y n100 ####
medias_25 <- medias_tbl %>%
  filter(n == 25)

medias_25 %>%
  ggplot(aes(x = medias)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal()

medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 200 experimentos ####
n_final <- 200
M <- 200 # 100 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 2000 experimentos ####
n_final <- 100
M <- 2000 # 100 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rbinom(n = 1, size = 1, prob = 0.5))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 23, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 2000 experimentos ####
n_final <- 2000
M <- 20000 # 100 experimentos
medias <- c()

set.seed(1234)
for (k in 1:M) {
  X1 <-  rbinom(n = n_final, size = 1, prob = 0.5)
  medias <- c(medias, mean(X1))
}

medias_tbl <- tibble(medias) %>%
  mutate(experiment = 1:M)

medias_tbl %>%
  ggplot(aes(x = medias)) +
  geom_histogram(aes(y = ..density..), fill = "darkorange", bins = 30, color = "black", alpha = .5) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed") +
  stat_function(fun = dnorm, args = list(mean = 0.5, sd = sqrt(0.5*0.5/n_final)), color = "blue", size = 1) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Las rachas de ceros y unos ####

# Hacemos un experimento tirando una moneda y de una serie de 120 tiradas contamos la racha más larga de unos
set.seed(123)
n_tiradas <- 200
n_experimentos <- 1000
rachas <- rep(NA, n_experimentos)
for (j in 1:n_experimentos) {
  tiradas <- rbinom(n = n_tiradas, size = 1, prob = 0.5)
  racha_actual <- 0
  racha_maxima <- 0
  for (i in 1:n_tiradas) {
    if (tiradas[i] == 1) {
      racha_actual <- racha_actual + 1
      if (racha_actual > racha_maxima) {
        racha_maxima <- racha_actual
      }
    } else {
      racha_actual <- 0
    }
  }
  rachas[j] <- racha_maxima
}

rachas_tbl <- tibble(rachas)

rachas_tbl %>%
  ggplot(aes(x = rachas)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "darkorange", color = "black", alpha = .5) +
  labs(x = "Racha máxima en 200 tiradas",
       y = "Densidad") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, max(rachas)), breaks = seq(0, max(rachas), by = 2)) +
  theme(legend.position = "top", legend.title = element_blank())

# Ahora veamos que pasa con la media de una normal ####

## Con 20 experimentos ####
n_final <- 200
M <- 20 # 20 experimentos
mu <- 10
sigma <- 2
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rnorm(n = 1, mean = mu, sd = sigma))    
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))

medias_tbl %>%
  ggplot(aes(x = n, y = medias, group = experiment)) +
  geom_hline(yintercept = 10, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "darkred", linetype = "dashed") +
  geom_vline(xintercept = 100, color = "darkred", linetype = "dashed") +
  geom_line(linewidth = .5, alpha = .5) +
  labs(color = NULL,
       x = "Tamaño de la muestra",
       y = "Media muestral") +
  theme_minimal() +
  theme(legend.position = "top")

## Con 20 experimentos ####
n_final <- 200
M <- 20 # 20 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rnorm(n = 1, mean = mu, sd = sigma))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = .5) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 200 experimentos ####
n_final <- 200
M <- 2000 # 2000 experimentos
medias <- c()

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  for (i in 1:n_final) {
    X1 <- c(X1, rnorm(n = 1, mean = mu, sd = sigma))
    medias <- c(medias, mean(X1))
  }  
}

medias_tbl <- tibble(medias) %>%
  mutate(n = rep(1:n_final, M),
         experiment = rep(1:M, each = n_final))
medias_25_100 <- medias_tbl %>%
  filter(n %in% c(25, 100))

medias_25_100 %>%
  ggplot(aes(x = medias, fill = factor(n))) +
  geom_histogram(aes(y = ..density..), position = 'identity', bins = 40, color = "black", alpha = .4) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("n=25", "n=100")) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma/sqrt(25)), color = "steelblue", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma/sqrt(100)), color = "darkorange", size = 1) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## Con 1000 experimentos ####
n <- 1000
M <- 2000 # 2000 experimentos
medias_1 <- c()
medias_2 <- c()
mu1 <- 10
mu2 <- 9.5

set.seed(123)
for (k in 1:M) {
  X1 <- c()
  X2 <- c()
  X1 <- rnorm(n = n, mean = mu1, sd = sigma)
  X2 <- rnorm(n = n, mean = mu2, sd = sigma)
  medias_1 <- c(medias_1, mean(X1))
  medias_2 <- c(medias_2, mean(X2))
}

medias_tbl <- tibble(medias = c(medias_1, medias_2)) %>%
  mutate(condicion = rep(c("pre", "post"), each = M))

medias_tbl %>%
  ggplot(aes(x = medias, fill = condicion)) +
  geom_histogram(aes(y = ..density..), position = 'identity', bins = 40, color = "black", alpha = .4) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("post", "pre")) +
  stat_function(fun = dnorm, args = list(mean = mu2, sd = sigma/sqrt(1000)), color = "steelblue", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mu1, sd = sigma/sqrt(1000)), color = "darkorange", size = 1) +
  labs(x = "Media muestral",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

## hagamos una realización

st.seed(421)
mean(rnorm(n = 25, mean = mu1, sd = sigma)) 
mean(rnorm(n = 25, mean = mu2, sd = sigma))

# Simulemos un experimento de Bernoulli ####
## Cómo evoluciona la media con n ####

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(30)), color = "steelblue", size = 1) +
  labs(x = "T30",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(30)), color = "steelblue", size = 1) +
  geom_vline(xintercept = 1.5, color = "darkorange", linetype = "dashed") +
  geom_vline(xintercept = 1.59, color = "darkred", linetype = "dashed") +
  labs(x = "T30",
       y = "Densidad") +
  theme_minimal()

ggplot(data = data.frame(x = c(-3, 3)), aes(x))+
  stat_function(mapping = aes(fill = "Area 02"), geom = "area", fun = function(x) ifelse(abs(x) > 1.6, dnorm(x), NA)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(-1.6, 1.6), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\bar{X}$)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none", panel.background = element_rect(fill = "#EEEEEE"), 
        plot.background = element_rect(fill = "#EEEEEE"))

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(30)), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(1.5-.09, 1.59), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\bar{X}$)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none")

2*pnorm(1.41, mean = 1.5, sd = .5/sqrt(30))

ggplot(data = data.frame(x = c(1.125, 1.875)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 1.5, sd = .5/sqrt(200)), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(1.5-.09, 1.59), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\bar{X}$)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none")

2*pnorm(1.41, mean = 1.5, sd = .5/sqrt(200))

set.seed(123)
x <- rnorm(30, mean = 1.75, sd = .5)
mean(x)
sd(x)/sqrt(30)
t_obs <- (mean(x) - 1.5)/(sd(x)/sqrt(30))
pt(mean(x), df = 29, lower.tail = FALSE)*2
t.test(x, mu = 1.5, alternative = "two.sided")
pt(t_obs, df = 29, lower.tail = FALSE)*2

ggplot(data = data.frame(x = c(-4,4)), aes(x)) +
  stat_function(mapping = aes(fill = "Area 02"), geom = "area", fun = function(x) ifelse(abs(x) > t_obs, dnorm(x), NA)) +
  stat_function(fun = dt, args = list(df = 29), color = "steelblue", size = 1) +
  geom_vline(xintercept = c(-t_obs, t_obs), color = "darkorange", linetype = "dashed") +
  labs(x = TeX(r"($\frac{\bar{T}_{30} - \mu_0}{\sqrt{S^2/30}} $)"),
       y = "Densidad") +
  theme_linedraw() +
  theme(legend.position = "none")

# Simulemos un experimento de una muestra normal ####
# Generamos una muestra 
set.seed(123)
x <- rnorm(20, mean = 1, sd = 3)

# Su histograma con una función base de R
hist(x)

# Y si nos ponemos chetardos con ggplot2
tibble(x) %>%
  ggplot(aes(x = x, y = ..density..)) +
  geom_histogram(fill = "steelblue", color = "white", binwidth = 2) +
  theme_bw()

# La media y la sd muestral     
mean(x)
sd(x)

# La distribución de las medias después de tomar 104 muestras
means <- c()
n <- 20
mu <- 1
sigma <- 3

set.seed(123)
for (i in 1:10000) {
  x_i <- rnorm(n, mean = mu, sd = sigma)
  means <- c(means, mean(x_i))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()

# La distribución de las medias normalizadas cuando H0 es verdadera
Zs <- c()
set.seed(123)
for (i in 1:10000) {
  x_i <- rnorm(20, mean = 0, sd = 3)
  Zs <- c(Zs, (mean(x_i))/sqrt(3^2/20))
}

Z_means <- tibble(Zs)
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

# Pongo la muestra x en la distribución cuando H0 es verdadera
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  geom_vline(xintercept = mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

# Y también grafico los valores críticos
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  geom_vline(xintercept = c(1, -1) * mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1, -1) * 1.96, linetype = "dashed", color = "blue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

# El p valor con sigma conocido
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

# La distribución t, cuando el sigma no es conocido
# primero con n=6
norm_mean <- c()
n <- 20
mu <- 1
sigma <- 3
set.seed(123)
for (i in 1:10000) {
  x_i <- rnorm(n, mean = mu, sd = sigma)
  norm_mean <- c(norm_mean, (mean(x_i)-mu)/sqrt(sd(x)^2/n))
}

# La distribución comparada con la normal (son casi iguales para este n)
norm_means <- tibble(norm_mean)
norm_means %>% ggplot() +
  geom_histogram(aes(x = norm_mean, y = ..density..), fill = "steelblue", alpha = .4) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = n-1), 
                color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  theme_bw()

# Veamos para n = 6 y n = 20 comparada con la normal
# normal -> negro
# t con n=6 -> rojo
# t con n=20 -> verde
norm_means %>% ggplot() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 5), 
                color = "red", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 19), 
                color = "darkgreen", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  theme_bw()

# La distribución de los p-valores cuando H0 es verdadera
ps <- c()
set.seed(123)
for (i in 1:1E4) {
  x <- rnorm(20, mean = 0, sd = 3)
  p <- 2*pnorm(-abs(mean(x)/sqrt(3^2/20)))
  ps <- c(ps, p)
}

p_values <- tibble(ps)
p_values %>% ggplot(aes(x = ps)) +
  geom_histogram(fill = "steelblue", color = "white", 
                 binwidth = .05, boundary = .05) +
  theme_bw()


# La distribución de los p-valores cuando H0 es falsa
ps <- c()
set.seed(123)
for (i in 1:1E4) {
  x <- rnorm(20, mean = 1, sd = 3)
  p <- 2*pnorm(-abs(mean(x)/sqrt(3^2/20)))
  ps <- c(ps, p)
}

p_values <- tibble(ps)
p_values %>% ggplot(aes(x = ps)) +
  geom_histogram(fill = "steelblue", color = "white", 
                 binwidth = .05, boundary = .05) +
  theme_bw()

# El p valor con t y normal
set.seed(123)
x <- rnorm(20, mean = 1, sd = 3) # La misma muestra que al principio

# Sigma conocido
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

# Sigma desconocido
2*pt(-abs(mean(x)/sqrt(sd(x)^2/20)), df = 19)


# Una muestra que es un falso negativo
set.seed(12)
x <- rnorm(20, mean = 1, sd = 3)
mean(x)
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue", alpha = .4) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = c(1, -1) * mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1, -1) * qnorm(0.05/2), linetype = "dashed", color = "blue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = 1/sqrt(9/20), sd = 1), 
                color = "red", linewidth = 1) +
  theme_bw()

# La potencia
1 + pnorm(qnorm(.05/2)-1/(sqrt(3^2/20))) -
  pnorm(qnorm(1-.05/2)-1/(sqrt(3^2/20)))

# Creamos una función para calcular la potencia
potencia <- function(sigma, mu, n, alpha) {
  pnorm(qnorm(alpha/2)-mu/(sqrt(sigma^2/n))) + 1 -
    pnorm(qnorm(1-alpha/2)-mu/(sqrt(sigma^2/n)))
}

# La potencia en función de mu
pot <- potencia(3,seq(-3,3,.1),20,0.05)
pot_tbl <- tibble(mu = seq(-3,3,.1)) %>%
  mutate(potencia = potencia(3,mu,20,0.05))

pot_tbl %>% ggplot(aes(x = mu,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()

# La potencia en función de alpha
pot_tbl <- tibble(alpha = seq(0, 0.05, 0.001)) %>%
  mutate(potencia = potencia(3,1,20,alpha))

pot_tbl %>% ggplot(aes(x = alpha,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()

# La potencia en función de n
pot_tbl <- tibble(n = seq(5, 200)) %>%
  mutate(potencia = potencia(3,1,n,.05))

pot_tbl %>% ggplot(aes(x = n,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()


# Effect size
set.seed(123)
u <- rnorm(20000, mean = .1, sd = 3)

t.test(u)

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(20000, mean = .1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "n = 20000") +
  theme_bw()

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(100, mean = .1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "n = 100") +
  theme_bw()

# Qué pasa cuando no hay normalidad (distribución de las medias de una exponencial)

# Empecemos con n=100
# Las Xs
set.seed(123)
n <- 100
Xs <- tibble(x = rexp(n,rate = 1))

Xs %>% ggplot() +
  geom_histogram(aes(x = x, y = ..density..), fill = "steelblue") +
  stat_function(fun = dexp, args = list(rate =1), 
                color = "black", linewidth = 1) +
  labs(x = "V") +
  theme_bw()

mean(x)
sd(x)

# Las medias para n=100
means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rexp(n,rate = 1)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()

# Las medias para n=1000
n = 1000
Zs <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rexp(n, rate = 1)
  Zs <- c(Zs, (mean(x) - 1)/sqrt(sd(x)^2/n))
}

Z_means <- tibble(Zs)
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  labs(title = paste("n = ", n), x = "Medias de v") +
  scale_x_continuous(limits = c(-4, 4)) +
  theme_bw()
