library("forecast")
library("pracma")
set.seed(123)

results_file <- "resultados_df_checkpoint.csv"

# criar ficheiro vazio com cabeçalho
write.table(
  resultados_df,
  file = results_file,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE
)


log_file <- "erros_gerar_ARn_autoSARIMA.csv"

write.table(
  data.frame(
    p = integer(),
    phi_real = character(),
    mensagem = character()
  ),
  file = log_file,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE
)

#========================================================
#  1. Geração de processo AR(n) + seleção automática via auto.arima
#========================================================
gerar_ARn_autoSARIMA <- function(phi.real,
                                 n = 2000,
                                 burn = 200,
                                 seasonal = FALSE,
                                 freq = NA,
                                 plotar = FALSE) {
  
  ## --------------------------------------------------------
  ## Preparação
  ## --------------------------------------------------------
  p <- length(phi.real)
  message("Gerando série AR(", p, ") ...", sep = "")
  
  ## --------------------------------------------------------
  ## Função auxiliar: cálculo dos pólos AR no plano-Z
  ## --------------------------------------------------------
  polos_AR <- function(phi) {
    polyroot(rev(c(1, -phi)))
  }
  
  ## --------------------------------------------------------
  ## Simulação do processo AR(p) com burn-in
  ## --------------------------------------------------------
  x.total <- arima.sim(
    n = n + burn,
    model = list(ar = phi.real)
  )
  
  # Remoção do período transiente
  x.total <- x.total[(burn + 1):(n + burn)]
  
  # Conversão para série temporal sazonal (se aplicável)
  if (seasonal && !is.na(freq)) {
    x.total <- ts(x.total, frequency = freq)
  }
  ## --------------------------------------------------------
  ## Separação da amostra treino/teste
  ## --------------------------------------------------------
  n <- length(x.total)
  n_train <- floor(0.7 * n)
  
  train_idx <- 1:n_train
  test_idx  <- (n_train + 1):n
  x = x.total[train_idx]
  x.teste = x.total[test_idx]
  
  ## --------------------------------------------------------
  ## Estimação AR por diferentes métodos
  ## --------------------------------------------------------
  
  # Método de Burg
  fit_burg <- ar(
    x,
    order.max = p,
    method = "burg"
  )
  
  # ARIMA(p,0,0) clássico
  fit_arima <- arima(
    x,
    order = c(p, 0, 0),
    include.mean = FALSE
  )
  
  # Seleção automática ARIMA / SARIMA
  fit_auto <- auto.arima(
    x,
    seasonal = seasonal,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  ## --------------------------------------------------------
  ## Tabela comparativa de parâmetros
  ## --------------------------------------------------------
  res <- data.frame(
    Parametro  = paste0("phi", 1:p),
    Verdadeiro = phi.real,
    Burg       = fit_burg$ar[1:p],
    ARIMA      = fit_arima$coef[1:p],
    row.names  = NULL
  )
  
  print(res)
  
  ## --------------------------------------------------------
  ## Informações do modelo escolhido pelo auto.arima
  ## --------------------------------------------------------
  cat("\nModelo escolhido pelo auto.arima():\n")
  print(fit_auto)
  
  cat("\nEstrutura do modelo selecionado:\n")
  cat(
    "ARIMA(",
    fit_auto$arma[1], ",",
    fit_auto$arma[6], ",",
    fit_auto$arma[2], ")\n",
    sep = ""
  )
  
  cat("\nAIC auto.arima:", fit_auto$aic, "\n")
  
  ## --------------------------------------------------------
  ## Cálculo dos pólos no plano-Z
  ## --------------------------------------------------------
  polos_real  <- polos_AR(phi.real)
  polos_burg  <- polos_AR(fit_burg$ar[1:p])
  polos_arima <- polos_AR(fit_arima$coef[1:p])
  polos_auto  <- polos_AR(fit_auto$coef[1:p])
  
  ## --------------------------------------------------------
  ## Output
  ## --------------------------------------------------------
  return(list(
    serie  = x,
    teste  = x.teste,
    tabela = res,
    
    modelos = list(
      burg  = fit_burg,
      arima = fit_arima,
      auto  = fit_auto
    ),
    
    polos = list(
      real  = polos_real,
      burg  = polos_burg,
      arima = polos_arima,
      auto  = polos_auto
    )
  ))
}
#========================================================
#  2. Estimativa ótima de ganho escalar (α)
#========================================================
alpha_opt <- function(x, yhat) {
  # Estima α que minimiza ||x − α·yhat||²
  sum(x * yhat, na.rm = TRUE) /
    sum(yhat^2, na.rm = TRUE)
}
#========================================================
#  3. Pólos de filtro Butterworth via Matched Z-Transform
#========================================================
butterworth_polos_MZT <- function(A, N, fs) {
  
  ## --------------------------------------------------------
  ## Parâmetros de entrada
  ## A  : atenuação em dB
  ## N  : ordem do filtro
  ## fs : frequência de amostragem (Hz)
  ## --------------------------------------------------------
  
  ## Constantes
  T <- 1 / fs
  f_nyquist <- fs / 2
  
  ## Atenuação linear
  Hs <- 10^(-A / 10)
  
  ## Frequência de corte analógica
  fc <- f_nyquist / ((1 / Hs - 1)^(1 / (2 * N)))
  wc <- 2 * pi * fc
  
  ## --------------------------------------------------------
  ## Pólos analógicos do Butterworth
  ## --------------------------------------------------------
  k <- 1:N
  s_k <- wc * exp(
    1i * (pi / 2 + (2 * k - 1) * pi / (2 * N))
  )
  
  ## --------------------------------------------------------
  ## Mapeamento Matched Z-Transform
  ## --------------------------------------------------------
  z_k <- exp(s_k * T)
  
  ## --------------------------------------------------------
  ## Output
  ## --------------------------------------------------------
  return(list(
    A_dB = A,
    N = N,
    fs = fs,
    fc = fc,
    wc = wc,
    polos_s = s_k,
    polos_z = z_k
  ))
}
#========================================================
#  4. Predição de um modelo AR(p)
#========================================================
yhat_ar <- function(x, phi) {
  
  p <- length(phi)
  n <- length(x)
  
  yhat <- rep(NA, n)
  
  for (t in (p + 1):n) {
    yhat[t] <- sum(
      phi * rev(x[(t - p):(t - 1)])
    )
  }
  
  return(yhat)
}
#========================================================
#  5. Cálculo do AIC a partir dos resíduos
#========================================================
AIC_from_residuals <- function(e, k = 1) {
  
  ## --------------------------------------------------------
  ## e : resíduos do modelo
  ## k : número de parâmetros estimados
  ##     (ex.: AR Butterworth → k = 1)
  ## --------------------------------------------------------
  
  e <- e[!is.na(e)]
  n <- length(e)
  
  ## Variância MLE
  sigma2_hat <- mean(e^2)
  
  ## Métricas
  rmse <- sqrt(sigma2_hat)
  
  logLik <- -n / 2 * (
    log(2 * pi) +
      log(sigma2_hat) +
      1
  )
  
  AIC <- -2 * logLik + 2 * k
  
  return(list(
    rmse = rmse,
    sigma2 = sigma2_hat,
    logLik = logLik,
    AIC = AIC
  ))
}
#========================================================
#  6. Cálculo do RMSE
#========================================================
rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm = TRUE))
}
#========================================================
#  7. Cálculo do MSE
#========================================================
mse <- function(y, yhat) {
  mean((y - yhat)^2, na.rm = TRUE)
}
#========================================================
#  8. Função para testar estacionaridade AR(p)
#========================================================
is_stationary_AR <- function(phi) {
  roots <- polyroot(c(1, -phi))
  all(Mod(roots) > 1)
}

#========================================================
#  9. Gerador de modelos AR 
#========================================================
gerar_ARp_random <- function(p,
                             n_models = 1000,
                             phi_min = 0.1,
                             phi_max = 1.0,
                             only_stationary = TRUE) {
  
  phis <- list()
  i <- 1
  
  while (length(phis) < n_models) {
    print(i)
    phi <- runif(p, phi_min, phi_max)
    
    if (!only_stationary || is_stationary_AR(phi)) {
      phis[[i]] <- phi
      i <- i + 1
    }
  }
  
  return(phis)
}
gerar_ARp_varrimento <- function(p_min = 3,
                                 p_max = 30,
                                 n_models_per_p = 200) {
  
  res <- list()
  
  for (p in p_min:p_max) {
    res[[paste0("p_", p)]] <-
      gerar_ARp_random(
        p = p,
        n_models = n_models_per_p
      )
  }
  
  return(res)
}


###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
## ========================================================
## 1. Definição do processo AR verdadeiro
## ========================================================

AR_models <- gerar_ARp_varrimento(p_min = 3,
                                  p_max = 4,
                                  n_models_per_p = 4)
resultados_df <- data.frame(
  p = integer(),
  phi_real = I(list()),
  phi_auto = I(list()),
  phi_btw  = I(list()),
  RMSE_auto.test  = numeric(),
  RMSE_btw.test   = numeric(),
  RMSE_auto.train = numeric(),
  RMSE_btw.train  = numeric(),
  delta_RMSE      = numeric(),
  stringsAsFactors = FALSE
)


for (p_name in names(AR_models)) {
  
  cat("Processando", p_name, "\n")
  
  for (phi.real in AR_models[[p_name]]) {
    
p=length(phi.real)
    
# Coeficientes reais do processo AR(p)
#phi.real <- c(0.6, -0.3, 0.2, 0.1, -0.5)
#phi.real <- c(0.2230762,0.2061473,0.3391942,0.2200997)

# Geração da série AR e estimação automática via ARIMA/SARIMA
resultado <- tryCatch({
  gerar_ARn_autoSARIMA(phi.real)
}, error = function(e) {
# Registar erro
  write.table(
    data.frame(
      p = p,
      phi_real = paste(phi.real, collapse = ","),
      mensagem = e$message
    ),
    file = log_file,
    sep = ";",
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
  
  return(NULL)
})
# Se houve erro, passa ao próximo phi.real
if (is.null(resultado)) {
  next
}


## ========================================================
## 2. Parâmetros de amostragem
## ========================================================

# Frequência de amostragem (Hz)
fs <- 12000

# Período de amostragem
T <- 1 / fs


## ========================================================
## 3. Modelo AR estimado via auto.arima
## ========================================================

# Objeto ARIMA estimado automaticamente
ar.estimado <- resultado$modelos$auto

# Série ajustada (valores previstos in-sample)
ar.estimado.y <- fitted(ar.estimado)

# Parâmetros estimados do modelo
ar.estimado.params <- coef(ar.estimado)

# Ordem do modelo ARIMA (p, d, q)
ar.estimado.ordem <- arimaorder(ar.estimado)

# Pólos estimados no plano-Z
ar.estimado.polos <- resultado$polos$auto


## ========================================================
## 4. Processo AR real (referência)
## ========================================================

# Série AR verdadeira simulada
ar.real.y <- resultado$serie

# Série AR verdadeira simulada de teste
ar.real.teste.y <- resultado$teste

# Parâmetros reais do processo
ar.real.params <- phi.real

# Pólos reais do processo AR
ar.real.polos <- resultado$polos$real


## ========================================================
## 5. Dimensões e ordens
## ========================================================

# Número de observações
nobs <- length(ar.real.y)

# Ordem do filtro Butterworth (igual à ordem AR)
N <- p


## ========================================================
## 6. Parâmetros do estudo Butterworth
## ========================================================

# Vetor de atenuações (em dB)
As_vec <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)


## ========================================================
## 7. Inicialização de estruturas de armazenamento
## ========================================================

# Resultados completos do Butterworth
outs <- vector("list", length(As_vec))
names(outs) <- paste0("A_", As_vec)

# Pólos no plano-Z
z_ks <- vector("list", length(As_vec))

# Coeficientes do polinómio característico
as <- vector("list", length(As_vec))

# Coeficientes AR equivalentes ao Butterworth
phis.btw <- vector("list", length(As_vec))

# Métricas de desempenho
rmses <- vector("list", length(As_vec))
AICs  <- vector("list", length(As_vec))

# Melhor predição Butterworth escalada
yhat.BTW.scaled.best <- NULL


## ========================================================
## 8. Loop de avaliação Butterworth → AR equivalente
## ========================================================

for (i in seq_along(As_vec)) {
  
  ## --------------------------------------
  ## 8.1 Projeto do filtro Butterworth
  ## --------------------------------------
  As <- As_vec[i]
  
  outs[[i]] <- butterworth_polos_MZT(
    A  = As,
    N  = N,
    fs = fs
  )
  
  # Pólos mapeados para o plano-Z
  z_ks[[i]] <- outs[[i]]$polos_z
  
  ## --------------------------------------
  ## 8.2 Conversão Butterworth → AR
  ## --------------------------------------
  
  # Coeficientes do polinómio (ordem decrescente)
  as[[i]] <- Re(Poly(z_ks[[i]]))
  
  # Coeficientes AR equivalentes
  phis.btw[[i]] <- -as[[i]][-1]
  
  ## --------------------------------------
  ## 8.3 Predição AR e ajuste de ganho
  ## --------------------------------------
  
  # Predição AR
  yhat.BTW <- yhat_ar(ar.real.y, phis.btw[[i]])
  
  # Ganho ótimo em mínimos quadrados
  alpha_BTW <- alpha_opt(ar.real.y, yhat.BTW)
  
  # Predição escalada
  yhat.BTW.scaled <- alpha_BTW * yhat.BTW
  
  ## --------------------------------------
  ## 8.4 Avaliação do desempenho
  ## --------------------------------------
  
  # Resíduos
  e.BTW.result <- ar.real.y - yhat.BTW.scaled
  
  # Critério de Informação de Akaike
  AICs[[i]] <- AIC_from_residuals(
    e.BTW.result[!is.na(e.BTW.result)]
  )$AIC
  
  # Erro quadrático médio
  rmses[[i]] <- rmse(ar.real.y, yhat.BTW.scaled)
  if(i>1){
    if(rmses[[i]]>rmses[[i-1]]){
      yhat.BTW.scaled.best = yhat.BTW.scaled
    }
  }else{
    yhat.BTW.scaled.best = yhat.BTW.scaled
  }
}
#========================================================
#  RMSE de TESTE — auto.arima vs Butterworth
#========================================================


## --------------------------------------------------------
## 1. Preparação dos dados
## --------------------------------------------------------

# Série completa (treino + teste)
x_total <- c(ar.real.y, ar.real.teste.y)

n_train <- length(ar.real.y)
n_test  <- length(ar.real.teste.y)

train_idx <- 1:n_train
test_idx  <- (n_train + 1):(n_train + n_test)


## --------------------------------------------------------
## RMSE de teste — AR auto.arima usando yhat_ar()
## --------------------------------------------------------

# Extrair coeficientes AR (sem MA)
phi_auto <- ar.estimado$coef[grep("^ar", names(ar.estimado$coef))]

# Predição AR one-step-ahead
yhat_auto_total <- yhat_ar(x_total, phi_auto)

# RMSE de treino — auto.arima
rmse_auto_train <- rmse(
  ar.real.y,
  yhat_auto_total[train_idx]
)

# RMSE de teste — auto.arima
rmse_auto_test <- rmse(
  ar.real.teste.y,
  yhat_auto_total[test_idx]
)

## --------------------------------------------------------
## 3. RMSE de teste — Butterworth (melhor As)
## --------------------------------------------------------

# Escolha do melhor Butterworth com base no RMSE de treino
rmse_train_vec <- unlist(rmses)
best_idx <- which.min(rmse_train_vec)

phi_btw_best <- phis.btw[[best_idx]]

# Predição AR (one-step-ahead) para toda a série
yhat_btw_total <- yhat_ar(x_total, phi_btw_best)

# Estimação do ganho APENAS no treino
alpha_btw <- alpha_opt(
  x_total[train_idx],
  yhat_btw_total[train_idx]
)

# Previsão no treino
yhat_btw_train <- alpha_btw * yhat_btw_total[train_idx]


# Previsão no teste
yhat_btw_test <- alpha_btw * yhat_btw_total[test_idx]

# RMSE de treino — Butterworth
rmse_btw_train <- rmse(
  ar.real.y,
  yhat_btw_train
)

# RMSE de teste (Butterworth)
rmse_btw_test <- rmse(
  ar.real.teste.y,
  yhat_btw_test
)

## --------------------------------------------------------
## 4. Comparação final
## --------------------------------------------------------

## ---------------------------
## Guardar resultados
## ---------------------------
resultados_df <- rbind(
  resultados_df,
  data.frame(
    p = p,
    phi_real = I(list(phi.real)),
    phi_auto = I(list(phi_auto)),
    phi_btw  = I(list(phi_btw_best)),
    RMSE_auto.test = rmse_auto_test,
    RMSE_btw.test  = rmse_btw_test,
    RMSE_auto.train = rmse_auto_train,
    RMSE_btw.train  = rmse_btw_train,
    delta_RMSE = rmse_auto_test - rmse_btw_test
  )
)

write.table(
  resultados_df,
  file = results_file,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE
)


  }
}





