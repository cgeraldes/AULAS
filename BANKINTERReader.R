# Instale os pacotes se ainda não tiver
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("stringr")

library(readxl)
library(dplyr)
library(stringr)

# Caminho para o ficheiro Excel
arquivo <- "BANKINTERCarlos.xls"

# Lê todas as linhas como texto, sem tratar como cabeçalho
dados_brutos <- read_excel(arquivo, col_names = FALSE)

# Visualizar as primeiras linhas para entender onde começa a tabela
print(head(dados_brutos, 10))

# Supondo que a tabela começa quando a primeira coluna tem data no formato "dd-mm-aaaa"
# Detectar linhas que são dados (e não header/footer)
linhas_com_dados <- which(str_detect(as.character(dados_brutos[[1]]), "^\\d{2}-\\d{2}-\\d{4}"))

# Definir os limites da tabela
inicio <- min(linhas_com_dados)
fim <- max(linhas_com_dados)

# Extrair apenas as linhas da tabela
tabela <- dados_brutos[inicio:fim, ]

# Opcional: renomear colunas
colnames(tabela) <- c("Data_Movimento", "Data_Valor", "Descricao", "Transacao", 
                      "Montante", "Moeda", "Saldo", "Taxa_Cambio", "Data_Taxa_Cambio")[1:ncol(tabela)]

# Visualizar
print(tabela)
View(tabela)


# Função para classificar cada descrição
classificar_tipo <- function(descricao) {
  if (grepl("^DISTRIB\\.REND\\.", descricao)) {
    return("Rendimento de Fundos")
  } else if (grepl("^FUNDOS SUBSCRIC|^SUBSCRICAO", descricao)) {
    return("Subscrição de Fundos")
  } else if (grepl("^FUNDOS RESGATE", descricao)) {
    return("Resgate de Fundos")
  } else if (grepl("^COMISSAO DE GUARDA", descricao)) {
    return("Comissão de Guarda")
  } else if (grepl("^IVA \\(OPERACOES DE TITULOS\\)", descricao)) {
    return("IVA sobre Operações")
  } else if (grepl("^TRF A CREDITO", descricao)) {
    return("Transferência a Crédito")
  } else if (grepl("^TRF P2P", descricao)) {
    return("Transferência P2P")
  } else if (grepl("^LEV", descricao)) {
    return("Levantamento")
  } else if (grepl("^PAGAM\\.SEG\\.SOCIAL", descricao)) {
    return("Pagamento Segurança Social")
  } else if (grepl("^RENDIMENTOS", descricao)) {
    return("Rendimento de Título")
  } else if (grepl("^AMORTIZACOES", descricao)) {
    return("Amortização")
  } else if (grepl("^JUROS DEVEDORES", descricao)) {
    return("Juros Devedores")
  } else if (grepl("^IMPOSTO SELO|^IMP\\. SELO", descricao)) {
    return("Imposto de Selo")
  } else if (grepl("^PAG\\. SERVICO|^PAGAMENTO SERVICOS", descricao)) {
    return("Pagamento de Serviço")
  } else if (grepl("^DISPONIBILIZACAO DE UM CARTAO DE DEBITO", descricao)) {
    return("Cartão de Débito")
  } else if (grepl("^COMPRA", descricao)) {
    return("Compra com Cartão")
  } else {
    return("Outro")
  }
}

# Garante que 'descricao' é character
descricao_vector <- as.character(tabela$Descricao)

# Aplica a classificação
tipo_vector <- sapply(descricao_vector, classificar_tipo)

# Adiciona coluna
tabela$tipo <- tipo_vector
