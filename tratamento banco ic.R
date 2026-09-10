###### VARIÁVEIS TEMA #####
library(readxl)
library(dplyr)
library(writexl)

banco <- read_excel("C:\\Users\\DRT904284\\Documents\\banco(1).xlsx")

banco <- banco %>%
  mutate(tema = as.character(tema),
         tema = recode(tema,
                       "v. mul" = "violência contra a mulher",
                      "direitos trabalhistas de gênero" = "direitos trabalhistas",
                      "parentalidade" = "direitos trabalhistas",
                      "licença parental" = "direitos trabalhistas"))

write_xlsx(banco, "C:\\Users\\DRT904284\\Documents\\banco(1).xlsx")

###### VARIÁVEL ESPECTRO POLÍTICO DETALHADA ######
library(tidyr)
library(stringr)

banco <- read_excel("C:\\Users\\DRT904284\\Documents\\banco(1).xlsx")

banco <- banco %>%
  mutate(
    # pega apenas o primeiro autor, antes da vírgula
    autor_partido_1 = str_split(autores_e_partidos, ",\\s*") %>% sapply(`[`, 1)
  ) %>%
  separate(
    autor_partido_1,
    into = c("autor", "partido_uf"),
    sep = " - ",
    extra = "merge",
    fill = "right"
  ) %>%
  separate(
    partido_uf,
    into = c("partido", "uf_autor"),
    sep = "/",
    extra = "merge",
    fill = "right"
  ) %>%
  select(-autores_e_partidos) %>%
  mutate(
    autor = trimws(autor),
    partido = trimws(partido),
    uf_autor = toupper(trimws(uf_autor)),
    regiao_autor = case_when(
      uf_autor %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      uf_autor %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf_autor %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      uf_autor %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      uf_autor %in% c("PR", "RS", "SC") ~ "Sul",)) %>%
  mutate(
    partido = tolower(trimws(partido)),  # padroniza
    espec = case_when(
      partido %in% c("solidariedade", "solidari", "cidadania", "avante", "pmn", "psd", "psdb", "mdb") ~ "centro-direita",
      
      partido %in% c("pros", "agir", "podemos", "pode", "prtb", "pmb", "ptb", "phs") ~ "direita",
      
      partido %in% c("novo", "progressistas", "pp", "patriota", "patri", "pr", "republicanos", "republic", "prb", "psl", "pl", "união", "uniao", "dc", "psc", "dem") ~ "extrema-direita",
      
      TRUE ~ NA_character_
    )
  )

write_xlsx(banco, "C:\\Users\\DRT904284\\Documents\\banco(2).xlsx")

###### VARIÁVEIS DO TSE #####
#Leia-se raça e religião

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)

# 1) Ler as duas bases
banco <- read_excel("C:\\Users\\DRT904284\\Documents\\banco(2).xlsx")
tse   <- read_excel("C:\\Users\\DRT904284\\Documents\\banco tse ic.xlsx")

# 2) Padronizar os nomes nas duas bases
padroniza_nome <- function(x) {
  tolower(trimws(x))
}

banco <- banco %>%
  mutate(nome_join = padroniza_nome(autor))

tse <- tse %>%
  mutate(nome_join = padroniza_nome(NM_URNA_CANDIDATO))

tse_info <- tse %>%
  select(
    nome_join,
    DS_COR_RACA,
    DT_NASCIMENTO,
    DS_GRAU_INSTRUCAO,
    DS_ESTADO_CIVIL,
    DS_OCUPACAO
  ) %>%
  distinct()

banco <- banco %>%
  left_join(tse_info, by = "nome_join") %>%
  rename(
    raca = DS_COR_RACA,
    nasc = DT_NASCIMENTO,
    escol = DS_GRAU_INSTRUCAO,
    es_civil = DS_ESTADO_CIVIL,
    ocup = DS_OCUPACAO
  ) %>%
  select(-nome_join)

write_xlsx(banco, "C:\\Users\\DRT904284\\Documents\\banco(3).xlsx")

#mesma coisa tudo de novo
banco <- read_excel("C:\\Users\\DRT904284\\Documents\\banco(2).xlsx")
religiao <- read_excel("C:\\Users\\DRT904284\\Downloads\\BANCO DE DADOS - 56º LEGISLATURA.xlsx")

banco <- banco %>%
  mutate(nome_join = padroniza_nome(autor))

religiao <- religiao %>%
  mutate(nome_join = padroniza_nome(NOME))

reg_info <- religiao %>%
  select(
    nome_join,
    `FRENTES RELIGIOSAS`
  ) %>%
  distinct()

reg_info <- religiao %>%
  select(
    nome_join,
    relig = `FRENTES RELIGIOSAS`
  ) %>%
  distinct()

banco <- banco %>%
  select(-any_of("relig")) %>%
  left_join(reg_info, by = "nome_join")

write_xlsx(banco, "C:\\Users\\DRT904284\\Documents\\banco(4).xlsx")

###### BASE RESTANTE (TCHELVIS) ######
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(stringr)

base <- read_excel("C:\\Users\\DRT904284\\Documents\\tchelvis.xlsx")

base <- base %>%
  mutate(
    # pega apenas o primeiro autor, antes da vírgula
    autor_partido_1 = str_split(autores_e_partidos, ",\\s*") %>% sapply(`[`, 1)
  ) %>%
  separate(
    autor_partido_1,
    into = c("autor", "partido_uf"),
    sep = " - ",
    extra = "merge",
    fill = "right"
  ) %>%
  separate(
    partido_uf,
    into = c("partido", "uf_autor"),
    sep = "/",
    extra = "merge",
    fill = "right"
  ) %>%
  select(-autores_e_partidos) %>%
  mutate(
    autor = trimws(autor),
    partido = trimws(partido),
    uf_autor = toupper(trimws(uf_autor)),
    regiao_autor = case_when(
      uf_autor %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      uf_autor %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf_autor %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      uf_autor %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      uf_autor %in% c("PR", "RS", "SC") ~ "Sul",)) %>%
  mutate(
    partido = tolower(trimws(partido)),  # padroniza
    espec = case_when(
      partido %in% c("solidariedade", "solidari", "cidadania", "avante", "pmn", "psd", "psdb", "mdb") ~ "centro-direita",
      
      partido %in% c("pros", "agir", "podemos", "pode", "prtb", "pmb", "ptb", "phs") ~ "direita",
      
      partido %in% c("novo", "progressistas", "pp", "patriota", "patri", "pr", "republicanos", "republic", "prb", "psl", "pl", "união", "uniao", "dc", "psc", "dem") ~ "extrema-direita",
      
      TRUE ~ NA_character_
    )
  )

write_xlsx(base, "C:\\Users\\DRT904284\\Documents\\tchelvis2.xlsx")

##################

base2 <- read_excel("C:\\Users\\DRT904284\\Documents\\tchelvis2.xlsx")
tse   <- read_excel("C:\\Users\\DRT904284\\Documents\\banco tse ic.xlsx")
religiao <- read_excel("C:\\Users\\DRT904284\\Downloads\\BANCO DE DADOS - 56º LEGISLATURA.xlsx")

padroniza_nome <- function(x) {
  tolower(trimws(x))
}

base2 <- base2 %>%
  mutate(nome_join = padroniza_nome(autor))

tse <- tse %>%
  mutate(nome_join = padroniza_nome(NM_URNA_CANDIDATO))

tse_info <- tse %>%
  select(
    nome_join,
    DS_COR_RACA,
    DT_NASCIMENTO,
    DS_GRAU_INSTRUCAO,
    DS_ESTADO_CIVIL,
    DS_OCUPACAO
  ) %>%
  distinct()

base2 <- base2 %>%
  left_join(tse_info, by = "nome_join") %>%
  rename(
    raca = DS_COR_RACA,
    nasc = DT_NASCIMENTO,
    escol = DS_GRAU_INSTRUCAO,
    es_civil = DS_ESTADO_CIVIL,
    ocup = DS_OCUPACAO
  ) %>%
  select(-nome_join)

base3 <- base2 %>%
  mutate(nome_join = padroniza_nome(autor))

religiao <- religiao %>%
  mutate(nome_join = padroniza_nome(NOME))

reg_info <- religiao %>%
  select(
    nome_join,
    `FRENTES RELIGIOSAS`
  ) %>%
  distinct()

reg_info <- religiao %>%
  select(
    nome_join,
    relig = `FRENTES RELIGIOSAS`
  ) %>%
  distinct()

base3 <- base3 %>%
  select(-any_of("relig")) %>%
  left_join(reg_info, by = "nome_join")

write_xlsx(base3, "C:\\Users\\DRT904284\\Documents\\tchelvis3.xlsx")

###### tratamento nome das variáveis ######
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(writexl)

base <- read_excel("C:\\Users\\DRT904284\\Documents\\banco CERTO.xlsx")

base_editada <- base %>%
  mutate(
    # trocar 4 por 3 na coluna par
    par = if_else(par == 4, 3, par),
    
    
    # recodificar tema
    tema = case_when(
      tema == "direitos reprodutivos" ~ "d. sexuais",
      tema == "direitos parentais" ~ "d. reprodutivos",
      tema == "direitos LGBT" ~ "d. LGBT",
      tema == "direitos trabalhistas" ~ "d. trabalhistas",
      tema == "direitos trans" ~ "d. trans",
      tema == "violência contra a mulher" ~ "v. contra mulher",
      tema == "violência contra vulnerável" ~ "v. contra vulnerável",
      TRUE ~ tema
    ),
    
    # calcular idade a partir do ano de nascimento
    nasc = 2026 - year(ymd_hms(as.character(nasc), tz = "UTC", quiet = TRUE)),
    
    # classificar partidos de esquerda na coluna espec
    espec = case_when(
      str_to_lower(partido) %in% c("pt", "psol", "pcdob", "pdt") ~ "esquerda",
      TRUE ~ espec
    )
  )

base_editada <- base_editada %>%
  mutate(
    par = if_else(espec == "esquerda", 1, 3)
  )

write_xlsx(base_editada, "C:\\Users\\DRT904284\\Documents\\base CERTA.xlsx")

###### variável age ######
library(readxl)
library(dplyr)
library(writexl)

# Ler planilha
banco <- read_excel("banco tse ic.xlsx")

# Ver como a coluna está sendo lida
class(banco$DT_NASCIMENTO)
head(banco$DT_NASCIMENTO)

# Criar coluna com o ano de nascimento
banco <- banco %>%
  mutate(
    DT_NASCIMENTO_2 = format(as.Date(DT_NASCIMENTO), "%Y")
  )

banco <- banco %>%
  mutate(
    idade = 2026 - as.numeric(DT_NASCIMENTO_2)
  )

# Ler os dois bancos
banco_idade <- read_excel("banco tse ic com idade.xlsx")
banco_atualizado <- read_excel("BANCO IC ATUALIZADO.xlsx")

# Criar uma tabela de correspondência: nome do candidato + idade
tabela_idade <- banco_idade %>%
  select(NM_URNA_CANDIDATO, idade) %>%
  mutate(
    NM_URNA_CANDIDATO = toupper(trimws(NM_URNA_CANDIDATO))
  ) %>%
  distinct(NM_URNA_CANDIDATO, .keep_all = TRUE)

# Adicionar idade ao BANCO IC ATUALIZADO com base no nome do autor
banco_atualizado_com_idade <- banco_atualizado %>%
  mutate(
    autor_padronizado = toupper(trimws(autor))
  ) %>%
  left_join(
    tabela_idade,
    by = c("autor_padronizado" = "NM_URNA_CANDIDATO")
  ) %>%
  select(-autor_padronizado)

# Salvar novo banco
write_xlsx(banco_atualizado_com_idade, "BANCO IC ATUALIZADO 2.xlsx")

###### religião de novo #####
# instalar pacotes se necessário
install.packages("readxl")
install.packages("writexl")

# carregar pacotes
library(readxl)
library(writexl)

# =========================
# importar os arquivos
# =========================

# arquivo X: possui NOME e FRENTES RELIGIOSAS
x <- read_excel("C:\\Users\\DRT904284\\Downloads\\BANCO DE DADOS - 56º LEGISLATURA.xlsx")

# arquivo Y: possui autor
y <- read_excel("C:\\Users\\DRT904284\\Documents\\BANCO IC ATUALIZADO 3.xlsx")

# =========================
# fazer o pareamento
# =========================

# encontrar a posição correspondente entre autor e NOME
posicao <- match(y$autor, x$NOME)

# criar nova coluna relig em Y
y$relig <- x$`FRENTES RELIGIOSAS`[posicao]

# =========================
# exportar resultado
# =========================

write_xlsx(y, "BANCO 4.xlsx")
