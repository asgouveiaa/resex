pacman::p_load(tidyverse)

# georeferenciado as comunidades

cores <- c("#fc8d59", "#91bfdb")

#

resex_resultado$Casa$Comunidade %>% str_sort() %>% unique()



georef <- data.frame(Comunidade = c("Escrivão", "Pinhel", "Anã","Vila Franca",
                          "Nova Vista","Surucuá", "Aminã", "Maripá",
                          "Nova Canaã", "São Miguel", "Cametá"),
           lat = c(-3.4361758,-3.3309388,-2.4079601,-2.344959,
                   -3.1775558,-2.6825783,-2.5537497,-2.4936523,
                   -3.1553152,-2.4336755,-3.3018192
                   ),
           lon = c(-55.3564065,-55.3313271,-55.1373213,-55.0330627,
                   -55.2801173,-55.1769225,-55.2813647,-55.1380303,
                   -55.8574547,-55.187603,-55.3189155
                   ))

ggplot() +
  geom_sf(data=t) + 
  geom_point(data=georef,
             aes(lon,lat, color = Comunidade))




  # comunidades com povos indígenas

comunidades_indigenas <- c("Escrivão", "Pinhel", "Jatequara", "Jauarituba", "Santo Amaro",
       "Vista Alegre", "Muratuba", "Cabeceira do Amorim", "Vila do Amorim",
       "Novo Progresso", "Aminã", "Nova Vista")

resex_resultado$Caça <- resex_resultado$Caça %>% 
  mutate(indig = case_when(
    Comunidade %in% comunidades_indigenas ~ "Sim",
    TRUE ~ "Não"),
    caçador_familia = ifelse(caçador_familia == "SIM","Sim","Não"),
    caça_consumida_n = str_count(caça_preferida, ";") + 1,
    caça_problema_n = str_count(caça_relatada_problema, ";") + 1,
    caça_problema_n = ifelse(problemas_encontrados_caça == "Nenhum",0,caça_problema_n),
    consome_caça = ifelse(freqcons_caça == "NUNCA", "Não", "Sim"),
    n_nao_caca = str_split(carne_musada, ";") %>%
      map_int(~ sum(str_trim(.x) != "CAÇA"))
  )

# criando variaveis

resex_resultado$Atividade %>% 
  mutate(
    roça = case_when(
      str_detect(profi, "ROÇA") == T ~ "Sim",
      str_detect(profi, "ROÇA") == F ~ "Não"),
    coletor = case_when(
      !is.na(colet) ~ "Sim",
      is.na(colet) ~ "Sim"),
    animal_consumo = str_split(criac_animais, "; ") %>%
      map_chr(~ {
        filtered = .x[!.x %in% c("CACHOR", "GATO")]
        if(length(filtered) == 0) NA else paste(filtered, collapse = "; ")
      }),
    animal_consumo_n = str_count(animal_consumo, ";") + 1,
    consome_animal = ifelse(!is.na(animal_consumo), "Sim", "Não"),
    animal_domestico = case_when(
      str_detect(criac_animais, "CACHOR|GAT|GATO|CACHORRO") ~ "Sim",
      !str_detect(criac_animais, "CACHOR|GAT|GATO|CACHORRO") ~ "Não"),
    saf = ifelse(!is.na(plant_saf), "Sim", "Não"),
    saf_n = str_count(plant_saf, ";") + 1
  )
  

# estimando a prob de caça diária

resex_resultado$Caça <- resex_resultado$Caça %>% 
  mutate(
    prob_caça_diaria = case_when(
      freqcons_caça %in% c("DIÁRIO", "DIÁRIO/SEMANAL") ~ 365/365,
      freqcons_caça == "1x MÊS" ~ 12/365,
      freqcons_caça == "2x MÊS" ~ 24/365,
      freqcons_caça == "SEMANAL" ~ 52/365,
      freqcons_caça == "2-3x SEMANA" ~ 130/365,
      freqcons_caça == "2x ANO" ~ 2/365,
      freqcons_caça == "4x ANO" ~ 4/365,
      freqcons_caça == "ÀS VEZES" ~ 4/365,
      freqcons_caça == "RARO" ~ 3/365,
      freqcons_caça == "NUNCA" ~ 0,
      TRUE ~ NA)
  )

resex_resultado$Caça %>% 
  select(freqcons_caça,prob_caça_diaria) %>% 
  distinct() %>% 
  arrange()

resex_resultado$Caça %>% 
  #filter(str_detect(caça_preferida, regex("CAITIT|PORCO|QUEIXA|PORCOMT",
  #                                        ignore_case = TRUE))) %>% 
  group_by(caçador_familia) %>% 
  summarise(n = n()) 

fig1.a <- resex_resultado$Caça %>% 
  filter(str_detect(caça_preferida, regex("PACA", ignore_case = TRUE))) %>% 
  ggplot() + 
  geom_bar(aes(x = freqcons_caça, fill = caçador_familia)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Freq. Consumo", y = "N", title = "C. Paca") + 
  scale_fill_manual(values = cores); fig1.a


fig1.b <- resex_resultado$Caça %>% 
  filter(str_detect(caça_preferida, regex("CAITIT|PORCO|QUEIXA|PORCOMT",
                                          ignore_case = TRUE))) %>% 
  ggplot() + 
  geom_bar(aes(x = freqcons_caça, fill = caçador_familia)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Freq. Consumo", y = "N", title = "P. tajacu") +
  scale_fill_manual(values=cores); fig1.b

fig1 <- ggpubr::ggarrange(fig1.a,fig1.b, common.legend = T); fig1

# tabela descritiva

## comunidade, # animal mais consumido, freq consumo, pct caçador_familia,
## como consome, quem limpa, problema_carne_caça

caça_mais_consumida <- resex_resultado$Caça %>%
  filter(Comunidade %in% names(which(table(Comunidade) > 1))) %>%
  separate_rows(caça_preferida, sep = "; ") %>%
  mutate(caça_preferida = trimws(caça_preferida)) %>%
  filter(caça_preferida != "") %>%
  group_by(Comunidade, caça_preferida) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(Comunidade) %>%
  filter(freq == max(freq)) %>%
  summarise(
    caça_mais_consumida = paste(sort(caça_preferida), collapse = "; "),
    frequência = first(freq)
  )


caça_mais_consumida <- caça_mais_consumida %>% 
  mutate(caça_mais_consumida = paste0(caça_mais_consumida," (",frequência,")")) %>% 
  select(-frequência)

freq_consumo <- resex_resultado$Caça %>%
  filter(Comunidade %in% names(which(table(Comunidade) > 1))) %>%
  filter(freqcons_caça != "") %>%
  group_by(Comunidade, freqcons_caça) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(Comunidade) %>%
  filter(freq == max(freq)) %>%
  summarise(
    freq_consumo = paste(sort(freqcons_caça), collapse = "; "),
    frequência = first(freq)
  )

freq_consumo <- freq_consumo %>% 
  mutate(freq_consumo = paste0(freq_consumo," (",frequência,")")) %>% 
  select(-frequência)

pct_caçadores <- resex_resultado$Caça %>% 
  filter(Comunidade %in% names(which(table(Comunidade) > 1))) %>%
  group_by(Comunidade) %>% 
  summarise(
    total = n(),
    n_sim = sum(caçador_familia == "SIM", na.rm = TRUE),
    n_nao = sum(caçador_familia == "NÃO", na.rm = TRUE),
    pct_sim = n_sim / total * 100,
  ) %>% 
  select(Comunidade,pct_sim, total)

pct_caçadores <- pct_caçadores %>% 
  mutate(pct_sim = paste0(round(pct_sim,1),"%"," (",total,")")) %>% 
  select(-total)
  
como_consome <- resex_resultado$Caça %>%
  filter(Comunidade %in% names(which(table(Comunidade) > 1))) %>%
  filter(como_consome_caça != "") %>%
  group_by(Comunidade, como_consome_caça) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(Comunidade) %>%
  filter(freq == max(freq)) 

como_consome <- como_consome %>% 
  mutate(como_consome_caça = paste0(como_consome_caça," (",freq,")")) %>% 
  select(-freq)

quem_limpa <- resex_resultado$Caça %>%
  filter(Comunidade %in% names(which(table(Comunidade) > 1))) %>%
  filter(quem_limpa_caça != "") %>%
  group_by(Comunidade, quem_limpa_caça) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(Comunidade) %>%
  filter(freq == max(freq)) 

quem_limpa <- quem_limpa %>% 
  mutate(quem_limpa_caça = paste0(quem_limpa_caça," (",freq,")")) %>% 
  select(-freq)

problemas_carne <- resex_resultado$Caça %>% 
  filter(Comunidade %in% names(which(table(Comunidade) > 1))) %>%
  filter(problemas_encontrados_caça != "") %>%
  group_by(Comunidade, problemas_encontrados_caça) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(Comunidade) %>%
  filter(freq == max(freq)) %>%
  summarise(
    problemas_encontrados_caça = paste(sort(problemas_encontrados_caça), collapse = "; "),
    freq = first(freq)
  )

problemas_carne <- problemas_carne %>% 
  mutate(problemas_encontrados_caça = paste0(problemas_encontrados_caça," (",freq,")")) %>% 
  select(-freq)


tabela1 <- reduce(
  list(caça_mais_consumida, freq_consumo, pct_caçadores,
       como_consome, quem_limpa, problemas_carne),
  left_join,
  by = "Comunidade"
)

tabela1 <- tabela1 %>%
  mutate(across(everything(),~ str_to_title(.)))

tab1 <- gt::gt(tabela1); tab1

# Atividades

resex_resultado$Atividade <- resex_resultado$Atividade %>% 
  mutate(
  indig = case_when(
    Comunidade %in% comunidades_indigenas ~ "Sim",
    TRUE ~ "Não"),
  roça = case_when(
    str_detect(profi, "ROÇA") == T ~ "Sim",
    str_detect(profi, "ROÇA") == F ~ "Não"),
  coletor = case_when(
    !is.na(colet) ~ "Sim",
    is.na(colet) ~ "Sim"),
  animal_consumo = str_split(criac_animais, "; ") %>%
    map_chr(~ {
      filtered = .x[!.x %in% c("CACHOR", "GATO")]
      if(length(filtered) == 0) NA else paste(filtered, collapse = "; ")
    }),
  animal_consumo_n = str_count(animal_consumo, ";") + 1,
  consome_animal = ifelse(!is.na(animal_consumo), "Sim", "Não"),
  animal_domestico = case_when(
    str_detect(criac_animais, "CACHOR|GAT|GATO|CACHORRO") ~ "Sim",
    !str_detect(criac_animais, "CACHOR|GAT|GATO|CACHORRO") ~ "Não"),
  saf = ifelse(!is.na(plant_saf), "Sim", "Não"),
  saf_n = str_count(plant_saf, ";") + 1
  )

##  

ativ <- resex_resultado$Atividade %>% 
  select(Amostra,Comunidade, indig,
         roça,coletor,
         animal_consumo,
         animal_consumo_n,
         animal_domestico,saf,
         saf_n)

caç <- resex_resultado$Caça %>% 
  select(Amostra,Comunidade, indig,
         consome_caça,caça_preferida,freqcons_caça,caçador_familia,
         como_consome_caça,quem_limpa_caça,problema_carne_caça, n_nao_caca,
         problemas_encontrados_caça,caça_consumida_n,caça_problema_n)

df1 <- ativ %>% left_join(caç, by = c("Amostra","Comunidade","indig"))
  
# roça está relacionadas ao consumo de caça?

df1 %>% 
  ggplot() + 
  geom_bar(aes(x=saf, fill = consome_caça), position = "dodge") +
  scale_fill_manual(values=cores)

table(df1$saf, df1$consome_caça)

fisher.test(df1$consome_caça, df1$saf)

# saf está relacionadas ao consumo de caça?

df1 %>% 
  ggplot() + 
  geom_bar(aes(x=consome_caça, fill = roça), position = "dodge")+
  scale_fill_manual(values=cores)+
  theme_minimal()

table(df1$roça, df1$consome_caça)

fisher.test(df1$consome_caça,df1$roça)

# indig está relacionadas ao consumo de caça?

table(df1$indig, df1$consome_caça)
fisher.test(df1$indig,df1$consome_caça)

df1 %>% 
  ggplot() + 
  geom_bar(aes(x=consome_caça, fill = indig), position = "dodge") + 
  scale_fill_manual(values=cores)+
  theme_minimal()

# o número de animais consumidos está relacionado à saf?

df1 %>% 
  ggplot() + 
  geom_bar(aes(x=caça_consumida_n, fill = saf)) +
  scale_fill_manual(values=cores)+
  theme_minimal()
  facet_wrap(~indig)

# o número de diferentes culturas (saf) está relacionado ao consumo de caça?
  
df1 %>% 
  ggplot() + 
  geom_bar(aes(x=saf_n, fill = consome_caça)) +
  theme_minimal()+
  scale_fill_manual(values=cores)+
  facet_wrap(~indig)

# o número de diferentes culturas (roça) está relacionado ao consumo de caça?

df1 %>% 
  ggplot() + 
  geom_bar(aes(x=caça_consumida_n, fill = roça)) +
  theme_minimal() +
  scale_fill_manual(values=cores)+
  facet_wrap(~indig)

# quais comunidades caçam mais?

df1 %>%
  group_by(Comunidade) %>%
  summarise(
    consome_caça = sum(!is.na(consome_caça), na.rm = TRUE),
    caçadores = sum(caçador_familia == "Sim", na.rm = TRUE),
    total = n()  
  ) %>%  
  mutate(caçada_familia = (caçadores/consome_caça)*100) %>% 
  arrange(desc(caçada_familia)) %>% 
  filter(total > 1) %>% 
  slice_head(n = 10)

# alguma comunidade está mais relacionada à presença de problemas na caça?

df1 %>% 
  group_by(Comunidade) %>% 
  summarise(n_problemas = n_distinct(problemas_encontrados_caça),
            n_total = n_distinct(Amostra)) %>% 
  mutate(pct_problemas = (n_problemas/n_total)*100) %>% 
  filter(n_problemas > 1) %>% 
  arrange(desc(pct_problemas)) %>% 
  slice_head(n = 10)

## Casa

cas <- resex_resultado$Casa %>% 
  mutate(
    tipo_casa2 = case_when(
      str_detect(tipo_casa, "TIJO") ~ "alvenária",
      str_detect(tipo_casa, "BARR") ~ "barro",
      str_detect(tipo_casa, "MAD") ~ "madeira",
    ),
    # Contar o número de fontes (separadas por "; ")
    n_fontes_agua = ifelse(
      is.na(origem_agua),
      0,
      str_count(origem_agua, "; ") + 1
    ),
    origem_agua2 = case_when(
      n_fontes_agua == 1 ~ tolower(origem_agua),
      n_fontes_agua == 2 ~ "2 fontes",
      n_fontes_agua == 3 ~ "3 fontes",
      n_fontes_agua == 4 ~ "4 fontes",
      n_fontes_agua == 5 ~ "5 fontes"
    )
  ) %>% 
  select(Amostra,Comunidade,tipo_casa2,origem_agua2)


## Etario

etar <- resex_resultado$Etario %>% 
  mutate(tamanh_familia = criança+adolesc+adulto+idosos)

## Habitos 

habit <- resex_resultado$Habitos %>% 
  mutate(
    filtr_agua2 = ifelse(filtr_agua == "FILSIM", "Sim", "Não"),
    ferve_agua2 = case_when(
      ferve_agua == "FERVRAR"  ~ "raramente",
      ferve_agua == "FERVDVQ" ~ "raramente",
      ferve_agua == "FERVSEMP" ~ "sempre",
      ferve_agua == "FERVSIM" ~ "sim",
      ferve_agua == "FERVNÃO" & ferve_agua == "FERVNAP" ~ "não",
      TRUE ~ "não"
    )) %>% 
  select(Amostra,Comunidade,filtr_agua2,ferve_agua2)


df2 <- ativ %>% left_join(caç, by = c("Amostra","Comunidade","indig")) %>% 
  left_join(etar, by = c("Amostra","Comunidade")) %>% 
  left_join(cas, by = c("Amostra","Comunidade")) %>% 
  left_join(habit, by = c("Amostra","Comunidade")) %>% 
  na.omit()

# o que está relacionado a frequência de caça?

# Exemplo: frequente vs não frequente
df2$freq_bin <- ifelse(df2$freqcons_caça %in% c("DIÁRIO", "2-3x SEMANA", "SEMANAL"), 
                       "Frequente", "Não_Frequente")

# Converter para fator (referência = Não_Frequente)
df2$freq_bin <- factor(df2$freq_bin, levels = c("Não_Frequente", "Frequente"))

mod_freq <- glm(freq_bin ~ saf_n  + tamanh_familia + caçador_familia,
                data = df2, family = binomial())

# considerar efeito aleatório por comunidade

require(lme4)

mod_freq2 <- glmer(freq_bin ~ saf_n + tamanh_familia + caçador_familia +
                     (1 | Comunidade), data = df2, family = binomial())


mod_freq3 <- glmer(freq_bin ~ saf_n + tamanh_familia + caçador_familia +
                     factor(tipo_casa2) + factor(animal_domestico) + caça_consumida_n +
                     (1 | Comunidade), data = df2, family = binomial())

AIC(mod_freq,mod_freq2, mod_freq3)

exp(mod_freq2@beta)

# H1: Famílias com mais animais domésticos/SAF consomem menos caça
mod1 <- glmer(freq_bin ~ caçador_familia + animal_consumo_n + saf_n + n_nao_caca +
                (1 | Comunidade),
              data = df2, family = binomial())

# H2: Famílias com crianças/adolescentes consomem mais caça
df2$total_crian_adolesc <- df2$criança + df2$adolesc
mod2 <- glmer(freq_bin ~ caçador_familia + total_crian_adolesc + (1 | Comunidade),
              data = df2, family = binomial())

# H3: Composição familiar (proporção de adultos)
df2$prop_adultos <- df2$adulto / df2$tamanh_familia
mod3 <- glmer(freq_bin ~ caçador_familia + prop_adultos + (1 | Comunidade),
              data = df2, family = binomial())

# H4: Famílias que encontram problemas na carne caçam menos
df2$tem_problema <- ifelse(df2$caça_problema_n > 0, "Sim", "Não")
mod4 <- glmer(freq_bin ~ caçador_familia + tem_problema + (1 | Comunidade),
              data = df2, family = binomial())

# H5: Número de espécies caçadas (diversidade)
mod5 <- glmer(freq_bin ~ caçador_familia + caça_consumida_n + (1 | Comunidade),
              data = df2, family = binomial())

# H6: Tipo de casa e condições de vida
mod6 <- glmer(freq_bin ~ caçador_familia + tipo_casa2 + (1 | Comunidade),
              data = df2, family = binomial())

# H7: Interação caçador * tamanho família (caçador é mais importante em famílias grandes?)
mod7 <- glmer(freq_bin ~ caçador_familia * tamanh_familia + (1 | Comunidade),
              data = df2, family = binomial())

# H8: Interação caçador * animais SAF (substituição de proteína)
mod8 <- glmer(freq_bin ~ caçador_familia * saf_n + (1 | Comunidade),
               data = df2, family = binomial())

mod_interacao <- glmer(freq_bin ~ caçador_familia * animal_consumo_n + 
                         caçador_familia * saf_n + (1 | Comunidade),
                       data = df2, family = binomial())

# Teste todas as hipóteses e compare AIC
modelos <- list(
  mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4,
  mod5 = mod5, mod6 = mod6, mod7 = mod7, mod8 = mod8,
  mod_interacao = mod_interacao
)

# Tabela comparativa
comparacao <- data.frame(
  Modelo = names(modelos),
  AIC = sapply(modelos, AIC),
  BIC = sapply(modelos, BIC)
)

comparacao[order(comparacao$AIC), ]

# Resumos dos top modelos
summary(mod4)  # Problemas na carne
summary(mod3)  # Proporção de adultos
summary(mod6)  # Tipo de casa

# Odds Ratios com IC 95%
library(broom.mixed)
tidy(mod4, conf.int = TRUE, exponentiate = TRUE)
tidy(mod3, conf.int = TRUE, exponentiate = TRUE)
tidy(mod6, conf.int = TRUE, exponentiate = TRUE)


# Juntar variáveis dos melhores modelos
mod_combined <- glmer(freq_bin ~ caçador_familia + tem_problema + 
                        prop_adultos + tipo_casa2 + (1 | Comunidade),
                      data = df2, family = binomial())

AIC(mod4, mod_combined)  # Ver se melhorou

# Modelo parcimonioso (stepwise backward)
summary(mod_combined)

# Vamos olhar detalhadamente o mod4
summary(mod4)

# Odds Ratios
exp(fixef(mod4))
exp(confint(mod4, method = "Wald"))

# Modelo final: mod4 (simples e com melhor AIC)
mod_final <- mod4

library(ggplot2)

# 1. Gráfico das probabilidades previstas
pred_data <- ggpredict(mod_final, terms = c("caçador_familia", "tem_problema"))

ggplot(as.data.frame(pred_data), 
       aes(x = x, y = predicted, color = group, group = group)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.3)) +
  labs(title = "Probabilidade de consumo frequente de caça",
       x = "Caçador na família",
       y = "Probabilidade prevista",
       color = "Problemas na carne") +
  theme_minimal()

# 2. Gráfico dos efeitos aleatórios (comunidades)
ranef_plot <- ranef(mod_final, condVar = TRUE)
dotplot(ranef_plot)


library(pROC)
pred_prob <- predict(mod_final, type = "response")
roc_curve <- roc(as.numeric(df2$freq_bin == "Frequente") ~ pred_prob)
plot(roc_curve)
auc(roc_curve)

# Hipótese: Caçador → mais caça consumida → mais problemas identificados → menor consumo futuro?
library(mediation)

# Primeiro ajuste os modelos
model_m <- glmer(caça_consumida_n ~ caçador_familia + (1|Comunidade),
                 data = df2, family = poisson())

model_y <- glmer(freq_bin ~ caçador_familia + caça_consumida_n + 
                   caça_problema_n + (1|Comunidade),
                 data = df2, family = binomial())

# Teste de mediação
med <- mediate(model_m, model_y, treat = "caçador_familia", 
               mediator = "caça_consumida_n", sims = 1000)
summary(med)

plot(med)


