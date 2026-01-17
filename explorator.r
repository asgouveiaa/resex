pacman::p_load(tidyverse)

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
  labs(x = "Freq. Consumo", y = "N") + 
  scale_fill_manual(values = c("#fc8d59", "#91bfdb")); fig1.a


fig1.b <- resex_resultado$Caça %>% 
  filter(str_detect(caça_preferida, regex("CAITIT|PORCO|QUEIXA|PORCOMT",
                                          ignore_case = TRUE))) %>% 
  ggplot() + 
  geom_bar(aes(x = freqcons_caça, fill = caçador_familia)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Freq. Consumo", y = "N") +
  scale_fill_manual(values=c("#fc8d59","#91bfdb")); fig1.b

ggpubr::ggarrange(fig1.a,fig1.b, common.legend = T)

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

gt::gt(tabela1)




  
  
  
 
  








