# recoding

resex_resultado$Caça <- resex_resultado$Caça %>%
  mutate(freqcons_caça = case_when(
    tolower(trimws(freqcons_caça)) %in% c("tododi", "todos os dias", "diário") ~ "DIÁRIO",
    tolower(trimws(freqcons_caça)) %in% c("2xmes", "2x_mes") ~ "2x MÊS",
    tolower(trimws(freqcons_caça)) %in% c("1xmes", "1x_mes") ~ "1x MÊS",
    tolower(trimws(freqcons_caça)) %in% c("2a3xsm", "2-3x semana", "2xmes; 2a3xsm") ~ "2-3x SEMANA",
    tolower(trimws(freqcons_caça)) %in% c("1xsem", "1x_sem") ~ "SEMANAL",
    tolower(trimws(freqcons_caça)) %in% c(">3xmês", "3x_mes", "frequentemente") ~ "SEMANAL",
    tolower(trimws(freqcons_caça)) %in% c("nunca", "não consome") ~ "NUNCA",
    tolower(trimws(freqcons_caça)) %in% c("raro", "raramente") ~ "RARO",
    tolower(trimws(freqcons_caça)) %in% c("asveze", "às vezes", "ocasionalmente") ~ "ÀS VEZES",
    tolower(trimws(freqcons_caça)) %in% c("4x_ano", "4x ano") ~ "4x ANO",
    tolower(trimws(freqcons_caça)) %in% c("2x_ano", "2x ano") ~ "2x ANO",
    tolower(trimws(freqcons_caça)) %in% c("_cada6mes", "a cada 6 meses") ~ "2x ANO",
    tolower(trimws(freqcons_caça)) %in% c(">3xmês; 2a3xsm") ~ "SEMANAL",
    tolower(trimws(freqcons_caça)) %in% c("tododi; 2a3xsm") ~ "DIÁRIO",
    is.na(freqcons_caça) | trimws(freqcons_caça) == "" ~ NA_character_,
    TRUE ~ freqcons_caça  # Mantém outros valores
  ))

resex_resultado$Caça <- resex_resultado$Caça %>%
  mutate(
    problemas_encontrados_caça = case_when(
      str_detect(problemas_encontrados_caça, "VERMES|PARASIT") ~ "Parasitas",
      str_detect(problemas_encontrados_caça, "TUMOR|CARO_FIG") ~ "Tumores/Caroços",
      str_detect(problemas_encontrados_caça, "FERIDA") ~ "Feridas",
      str_detect(problemas_encontrados_caça, "MAGRA|SINT_DO|PERD_PEL|MAGRA|Alt_Carn|Alt_Carn") ~ "Doença sistêmica",
      str_detect(problemas_encontrados_caça, "CHUMBO|URA") ~ "Resquício Bala",
      str_detect(problemas_encontrados_caça, "NÃO_DET") ~ "Nenhum",
      problema_carne_caça == "NÃO" ~ "Nenhum",
      is.na(problemas_encontrados_caça) ~ NA_character_,
      TRUE ~ "Outros"
    )
  )

resex_resultado$Caça <- resex_resultado$Caça %>%
  mutate(freqcons_caça = factor(freqcons_caça,
                                levels = c("DIÁRIO","2-3x SEMANA","SEMANAL",
                                           "1x MÊS", "2x MÊS", "2x ANO",
                                           "ÀS VEZES", "RARO", "NUNCA"))
  )

resex_resultado$Caça <- resex_resultado$Caça %>% 
  mutate(caçador_familia = case_when(
    is.na(caçador_familia) ~ "NÃO",
    TRUE ~ caçador_familia
  )) 


resex_resultado$Caça <- resex_resultado$Caça %>%
  mutate(
    quem_limpa_caça = case_when(
      str_detect(quem_limpa_caça, "PROPRI") ~ "Caçador",
      str_detect(quem_limpa_caça, "ESPOS") | 
        str_detect(quem_limpa_caça, "MARID") ~ "Cônjuge",
      str_detect(quem_limpa_caça, "FILH[AO]") | 
        str_detect(quem_limpa_caça, "NETO") ~ "Filhos/Netos",
      str_detect(quem_limpa_caça, "MAE") | 
        str_detect(quem_limpa_caça, "PAI") | 
        str_detect(quem_limpa_caça, "AVO") ~ "Pais/Avós",
      str_detect(quem_limpa_caça, "IRMA") | 
        str_detect(quem_limpa_caça, "SOBRIN") | 
        str_detect(quem_limpa_caça, "TIOS") ~ "Outros parentes",
      str_detect(quem_limpa_caça, "TODOS") ~ "Toda a família",
      quem_limpa_caça == "CAÇAD" ~ "Caçador",
      quem_limpa_caça == "FAMIL" ~ "Família",
      quem_limpa_caça == "OUTRO" ~ "Família",
      is.na(quem_limpa_caça) ~ NA_character_,
      TRUE ~ "Outros"
    )
  )

resex_resultado$Caça <- resex_resultado$Caça %>%
  mutate(
    como_consome_caça = case_when(
      como_consome_caça == "DEPEND" ~ "PEDAÇO",
      como_consome_caça == "INTEIR" ~ "INTEIR",
      is.na(como_consome_caça) ~ NA_character_,
      TRUE ~ "INTEIRO/PEDAÇO"
    )
  )

resex_resultado$Caça <- resex_resultado$Caça %>%
  mutate(
    problema_carne_caça = case_when(
      problema_carne_caça == "SIM; RARO" ~ "RARO",
      problema_carne_caça == "DVZQD" ~ "OCASIONALMENTE",
      is.na(problema_carne_caça) ~ NA_character_,
      TRUE ~ problema_carne_caça
    )
  )

