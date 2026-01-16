pacman::p_load(
  tidyverse,readxl
)

file_path <- "./PLANILHA_MATRIZES_ANALISE_RISCO_RESEX.xlsx"
all_sheets <- excel_sheets("./PLANILHA_MATRIZES_ANALISE_RISCO_RESEX.xlsx")

list_of_tables <- map(all_sheets, ~ read_excel(file_path, sheet = .x))
names(list_of_tables) <- all_sheets # Assign sheet names to list elements

list_of_tables$Caça <- list_of_tables$Caça %>% 
  rename(OUTROS = CPOUTROS,
         QUEIMA = CPQUEIMA)

list_of_tables$Etário <- list_of_tables$Etário %>% 
  left_join(list_of_tables$Comunidade)

list_of_tables$Casa <- list_of_tables$Casa %>% 
  left_join(list_of_tables$Comunidade)

list_of_tables$Hábitos <- list_of_tables$Hábitos %>% 
  left_join(list_of_tables$Comunidade)

list_of_tables$Atividades <- list_of_tables$Atividades %>% 
  left_join(list_of_tables$Comunidade)

list_of_tables$Saúde <- list_of_tables$Saúde %>% 
  left_join(list_of_tables$Comunidade)

list_of_tables$Caça <- list_of_tables$Caça %>% 
  left_join(list_of_tables$Comunidade)

list_of_tables$Animais <- list_of_tables$Animais %>% 
  left_join(list_of_tables$Comunidade)

##

resex <- list_of_tables[-1] %>%
  reduce(full_join, by = "Amostra")

resex$Comunidade %>% n_distinct() 
resex$Comunidade %>% unique()


resex$Amostra %>% n_distinct()  
resex$Amostra %>% unique()

resex %>% group_by(Comunidade) %>% 
  summarize(n_amostras = length(Amostra)) %>% View()


names(list_of_tables$Atividades)
names(list_of_tables$Saúde)
names(list_of_tables$Caça)
names(list_of_tables$Animais)

#CF sim = caçado por alguém família
#LP = quem limpa
#PC = problemas na carne
#chum = chumbo da bala
#QA = quem come a carne infectada descartada

#cl = chegam longe
#cldo = chegam de longe doentes
#OA = outros animais que passam doença segundo os entrevistados

# Hábitos


# Etário

list_of_tables$Etário <- list_of_tables$Etário %>% 
  rename(criança = CSCRIANC,
         adolesc = CSADOLEN,
         adulto = CSADULTO,
         idosos = CSIDOSOS)


# Casa

#2. Sua casa é de quê?
tipo_casa <- c("BARRO", "TIJOLO", "MADEIRA","PALHA","OUTRO")

#3. Água que sua família bebe é do:
origem_agua <- c("MICRO","NASCENTE","POÇO","RIO","OUTRA")

resultado_casa <- list_of_tables$Casa %>%
  select(Amostra, Comunidade) %>%
  mutate(
    tipo_casa = consolidar_grupo(list_of_tables$Casa, tipo_casa, ""),
    origem_agua = consolidar_grupo(list_of_tables$Casa, origem_agua, ""),
  )

# Hábitos

#4. Você usa filtro de água em casa?
filtr_agua <- c("FILSIM","FILNÃO")

#5. Ferve a mamadeira e chupeta das crianças?
ferve_agua <- c("FERVSIM","FERVSEMP","FERVDVQ","FERVRAR","FERVNÃO","FERVNAP")


resultado_habitos <- list_of_tables$Hábitos %>%
  select(Amostra, Comunidade) %>%
  mutate(
    filtr_agua = consolidar_grupo(list_of_tables$Hábitos, filtr_agua, ""),
    ferve_agua = consolidar_grupo(list_of_tables$Hábitos, ferve_agua, ""),
  )

# Atividades

#6. Quais atividades de sua família?
profi <- c("AGSAUDE","ARTESANAT","COMERC","EXTRAT","PESCA","PROFESSOR",
            "ROÇA","TECNPROJ","ATVAPOSE","ATVCRIAC","ATVSAF","ATVTURIS",
            "ATVOUTRO")

#6.1 O que vocês plantam? (roça)
plant_roça <- c("ABACAT","ABACAX","ABANO","ABOBOR","ABOBRINH","AÇAI","ACEROLA",
           "AIMARU","ALFACE") 

# 6.2 O que vocês tiram? 
colet <- c("ALFAVAC","AMBE","ANDIROB","ANGELIM","ARAÇA")

# 6.3 O que vcs plantam? (SAF)
plant_saf <- c("ARROZ","BACABA","BACURI","BAMBU","BANANA","BATATA","BATDOCE",
               "BEIJU","BORRACH","BREU","BURITI...39","CAÇA","CACAU","CAFÉ",
               "CANA","CARÁ","CASTNH","CASTNHPAR","CASTNHCAJ","CEBOLA","CEBOLIN",
               "CEDRO","CHEIRVERD","CHICOR","CIPÓ","COCO","COENTR","COPAIB",
               "CUMARU","CUPUAÇU","ESPNATIV","FARINHA","FEIJAO","FRUTA","GENGIBR",
               "GERIMUM","GOIABA","HORTA","INGA","IPE","IXI","ITAUBA","JACARAND",
               "LARANJ","BURITI...74","LATEX","LEGUMES","LEITE","LEITEAMAP",
               "LEITSERI","LEITESUCUB","LEULA","LIMAO","MACAXER","MADEIRA",
               "MAMAO","MANACA","MANDIOC","MANGA","MANGARA","MANICUI",
               "MANIVA","MARACUJ","MAXIXE","MEL","MELANC","MELIPONI",
               "MILHO","MOGNO","MURICI","OLEO","OLEOAND","OLEOCOC","OLEOCOP",
               "OLEOPIQ","OLEOSIP","PAJURA","PALHA","PALHABUR","PALHACIP",
               "PATAMA","PATAUA","PEROBA","PIMENT","PIMENCHR","PIQUIA","PINHA",
               "PITANGA","PUPUNHA","QUIABO","SEMENT","SUCURUB","SUCUBA","TALA",
               "TANGERIN","TEMPEROS","TIPITI","TIRIRIC","TOMAT","TUCUMA",
               "URUCUM","UXI","VERDUR"
               )

criac_animais <- c("ACBOI","ACCABRA","ACCACHOR","ACCAITIT","ACCARNEI",
                   "ACCAVALO","ACCUTIA","ACGALIN","ACGATO","ACJABUTI",
                   "ACMACACO","ACMARREC","ACPACA","ACPSITAC","ACPASSAR",
                   "ACPATO","ACPEIXE","ACPORCO","ACTATU","ACVEADO")

criac_tipo <- c("ACSOLTO","ACPRESO")

criac_motivo <- c("ACCOMER","ACVENDER","ACOUTRA")

animal_vac <- names(list_of_tables$Atividades)[startsWith(names(list_of_tables$Atividades), "AV")]

quem_vac <- c("QVABARE", "QVPROPRI", "QVPREFEI", "QVPROJET", "QVBOUBA",
              "QVCENVAL", "QVFAMILI", "QVOUTROS")

vac_para <- c("QVSAUMEG","QVAFTOSA","QVBOUBA","QVBRUCEL","QVVERDEX","QVOUTRAS",
              "QVVERME","VC_CALAZ","VC_VITAMIN")

nao_identificadas <- c("VQD2013","VQD2012","VQD2010","VQD2015","VQD2014","VQDNOLEMB",
                       "UV_2016", "JABECTO","MACACINT","PASSARIND")


caes_vac_para <- names(list_of_tables$Atividades)[startsWith(names(list_of_tables$Atividades), "CAO")]

galinhas_vac_para <- names(list_of_tables$Atividades)[startsWith(names(list_of_tables$Atividades), "GAL")]

boi_vac_para <- names(list_of_tables$Atividades)[startsWith(names(list_of_tables$Atividades), "BOI")]

gato_vac_para <- names(list_of_tables$Atividades)[startsWith(names(list_of_tables$Atividades), "GAT")]

pato_vac_para <- c("PATVIRUS","PATPARAS","PATBACT","PATNUTRI")

porco_vac_para <- names(list_of_tables$Atividades)[startsWith(names(list_of_tables$Atividades), "PORC")]

resultado_atividade <- list_of_tables$Atividade %>%
  select(Amostra, Comunidade) %>%
  mutate(
    profi = consolidar_grupo(list_of_tables$Atividade, profi, ""),
    plant_roça = consolidar_grupo(list_of_tables$Atividade, plant_roça, ""),
    colet = consolidar_grupo(list_of_tables$Atividade, colet, ""),
    plant_saf = consolidar_grupo(list_of_tables$Atividade, plant_saf, ""),
    criac_animais = consolidar_grupo(list_of_tables$Atividades, criac_animais, "AC"),
    criac_tipo = consolidar_grupo(list_of_tables$Atividades, criac_tipo, "AC"),
    criac_motivo = consolidar_grupo(list_of_tables$Atividades, criac_motivo, "AC"),
    animal_vac = consolidar_grupo(list_of_tables$Atividades, animal_vac, "AV"),
    quem_vac = consolidar_grupo(list_of_tables$Atividades, quem_vac, "QV"),
    vac_para = consolidar_grupo(list_of_tables$Atividades, vac_para, c("QV","VC")),
  )


# Saúde

problemas_saude_homem <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "H")]

problemas_saude_mulher <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "M")]

problemas_saude_idoso <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "I")]

problemas_saude_crianc <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "C")]

resultado_saude <- list_of_tables$Saúde %>%
  select(Amostra, Comunidade) %>%
  mutate(
    problemas_saude_homem = consolidar_grupo(list_of_tables$Saúde, problemas_saude_homem, "H"),
    problemas_saude_mulher = consolidar_grupo(list_of_tables$Saúde, problemas_saude_mulher, "M"),
    problemas_saude_idoso = consolidar_grupo(list_of_tables$Saúde, problemas_saude_idoso, "I"),
    problemas_saude_crianc = consolidar_grupo(list_of_tables$Saúde, problemas_saude_crianc, "C"),
  )


# CAÇA

carne_musada <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "MU")] 
#carne_musada <- gsub("^MU", "", carne_musada)

caça_preferida <-  names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "CP")]
#caça_preferida <- gsub("^CP", "", caça_preferida)

freqcons_caça <-  names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "FC")]
#freqcons_caça <- gsub("^FC", "", freqcons_caça)

caçador_familia <-  names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "CF")]

como_consome_caça <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "CC")]
#como_consome_caça <- gsub("^CC", "", como_consome_caça)

quem_limpa_caça <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "LP")]
#quem_limpa_caça <- gsub("^LP", "", quem_limpa_caça)

problema_carne_caça <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "PC")]
#problema_carne_caça <- gsub("^PC", "", problema_carne_caça)

problemas_encontrados_caça <- c("Alt_Carn", "CARO_FIG", "CHUMBO",
                                "FERIDA","MAGRA", "MANCH_BR","MICROB",
                                "NÃO_DET","SINT_DO","SINT_INF","PARASIT",
                                "PERD_PEL","REMOSA","TUMOR","URA","VERMES")

caça_relatada_problema <- c("ANTA","BOI","ARAÇU","BOZO",
                            "CACHO","CAITI","CAPIV","CARAT",
                            "COELH","CUTIA","GALI","GATO","GUARI",
                            "JABUT","jacamim","acu","ACUN","JAVAL",
                            "MACAC","MACBG","MACPREG","MCVLH","MUCUR",
                            "MUTUM","ONÇA","ONCPI","OURIÇ","OUTROS","PACA",
                            "PEIXE","piranh","PXBOI","PESCA","PORCO","PORMA",
                            "PREGU","QUATI","QUEIX","RAPOS","TAMAN","TATU",
                            "TODOS","TRAIR","TUCUN","VEADO")


destino_caça_impropria <- c("COZINCOM","ANIMCOME","DEIXAMAT","ENTERRA","ESCALDA",
                            "JOGALIXO","JOGAMATO","NAOCOME","OUTROS","QUEIMA",
                            "TIRPEDAC")

animal_come_caça_impropria <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "QA")]
#animal_come_caça_impropria <- gsub("^QA", "", animal_come_caça_impropria)

# Função para consolidar um grupo de colunas
consolidar_grupo <- function(dados, colunas_grupo, prefixo) {
  dados %>%
    select(all_of(colunas_grupo)) %>%
    pmap_chr(function(...) {
      respostas <- c(...)
      indices <- which(respostas == 1)
      if(length(indices) == 0) return(NA_character_)
      itens_limpos <- gsub(paste0("^", prefixo), "", colunas_grupo[indices])
      paste(itens_limpos, collapse = "; ")
    })
}

# Aplicar a todos os grupos
resultado_caça <- list_of_tables$Caça %>%
  select(Amostra, Comunidade) %>%
  mutate(
    carne_musada = consolidar_grupo(list_of_tables$Caça, carne_musada, "MU"),
    caça_preferida = consolidar_grupo(list_of_tables$Caça, caça_preferida, "CP"),
    freqcons_caça = consolidar_grupo(list_of_tables$Caça, freqcons_caça, "FC"),
    caçador_familia = consolidar_grupo(list_of_tables$Caça, caçador_familia, "CF"),
    como_consome_caça = consolidar_grupo(list_of_tables$Caça, como_consome_caça, "CC"),
    quem_limpa_caça = consolidar_grupo(list_of_tables$Caça, quem_limpa_caça, "LP"),
    problema_carne_caça = consolidar_grupo(list_of_tables$Caça, problema_carne_caça, "PC"),
    problemas_encontrados_caça = consolidar_grupo(list_of_tables$Caça, problemas_encontrados_caça, "EN"),
    caça_relatada_problema = consolidar_grupo(list_of_tables$Caça, caça_relatada_problema, "RP"),
    destino_caça_impropria = consolidar_grupo(list_of_tables$Caça, destino_caça_impropria, "DI"),
    animal_come_caça_impropria = consolidar_grupo(list_of_tables$Caça, animal_come_caça_impropria, "AI")
  )

# resultado_final %>%
#   pivot_longer(
#     cols = -Amostra,
#     names_to = "pergunta",
#     values_to = "respostas"
#   ) %>%
#   # Separar respostas múltiplas em linhas diferentes
#   separate_rows(respostas, sep = ";\\s*") %>%
#   filter(!is.na(respostas) & respostas != "") %>%
#   arrange(Amostra, pergunta, respostas)


# ANIMAIS

animais_nunca_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "NP")]

animais_sempre_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "SP")]

animais_epoca_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "EDQ")]

animais_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "EDP")]

animais_chegam_longe <-  names(list_of_tables$Animais[
  , startsWith(names(list_of_tables$Animais), "CL") & 
    !startsWith(names(list_of_tables$Animais), "CLQ") &
    !startsWith(names(list_of_tables$Animais), "CLDO"),
  drop = FALSE
])

animais_chegam_quando <-  names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "CLQ")]

vetores_conhecidos <- c("AEDES","CARRAP","CARAMU","CASCUD","PERNILO","TATUQUI")

onde_ve_vetores <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "OV")]

outros_vetores <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "OA")]

doenças_conhecidas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), c("DC","DR"))]

resultado_animais <- list_of_tables$Animais %>%
  select(Amostra, Comunidade) %>%
  mutate(
    animais_nunca_problemas = consolidar_grupo(list_of_tables$Animais, animais_nunca_problemas, "NP"),
    animais_sempre_problemas = consolidar_grupo(list_of_tables$Animais, animais_sempre_problemas, "SP"),
    animais_epoca_problemas = consolidar_grupo(list_of_tables$Animais, animais_epoca_problemas, "EDQ"),
    animais_problemas = consolidar_grupo(list_of_tables$Animais, animais_problemas, "EDP"),
    animais_chegam_longe = consolidar_grupo(list_of_tables$Animais, animais_chegam_longe, "CL"),
    animais_chegam_quando = consolidar_grupo(list_of_tables$Animais, animais_chegam_quando, "CLQ"),
    vetores_conhecidos = consolidar_grupo(list_of_tables$Animais, vetores_conhecidos, ""),
    onde_ve_vetores = consolidar_grupo(list_of_tables$Animais, onde_ve_vetores, "OV"),
    outros_vetores = consolidar_grupo(list_of_tables$Animais, outros_vetores, "OA"),
    doenças_conhecidas = consolidar_grupo(list_of_tables$Animais, doenças_conhecidas, c("DC","DR")),
  )



resex_resultado <- list(list_of_tables$Etário,resultado_casa,resultado_habitos,
                        resultado_atividade,resultado_caça,
                        resultado_animais)

names(resex_resultado) <- c("Etario","Casa","Habitos","Atividade","Caça","Animal")

save(resex_resultado, file = "./resex_ok.RData")















































