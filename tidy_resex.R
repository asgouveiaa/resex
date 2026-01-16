pacman::p_load(
  tidyverse,readxl
)

file_path <- "./PLANILHA_MATRIZES_ANALISE_RISCO_RESEX.xlsx"
all_sheets <- excel_sheets("./PLANILHA_MATRIZES_ANALISE_RISCO_RESEX.xlsx")

list_of_tables <- map(all_sheets, ~ read_excel(file_path, sheet = .x))
list_of_tables$Caça <- list_of_tables$Caça %>% 
  rename(OUTROS = CPOUTROS,
         QUEIMA = CPQUEIMA)

names(list_of_tables) <- all_sheets # Assign sheet names to list elements

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


## perguntas

#1. Quantas pessoas moram em sua casa?
morad <- c("CSCRIANC","CSADOLEN","CSADULTO","CSIDOSOS") 

#2. Sua casa é de quê?
casa <- c("BARRO", "TIJOLO", "MADEIRA.x","PALHA.x","OUTRO")

#3. Água que sua família bebe é do:
agua <- c("MICRO","NASCENTE","POÇO","RIO.x","OUTRA")

#4. Você usa filtro de água em casa?
filtr <- c("FILSIM","FILNÃO")

#5. Ferve a mamadeira e chupeta das crianças?
ferve <- c("FERVSIM","FERVSEMP","FERVDVQ","FERVRAR","FERVNÃO","FERVNAP")

#6. Quais atividades de sua família?
profi <- c("AGSAUDE","ARTESANAT","COMERC","EXTRAT","PESCA.X","PROFESSOR",
            "ROÇA","TECNOPROJ","ATVAPOSE","ATVCRIAC","ATVSAF","ATVTURIS",
            "ATVOUTRO")

#6.1 O que vocês plantam? (roça)
plant_roça <- c("ABACAT","ABACAXI","ABANO","ABOBOR","ABOBRINH","AÇAI","ACEROLA",
           "AIMARU","ALFACE") 

# 6.2 O que vocês tiram? 
colet <- c("ALFAVAC","AMBE","ANDIROBA","ANGELIM","ARAÇA")

# 6.3 O que vcs plantam? (SAF)
plant_saf <- c("ARROZ","BACABA","BACURI.x","BAMBU","BANANA","BATATA","BATDOCE",
               "BEIJU","BORRACHA","BREU","BURITI...39","CAÇA","CACAU","CAFÉ",
               "CANA","CARÁ","CASTNH","CASTNHPAR","CASTANHCAJ","CEBOLA","CEBOLIN",
               "CEDRO","CHEIROVERD","CHICOR","CIPÓ","COCO","COENTRO","COPAIB",
               "CUMARU","CUPUAÇU","ESPNATIV","FARINHA","FEIJAO","FRUTA","GENGIBRE",
               "GERIMUM","GOIABA","HORTA","INGA","IPE","IXI","ITAUBA","JACARAND",
               "LARANJ","BURITI...74","LATEX","LEGUMES","LEITE","LEITEAMAP",
               "LEITSERI","LEITESUCUB","LEULA","LIMAO","MACAXER","MADEIRA.y",
               "MAMAO","MANACA","MANDIOCA","MANGA","MANGARA","MANICUI",
               "MANIVA","MARACUJ","MAXIXE","MEL","MELANC","MELIPONI",
               "MILHO","MOGNO","MURUCI","OLEO","OLEOAND","OLEOCOC","OLEOCOP",
               "OLEOPIQ","OLEOSIP","PAJURA","PALHA.y","PALHABUR","PALHACIP",
               "PATAMA","PATAUA","PEROBA","PIMENT","PIMENTCHR","PIQUIA","PINHA",
               "PITANGA","PUPUNHA","QUIABO","SEMENT","SUCURUB","SUCUBA","TALA",
               "TANGERIN","TEMPEROS","TIPITI","TIRIRIC","TOMATE","TUCUMA.X",
               "URUCUM","UXI","VERDUR",
               )

criac_animais <- c("ACBOI","ACCABRA","ACCACHOR","ACCAITIT","ACCARNEI",
                   "ACCAVALO","ACCUTIA","ACGALIN","ACGATO","ACJABUTI",
                   "ACMACACO","ACMARREC","ACPACA","ACPSITAC","ACPASSAR",
                   "ACPATO","ACPEIXE","ACPORCO","ACTATU","ACVEADO")

criac_tipo <- c("ACSOLTO","ACPRESO")

criac_motivo <- c("ACCOMER","ACVENDER","ACOUTRA")

animal_vac <- c("AVBOI","AVCAO","AVGATO","AVGALINH","AVPORCO")

quem_vac <- c("QVABARE", "QVPROPRI", "QVPREFEI", "QVPROJET", "QVBOUBA",
              "QVENVAL", "QVFAMILI", "QVOUTROS")

vac_para <- c("QVSAUMEG","QVAFTOSA","QVBOUBA","QVBRUCEL","QVVERDEX","QVOUTRAS",
              "QVVERME","VC_CALAZ","VC_VITAMIN")

nao_identificadas <- c("VQD2013","VQD2012","VQD2010","VQD2015","VQD2014","VQDNOLEMB",
                       "UV_2016", "JABECTO","MACACINT","PASSARIND")


caes_vac_para <- c("CAOENDOP","CAOECTOP","CAOVIRUS","CAOBACTER","CAOLVC",
                       "CAOTRAUM","CAOINTOX","CAOINDET")

galinhas_vac_para <- c("GALVIRUS","GALBOURBA","GALMAREK","GALNEWCA",
                       "GALPARAS","GALIFBAC","GALCORIZ","GALCEGU",
                       "GALINDET","GALINTOX","GALNEOP")

boi_vac_para <- c("GALVIRUS","GALBOURBA","GALMAREK","GALNEWCA",
                       "GALPARAS","GALIFBAC","GALCORIZ","GALCEGU",
                       "GALINDET","GALINTOX","GALNEOP")

gato_vac_para <- c("GATVIRUS", "GATPARAS","GATBACT","GATALERG","GATOIND")

pato_vac_para <- c("PATVIRUS","PATPARAS","PATBACT","PATNUTRI")

porco_vac_para <- c("PORCENDO","PORCECTO","PORCIND")

uso_alcool <- c("HALCOOL")

problemas_saude_homem <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "H")]

problemas_saude_mulher <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "M")]

problemas_saude_idoso <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "I")]

problemas_saude_crianc <- names(list_of_tables$Saúde)[startsWith(names(list_of_tables$Saúde), "C")]

# CAÇA

carne_musada <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "MU")]

caça_preferida <-  names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "CP")]

freqcons_caça <-  names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "FC")]

como_consome_caça <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "CC")]

quem_limpa_caça <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "CC")]

problema_carne_caça <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "PC")]

problemas_encontrados_caça <- c("Alt_Carn", "CARO_FIG", "CHUMBO",
                                "FERIDA","MAGRA", "MANCH_BR","MICROB",
                                "NAO_DET","SINT_DO","SINT_INF","PARASIT",
                                "PERD_PEL","REMOSA","TUMOR","URA","VERMES")

caça_relatada_problema <- c("ANTA","BOI","ARAÇU","BOZO",
                            "CACHO","CAITI","CAPIV","CARAT",
                            "COELH","CUTIA","GALI","GATO","GUARI",
                            "JABUT","jacamin","acu","ACUN","JAVAL",
                            "MACAC","MACBG","MACPREG","MCVLH","MUCUR",
                            "MUTUM","ONÇA","ONCPI","OURIÇ","OUTRO","PACA",
                            "PEIXE","piranh","PXBOI","PESCA","PORCO","PORMA",
                            "PREGU","QUATI","QUEIX","RAPOS","TAMAN","TATU",
                            "TODOS","TRAIR","TUCUN","VEADO")


destino_caça_impropria <- c("COZINCOM","ANIMCOME","DEIXAMAT","ENTERRA","ESCALDA",
                            "JOGALIXO","JOGAMATO","NAOCOME","CPOUTROS","CPQUEIMA",
                            "TIRPEDAC")

animal_come_caça_impropria <- names(list_of_tables$Caça)[startsWith(names(list_of_tables$Caça), "QA")]


# ANIMAIS

animais_nunca_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "NP")]

animais_sempre_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "SP")]

animais_epoca_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "EDQ")]

animais_epoca_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "EDQ")]

animais_problemas <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "EDP")]

animais_chegam_longe <- names(list_of_tables$Animais)[startsWith(names(list_of_tables$Animais), "CL")]

