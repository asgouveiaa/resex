# NOVA FUNCIONALIDADE: Configuração de cenários COM TIPO DIFERENCIADO
scenario_config <- list(
  "Cametá" = list(
    type = "monthly",       # Caça mensal baseada em dias do mês
    hunt_days = c(15),      # 1 vez ao mês: dia 15
    hunt_weekdays = NULL,    # Não usado para caça mensal,
    Sh = 38,
    Sc = 180-38
  ),
  "Escrivão" = list(
    type = "monthly",       # Caça mensal baseada em dias do mês
    hunt_days = c(7, 15),   # 2 vezes ao mês: dias 7 e 15
    hunt_weekdays = NULL,
    Sh = 12,
    Sc = 57-12
  ),
  "Nova Vista" = list(
    type = "weekly",        # Caça SEMANAL baseada em dias da semana
    hunt_days = NULL,       # Não usado para caça semanal
    hunt_weekdays = c(2, 4, 7), # 3 vezes na semana: segunda(2), quarta(4), sábado(7)
    Sh = 18,
    Sc = 40-18
  ),
  "Pinhel" = list(
    type = "weekly",        # Caça SEMANAL baseada em dias da semana
    hunt_days = NULL,       # Não usado para caça semanal
    hunt_weekdays = c(6),    # 1 vez na semana: sexta(6)
    Sh = 20,
    Sc = 55-20
  )
)

# Funções auxiliares para determinar dias de caça
is_hunting_day_monthly <- function(t, hunt_days) {
  # Converter t para data (assumindo que t=1 corresponde a "1970-01-01")
  current_date <- as.Date("1970-01-01") + (t - 1)
  day_of_month <- as.numeric(format(current_date, "%d"))
  
  # Verificar se o dia do mês está na lista de dias de caça
  return(ifelse(day_of_month %in% hunt_days, 1, 0))
}

is_hunting_day_weekly <- function(t, hunt_weekdays) {
  # Dias da semana: 1=domingo, 2=segunda, 3=terça, 4=quarta, 5=quinta, 6=sexta, 7=sábado
  # Ajustar para o formato do seu código: weekday <- (floor(t) %% 7) + 1
  weekday <- (floor(t) %% 7) + 1
  
  # Verificar se o dia da semana está na lista de dias de caça
  return(ifelse(weekday %in% hunt_weekdays, 1, 0))
}

# Simulation parameters
n_years <- 10
total_days <- n_years * 365
tempo <- seq(1, total_days, by = 1)
lanina_years <- c(0)

# Gerar dados de precipitação (mantendo seu código original)
precipitation_data <- generate_precipitation_dataset(
  years = n_years,
  la_nina_years = lanina_years,  
  nina_intensity = 1.5,
  drought_periods = create_periods(c(0,0,0), c(0,0,0)),
  flood_periods = create_periods(c(0,0), c(0,0)),
  flood_intensity = 3
) %>%
  mutate(p_normalizado = (precipitation - min(precipitation)) / 
           (max(precipitation) - min(precipitation)))

# FUNÇÃO PARA RODAR SIMULAÇÃO PARA UM CENÁRIO ESPECÍFICO
run_scenario_simulation <- function(scenario_name, scenario_info) {
  cat("Executando cenário:", scenario_name, "\n")
  cat("Tipo de caça:", scenario_info$type, "\n")
  cat("Caçadores:", scenario_info$Sh, "\n")
  cat("Consumidores:", scenario_info$Sc, "\n")
  
  if (scenario_info$type == "monthly") {
    cat("Dias de caça por mês:", paste(scenario_info$hunt_days, collapse = ", "), "\n")
    hunt_type <- 1  # 1 = mensal
  } else {
    cat("Dias de caça por semana:", paste(scenario_info$hunt_weekdays, collapse = ", "), "\n")
    # Converter números para nomes dos dias
    weekday_names <- c("domingo", "segunda", "terça", "quarta", "quinta", "sexta", "sábado")
    cat("Dias da semana:", paste(weekday_names[scenario_info$hunt_weekdays], collapse = ", "), "\n")
    hunt_type <- 2  # 2 = semanal
  }
  
  # Model parameters - CORRIGIDO: sem parâmetros de texto
  pars <- list(
    # HUNTING
    h = 0.13,                    # hunting efficiency
    h_sazo = 1.7,#1.2,               # seasonal increase 
    base_time_forest = 1,#4,       # average daily hunting time (hours)
    time_max_optimal = 1,#6,       # increased max hunting time
    hunt_type = hunt_type,      # 1 = mensal, 2 = semanal
    hunt_days_monthly = if(!is.null(scenario_info$hunt_days)) scenario_info$hunt_days else numeric(0),
    hunt_days_weekly = if(!is.null(scenario_info$hunt_weekdays)) scenario_info$hunt_weekdays else numeric(0),
    hunter_experience = 0,      # experience factor
    
    # ANIMALS
    r = 0.002,                  # daily growth rate
    K_max = 2000,              # maximum carrying capacity
    K_sazo = 0.85,              # seasonal decrease
    mu = 0.0002,                # daily mortality
    lambda_base = 0.1,          # baseline transmission rate
    lambda_agg_boost = 5,#2.5,     # stronger aggregation effect
    migration_rate = 0,         # migration rate  
    
    # PATHOGENS
    phi = 50,                   # shedding rate
    nu = 0.7,                   # slower pathogen decay
    P_max = 1e6,                # max pathogen load
    pathogen_seasonality = 0.3, # seasonal pathogen survival
    
    # SPILLOVER - Direct transmission (pathogen 1) - for hunters
    injurie_risk = 0, p_I = 0, w_injuries = 0,          # injury risk
    processing_risk = 0, p_P = 0, w_processing = 0,     # processing risk
    eating_risk = 0.1, p_E = 1, w_eating = 1,             # eating risk
    cont_rate = 0, p_F = 0, w_fomites = 0,              # fomite risk
    protective_equipment = 0,                           # PPE usage rate
    
    ## for consumers
    sharing_fraction = 0.5 ,       # meat shared fraction
    home_processing_risk = 0,   # home processing risk
    home_eating_risk = 1,       # home eating risk
    home_ppe_effect = 0,        # PPE at home
    cooking_protection = 0,     # risk reduction by cooking
    
    # SPILLOVER - Vector transmission (pathogen 2)
    encounters_day = 10,        # encounters per day
    p_V = 0.15,                 # higher infection probability
    bite_risk = 0.49,           # higher bite risk
    vector_seasonality = 0      # seasonal vector activity
  )
  
  # Initial conditions
  state <- c(
    Sh = scenario_info$Sh,                   # Hunters S
    Ih = 0,                     # Hunters I
    Sc = scenario_info$Sc,                   # Consumers S
    Ic = 0,                     # Consumers I
    A = 1000,                  # Animals S
    Ia = 100,                  # Animals I
    P = 0                       # Pathogen in the environment
  )
  
  # MODELO ADAPTADO COM LÓGICA DE CAÇA FLEXÍVEL
  spillover_model <- function(t, state, pars, p_data) {
    with(as.list(c(state, pars)), {
      # Time indexing
      t_int <- pmax(1, pmin(floor(t), length(p_data)))
      p_norm <- p_data[t_int]
      
      # Climate-dependent effects
      threshold <- 0.7
      clim_effect <- pmax(0, 0.3 * (p_norm - threshold) / (1 - threshold))
      agg_factor <- 1 + clim_effect
      
      # Carrying capacity
      K_effective <- K_max * (K_sazo + (1 - K_sazo) * (1 - p_norm))
      
      lambda_effective <- lambda_base * (1 + (agg_factor - 1) * lambda_agg_boost)
      
      # LÓGICA FLEXÍVEL: Determinar se é dia de caça baseado no tipo (NUMÉRICO)
      if (hunt_type == 1 && length(hunt_days_monthly) > 0) {
        # Caça mensal
        current_date <- as.Date("1970-01-01") + (t - 1)
        day_of_month <- as.numeric(format(current_date, "%d"))
        is_hunting_day <- ifelse(day_of_month %in% hunt_days_monthly, 1, 0)
      } else if (hunt_type == 2 && length(hunt_days_weekly) > 0) {
        # Caça semanal
        weekday <- (floor(t) %% 7) + 1
        is_hunting_day <- ifelse(weekday %in% hunt_days_weekly, 1, 0)
      } else {
        is_hunting_day <- 0
      }
      
      # Weather-dependent hunting (hunters avoid rain)
      weather_factor <- ifelse(p_norm > 0.8, 0, 1.0)
      
      # Time in forest calculation
      optimal_climate <- 0.2
      climate_hunting_factor <- 1 - 0.5 * abs(p_norm - optimal_climate)
      
      time_in_forest_base <- (base_time_forest + 
                                (time_max_optimal - base_time_forest) * 
                                climate_hunting_factor) / 24
      
      # Effective rates on hunting days only
      time_in_forest <- is_hunting_day * time_in_forest_base * weather_factor
      hunting_rate <- is_hunting_day * h * (1 + h_sazo * p_norm)
      
      # Inicializa derivadas
      dSh <- dIh <- dSc <- dIc <- dA <- dIa <- dP <- 0
      hunter_spillover_prob <- 0
      consumer_spillover_prob <- 0
      
      # ------------------------------------------------------------
      # DINÂMICA DOS CAÇADORES (PRODUTORES)
      # ------------------------------------------------------------
      if (is_hunting_day == 1 && time_in_forest > 0) {
        animal_prop <- ifelse((Ia + A) > 0, Ia / (Ia + A), 0)
        
        # Componentes de risco para caçadores
        injury_risk_component <- injurie_risk * w_injuries * p_I * agg_factor * (1 - protective_equipment)
        processing_risk_hunter <- processing_risk * w_processing * p_P * (1 - protective_equipment)
        eating_risk_hunter <- eating_risk * w_eating * p_E
        fomite_risk <- time_in_forest * cont_rate * p_F * w_fomites * P * (1 - protective_equipment)
        
        # Risco total caçadores
        hunter_risk <- time_in_forest * hunting_rate * (
          injury_risk_component + 
            processing_risk_hunter + 
            eating_risk_hunter
        ) * animal_prop
        
        # Transmissão vetorial
        vector_seasonal_factor <- 1 + vector_seasonality * sin(2 * pi * (t - 120) / 365)
        vector_risk_hunter <- time_in_forest * encounters_day * 
          bite_risk * p_V * animal_prop * vector_seasonal_factor
        
        hunter_spillover <- (1 - exp(-hunter_risk)) * Sh
        vector_spillover <- (1 - exp(-vector_risk_hunter)) * Sh
        
        hunter_spillover_prob <- (1 - exp(-hunter_risk)) + (1 - exp(-vector_risk_hunter))
        
        dSh <- -(hunter_spillover + vector_spillover)
        dIh <- hunter_spillover + vector_spillover
      }
      
      # ------------------------------------------------------------
      # DINÂMICA DOS FAMILIARES (CONSUMIDORES)
      # ------------------------------------------------------------
      if (hunting_rate > 0) {
        # Proporção de carne infectada compartilhada
        prop_infected <- ifelse((Ia + A) > 0, Ia / (Ia + A), 0)
        shared_meat <- sharing_fraction * hunting_rate * prop_infected
        
        # Riscos específicos para consumidores
        home_processing_risk_component <- shared_meat * home_processing_risk * (1 - home_ppe_effect)
        home_eating_risk_component <- shared_meat * home_eating_risk * (1 - cooking_protection)
        
        consumer_risk <- home_processing_risk_component + home_eating_risk_component
        
        consumer_spillover_prob <- (1 - exp(-consumer_risk))
        
        consumer_spillover <- (1 - exp(-consumer_risk)) * Sc
        
        dSc <- -consumer_spillover
        dIc <- consumer_spillover
      }
      
      # ------------------------------------------------------------
      # DINÂMICA ANIMAL E AMBIENTAL
      # ------------------------------------------------------------
      # Transmissão animal-animal
      animal_transmission <- ifelse((Ia + A) > 0, 
                                    lambda_effective * A * (Ia / (Ia + A)), 0)
      
      # Equações diferenciais
      dA <- r * A * (1 - (A + Ia) / K_effective) - 
        Sh * hunting_rate * A/(Ia+A) - mu * A - 
        animal_transmission + 0.1 * Ia
      
      dIa <- animal_transmission - Sh * hunting_rate * Ia/(Ia+A) - 
        (mu*5) * Ia - 0.1 * Ia
      
      dP <- phi * Ia * (1 - (P / P_max)) - nu * P
      
      # ------------------------------------------------------------
      # OUTPUTS ADICIONAIS
      # ------------------------------------------------------------
      # Obter informações de data para análise
      current_date <- as.Date("1970-01-01") + (t - 1)
      day_of_month <- as.numeric(format(current_date, "%d"))
      month <- as.numeric(format(current_date, "%m"))
      year_num <- as.numeric(format(current_date, "%Y"))
      weekday_num <- (floor(t) %% 7) + 1  # 1=domingo, 2=segunda, ..., 7=sábado
      
      list(
        c(dSh = dSh, dIh = dIh, dSc = dSc, dIc = dIc, 
          dA = dA, dIa = dIa, dP = dP),
        
        # Saídas para análise
        hunter_spillover = ifelse(exists("hunter_spillover"), hunter_spillover, 0),
        vector_spillover = ifelse(exists("vector_spillover"), vector_spillover, 0),
        consumer_spillover = ifelse(exists("consumer_spillover"), consumer_spillover, 0),
        prop_meat_infected = ifelse((Ia + A) > 0, Ia / (Ia + A), 0),
        hunting_rate = hunting_rate,
        K_effective = K_effective,
        lambda_effective = lambda_effective,
        time_in_forest = time_in_forest,
        is_hunting_day = is_hunting_day,
        day_of_month = day_of_month,
        month = month,
        year = year_num,
        weekday = weekday_num,
        hunter_spillover_prob = hunter_spillover_prob,
        consumer_spillover_prob = consumer_spillover_prob
      )
    })
  }
  
  # Run simulation
  out <- ode(y = state, times = tempo, func = spillover_model, 
             parms = pars, p_data = precipitation_data$p_normalizado)
  
  # Process results
  resultado <- as.data.frame(out) %>%
    left_join(precipitation_data, by = c("time" = "day"))
  
  # Adicionar identificador do cenário
  resultado$scenario <- scenario_name
  resultado$scenario_type <- scenario_info$type
  
  if (scenario_info$type == "monthly") {
    resultado$hunt_frequency <- paste("Mensal: dias", paste(scenario_info$hunt_days, collapse = ", "))
  } else {
    weekday_names <- c("domingo", "segunda", "terça", "quarta", "quinta", "sexta", "sábado")
    days_text <- paste(weekday_names[scenario_info$hunt_weekdays], collapse = ", ")
    resultado$hunt_frequency <- paste("Semanal:", days_text)
  }
  
  return(resultado)
}

# TESTAR PRIMEIRO COM UM CENÁRIO APENAS
cat("\n=== TESTANDO CENÁRIO CAMETÁ ===\n")
tryCatch({
  resultado_cameta <- run_scenario_simulation("Cametá", scenario_config$Cametá)
  cat("Sucesso! Cenário Cametá executado.\n")
  print(head(resultado_cameta))
  
  # SE FUNCIONAR, EXECUTAR TODOS OS CENÁRIOS
  cat("\n=== EXECUTANDO TODOS OS CENÁRIOS ===\n")
  all_results <- list()
  
  for(scenario_name in names(scenario_config)) {
    cat("\n--- Executando:", scenario_name, "---\n")
    all_results[[scenario_name]] <- run_scenario_simulation(
      scenario_name, 
      scenario_config[[scenario_name]]
    )
  }
  
  # JUNTAR TODOS OS RESULTADOS PARA ANÁLISE COMPARATIVA
  combined_results <- bind_rows(all_results)
  
  # ANÁLISE DOS RESULTADOS
  summary_stats <- combined_results %>%
    group_by(scenario, scenario_type, hunt_frequency) %>%
    summarise(
      total_hunting_days = sum(is_hunting_day),
      avg_hunting_days_per_month = total_hunting_days / (n_years * 12),
      avg_hunting_days_per_week = total_hunting_days / (n_years * 52.1429),
      total_hunter_infections = max(Ih),
      total_consumer_infections = max(Ic),
      avg_hunting_rate = mean(hunting_rate[is_hunting_day == 1], na.rm = TRUE),
      avg_hunter_spillover_prob = mean(hunter_spillover_prob[is_hunting_day == 1], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(scenario_type, desc(total_hunting_days))
  
  cat("\n=== RESUMO DOS CENÁRIOS ===\n")
  print(summary_stats)
  
  # VISUALIZAÇÃO BÁSICA
  library(ggplot2)
  
  # Gráfico simples de dias de caça
  hunting_summary_plot <- combined_results %>%
    filter(time <= 30) %>%  # Primeiros 90 dias
    ggplot(aes(x = time, y = is_hunting_day, color = scenario)) +
    geom_line() +
    facet_wrap(~scenario, ncol = 2) +
    labs(
      title = "Padrão de Dias de Caça - Primeiros 90 Dias",
      x = "Dia",
      y = "Dia de Caça"
    ) +
    theme_minimal()
  
  print(hunting_summary_plot)
  
  # Exportar resultados
  # write.csv(combined_results, "resultados_cenarios.csv", row.names = FALSE)
  
}, error = function(e) {
  cat("Erro durante execução:", e$message, "\n")
  
  # Versão simplificada para debugging
  cat("\nTentando versão simplificada...\n")
  
  # Executar apenas com parâmetros mínimos
  simple_params <- list(
    h = 0.33,
    h_sazo = 1.2,
    base_time_forest = 1,
    time_max_optimal = 1,
    hunt_type = 1,  # mensal
    hunt_days_monthly = c(15),
    hunt_days_weekly = numeric(0),
    hunter_experience = 0,
    r = 0.001,
    K_max = 50000,
    K_sazo = 0.85,
    mu = 0.0001,
    lambda_base = 0.1,
    lambda_agg_boost = 2.5,
    migration_rate = 0,
    phi = 50,
    nu = 0.7,
    P_max = 1e6,
    pathogen_seasonality = 0.3,
    injurie_risk = 0, p_I = 0, w_injuries = 0,
    processing_risk = 0, p_P = 0, w_processing = 0,
    eating_risk = 0, p_E = 0, w_eating = 0,
    cont_rate = 0, p_F = 0, w_fomites = 0,
    protective_equipment = 0,
    sharing_fraction = 0,
    home_processing_risk = 0,
    home_eating_risk = 0,
    home_ppe_effect = 0,
    cooking_protection = 0,
    encounters_day = 10,
    p_V = 0.15,
    bite_risk = 0.49,
    vector_seasonality = 0
  )
  
  simple_state <- c(Sh = 100, Ih = 0, Sc = 400, Ic = 0, A = 40000, Ia = 4000, P = 0)
  
  # Testar com menos dias primeiro
  tempo_test <- seq(1, 30, by = 1)  # Apenas 30 dias para teste
  
  simple_model <- function(t, state, pars, p_data) {
    with(as.list(c(state, pars)), {
      # Lógica simplificada
      t_int <- pmax(1, pmin(floor(t), length(p_data)))
      p_norm <- p_data[t_int]
      
      # Determinar se é dia de caça
      if (hunt_type == 1 && length(hunt_days_monthly) > 0) {
        current_date <- as.Date("1970-01-01") + (t - 1)
        day_of_month <- as.numeric(format(current_date, "%d"))
        is_hunting_day <- ifelse(day_of_month %in% hunt_days_monthly, 1, 0)
      } else {
        is_hunting_day <- 0
      }
      
      # Equações diferenciais básicas
      dSh <- 0
      dIh <- 0
      dSc <- 0
      dIc <- 0
      dA <- 0
      dIa <- 0
      dP <- 0
      
      list(
        c(dSh, dIh, dSc, dIc, dA, dIa, dP),
        is_hunting_day = is_hunting_day
      )
    })
  }
  
  cat("Testando modelo simplificado...\n")
  test_out <- ode(y = simple_state, times = tempo_test, func = simple_model, 
                  parms = simple_params, p_data = precipitation_data$p_normalizado[1:30])
  
  cat("Modelo simplificado executado com sucesso!\n")
  print(head(as.data.frame(test_out)))
})


combined_results %>%
  ggplot() +
  geom_line(aes(x = time, y = A, col = "Susceptible"), linewidth = 0.7) +
  geom_line(aes(x = time, y = Ia, col = "Infected"), linewidth = 0.7) +
  labs(y = "Animal Population", x = "Time (days)", col = "") +
  scale_color_manual(values = c("Susceptible" = "#1E90FF", "Infected" = "red3")) +
  theme_bw() + theme(legend.position = "top") + 
  facet_wrap(~scenario, scales = "free_y")

combined_results %>%
  mutate(prev_hun = Ih/(Sh+Ih),
         prev_anim = Ia/(Ia+A),
         prev_con = ifelse(is.na(Ic/(Ic+Sc)),0,Ic/(Ic+Sc))) %>%
  ggplot() +
  geom_line(aes(x = time, y = prev_anim *100, col = "animal"), linetype = 1) +
  geom_line(aes(x = time, y = prev_hun * 100, col = "hunter"), linetype = 1) +
  geom_line(aes(x = time, y = prev_con * 100, col = "consumers"), linetype = 1) +
  scale_color_manual(values = c("red","#1E90FF","black")) +
  theme_bw(base_size = 15) +
  theme(legend.position = "top") +
  labs(x = "Time (years)", y = "Prevalence (%)", color = "") +
  scale_x_continuous(
    breaks = seq(1, max(combined_results$time), by = 365),  # Breaks at 0, 365, 730
    labels = c(seq(1,10,1))
  ) +
  facet_wrap(~scenario)
