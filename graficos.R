# Carregar pacotes necessários
library(pacman)
p_load(tidyverse, dplyr, readxl, gridExtra)

# Importar e pré-processar dados
dados <- read_excel("Página 14 - Linha 650(Planilha2).xlsx", 
                    sheet = "Todas_Amostras") %>%
  filter(Placa != "NA") %>%
  mutate(`Valor Venal` = as.numeric(`Valor Venal`),
         `Convencional` = as.numeric(`Convencional`)) %>%
  mutate(regiao = case_when(
    UF %in% c("AC", "AL", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    UF %in% c("BA", "CE", "ES", "MA", "PB", "PE", "PI", "RJ", "RN", "SE") ~ "Nordeste",
    UF %in% c("GO", "MS", "MT", "DF") ~ "Centro-Oeste",
    UF %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste",
    UF %in% c("PR", "SC", "RS") ~ "Sul",
    TRUE ~ NA
  ))

dados$regiao <- factor(dados$regiao, levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))

# Testes estatísticos Kruskal-Wallis por técnica de amostragem
kruskal.test(`Valor Venal` ~ regiao, data = dados %>% filter(`Técnica de Amostragem` == "AAS_1"))
kruskal.test(`Valor Venal` ~ regiao, data = dados %>% filter(`Técnica de Amostragem` == "AAS_2"))
kruskal.test(`Valor Venal` ~ regiao, data = dados %>% filter(`Técnica de Amostragem` == "SIS_1"))
kruskal.test(`Valor Venal` ~ regiao, data = dados %>% filter(`Técnica de Amostragem` == "SIS_2"))

#-----------------------------------------------------------
# Criação dos BOXPLOTS por região e técnica de amostragem
#-----------------------------------------------------------

# Boxplot para AAS_1
graf1 <- dados %>% filter(`Técnica de Amostragem` == "AAS_1") %>%
  ggplot(aes(x = regiao, y = `Valor Venal`, fill = regiao)) +
  geom_boxplot() +
  labs(x = "Região", y = "Valor Venal", title = "Simples - 1") +
  theme_bw() +
  coord_cartesian(ylim = c(0, max(dados$`Valor Venal`) + 1000)) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Boxplot para AAS_2
graf2 <- dados %>% filter(`Técnica de Amostragem` == "AAS_2") %>%
  ggplot(aes(x = regiao, y = `Valor Venal`, fill = regiao)) +
  geom_boxplot() +
  labs(x = "Região", y = "Valor Venal", title = "Simples - 2") +
  theme_bw() +
  coord_cartesian(ylim = c(0, max(dados$`Valor Venal`) + 1000)) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Boxplot para SIS_1
graf3 <- dados %>% filter(`Técnica de Amostragem` == "SIS_1") %>%
  ggplot(aes(x = regiao, y = `Valor Venal`, fill = regiao)) +
  geom_boxplot() +
  labs(x = "Região", y = "Valor Venal", title = "Sistemática - 1") +
  theme_bw() +
  coord_cartesian(ylim = c(0, max(dados$`Valor Venal`) + 1000)) +
  guides(fill = "none")

# Boxplot para SIS_2
graf4 <- dados %>% filter(`Técnica de Amostragem` == "SIS_2") %>%
  ggplot(aes(x = regiao, y = `Valor Venal`, fill = regiao)) +
  geom_boxplot() +
  labs(x = "Região", y = "Valor Venal", title = "Sistemática - 2") +
  theme_bw() +
  coord_cartesian(ylim = c(0, max(dados$`Valor Venal`) + 1000)) +
  guides(fill = "none")

# Salvar os boxplots em um único arquivo
ggsave("boxplots_valor_venal_regiao.png", arrangeGrob(graf1, graf2, graf3, graf4, ncol = 2),
       width = 200, height = 150, units = "mm")

#-----------------------------------------------------------
# Criação dos GRÁFICOS DE BARRAS por marca e técnica
#-----------------------------------------------------------

# Gráfico de barras para AAS_1
graf1 <- dados %>% 
  filter(`Técnica de Amostragem` == "AAS_1") %>%
  count(Marca) %>%  
  ggplot(aes(x = reorder(Marca, n), y = n)) +  
  geom_bar(stat = "identity", fill = "#13478C") +  
  labs(x = "Marca", y = "Frequência", title = "Simples - 1") +  
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, by = 2))

# Gráfico de barras para AAS_2
graf2 <- dados %>% 
  filter(`Técnica de Amostragem` == "AAS_2") %>%
  count(Marca) %>%  
  ggplot(aes(x = reorder(Marca, n), y = n)) +  
  geom_bar(stat = "identity", fill = "#13478C") +  
  labs(x = "Marca", y = "Frequência", title = "Simples - 2") +  
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, by = 2))

# Gráfico de barras para SIS_1
graf3 <- dados %>% 
  filter(`Técnica de Amostragem` == "SIS_1") %>%
  count(Marca) %>%  
  ggplot(aes(x = reorder(Marca, n), y = n)) +  
  geom_bar(stat = "identity", fill = "#13478C") +  
  labs(x = "Marca", y = "Frequência", title = "Sistemática - 1") +  
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, by = 2))

# Gráfico de barras para SIS_2
graf4 <- dados %>% 
  filter(`Técnica de Amostragem` == "SIS_2") %>%
  count(Marca) %>%  
  ggplot(aes(x = reorder(Marca, n), y = n)) +  
  geom_bar(stat = "identity", fill = "#13478C") +  
  labs(x = "Marca", y = "Frequência", title = "Sistemática - 2") +  
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, by = 2))

# Salvar os gráficos de barras em um único arquivo
ggsave("barras_freq_marcas.png", arrangeGrob(graf1, graf2, graf3, graf4, ncol = 2),
       width = 200, height = 150, units = "mm")




N <- 198
dados_graf_eletrico_e_hibrido <- dados %>%
  filter(`Técnica de Amostragem` == "AAS_1") %>%
  summarise(
    Convencional = sum(Convencional == "1"),
    Elétrico = sum(Elétrico == "1"),
    Híbrido = sum(Híbrido == "1")
  ) %>%
  gather(key = "Categoria", value = "Freq_Abso") %>%
  mutate(
    p = Freq_Abso / sum(Freq_Abso),
    f = sum(Freq_Abso)/N,
    n = sum(Freq_Abso),
    erro_amostral = sqrt((1-f)*(p*(1-p))/(n-1))
  )

graf1 <- dados_graf_eletrico_e_hibrido %>%
  ggplot(aes(x = Categoria, y = p, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Barras
  geom_errorbar(
    aes(
      ymin = p - erro_amostral,  # Definir o intervalo inferior
      ymax = p + erro_amostral   # Definir o intervalo superior
    ),
    width = 0.2,
    position = position_dodge(0.6)  # Para alinhar as barras com as barras de erro
  ) +
  theme_bw() +
  labs(y = "Proporção",
       title = "Simples - 1") + 
  guides(fill = "none")


dados_graf_eletrico_e_hibrido <- dados %>%
  filter(`Técnica de Amostragem` == "AAS_2") %>%
  summarise(
    Convencional = sum(Convencional == "1"),
    Elétrico = sum(Elétrico == "1"),
    Híbrido = sum(Híbrido == "1")
  ) %>%
  gather(key = "Categoria", value = "Freq_Abso") %>%
  mutate(
    p = Freq_Abso / sum(Freq_Abso),
    f = sum(Freq_Abso)/N,
    n = sum(Freq_Abso),
    erro_amostral = sqrt((1-f)*(p*(1-p))/(n-1))
  )

graf2 <- dados_graf_eletrico_e_hibrido %>%
  ggplot(aes(x = Categoria, y = p, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Barras
  geom_errorbar(
    aes(
      ymin = p - erro_amostral,  # Definir o intervalo inferior
      ymax = p + erro_amostral   # Definir o intervalo superior
    ),
    width = 0.2,
    position = position_dodge(0.6)  # Para alinhar as barras com as barras de erro
  ) +
  theme_bw() +
  labs(y = "Proporção",
       title = "Simples - 2") +  
  guides(fill = "none")

dados_graf_eletrico_e_hibrido <- dados %>%
  filter(`Técnica de Amostragem` == "SIS_1") %>%
  summarise(
    Convencional = sum(Convencional == "1"),
    Elétrico = sum(Elétrico == "1"),
    Híbrido = sum(Híbrido == "1")
  ) %>%
  gather(key = "Categoria", value = "Freq_Abso") %>%
  mutate(
    p = Freq_Abso / sum(Freq_Abso),
    f = sum(Freq_Abso)/N,
    n = sum(Freq_Abso),
    erro_amostral = sqrt((1-f)*(p*(1-p))/(n-1))
  )

graf3 <- dados_graf_eletrico_e_hibrido %>%
  ggplot(aes(x = Categoria, y = p, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Barras
  geom_errorbar(
    aes(
      ymin = p - erro_amostral,  # Definir o intervalo inferior
      ymax = p + erro_amostral   # Definir o intervalo superior
    ),
    width = 0.2,
    position = position_dodge(0.6)  # Para alinhar as barras com as barras de erro
  ) +
  theme_bw() +
  labs(y = "Proporção",
       title = "Sistemática - 1") +  
  guides(fill = "none")

dados_graf_eletrico_e_hibrido <- dados %>%
  filter(`Técnica de Amostragem` == "SIS_2") %>%
  summarise(
    Convencional = sum(Convencional == "1"),
    Elétrico = sum(Elétrico == "1"),
    Híbrido = sum(Híbrido == "1")
  ) %>%
  gather(key = "Categoria", value = "Freq_Abso") %>%
  mutate(
    p = Freq_Abso / sum(Freq_Abso),
    f = sum(Freq_Abso)/N,
    n = sum(Freq_Abso),
    erro_amostral = sqrt((1-f)*(p*(1-p))/(n-1))
  )

graf4 <- dados_graf_eletrico_e_hibrido %>%
  ggplot(aes(x = Categoria, y = p, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Barras
  geom_errorbar(
    aes(
      ymin = p - erro_amostral,  # Definir o intervalo inferior
      ymax = p + erro_amostral   # Definir o intervalo superior
    ),
    width = 0.2,
    position = position_dodge(0.6)  # Para alinhar as barras com as barras de erro
  ) +
  theme_bw() +
  labs(y = "Proporção",
       title = "Sistemática - 2") +  
  guides(fill = "none")


# ggsave("barras_total_eletricos_hibridos.png", arrangeGrob(graf1, graf2, graf3, graf4, ncol = 2),
#        width = 200, height = 150, units = "mm")

ggsave("barras_proporcao_eletricos_hibridos.png", arrangeGrob(graf1, graf2, graf3, graf4, ncol = 2),
       width = 200, height = 150, units = "mm")




print_quadro_resumo(dados %>% group_by(`Técnica de Amostragem`), var_name = `Valor Venal`)


a<- dados %>% group_by(`Técnica de Amostragem`) %>%
  summarise(x_barra = mean(`Valor Venal`),
            s2 = var(`Valor Venal`),
            n = n()) %>%
  mutate(f = n/N,
         t = qt(0.975, n-1),
         erro_amostral = t*sqrt((1-f)*s2/n),
         lim_inf = x_barra - erro_amostral,
         lim_sup = x_barra + erro_amostral)

a$`Técnica de Amostragem` <- factor(a$`Técnica de Amostragem`, 
                                    levels = c("AAS_1", "SIS_1", "AAS_2", "SIS_2"),
                                    labels = c("Simples - 1", "Sistemática - 1", "Simples - 2", "Sistemática - 2"))

  

graf_a <- a %>%
  ggplot(aes(x = `Técnica de Amostragem`, color = `Técnica de Amostragem`)) +
  geom_point(aes(y = x_barra)) +
  geom_errorbar(
    aes(
      ymin = lim_inf,  
      ymax = lim_sup   
    ),
    width = 0.2,
    position = position_dodge(0.6) 
  ) +
  geom_text(
    aes(
      y = lim_inf, 
      label = round(lim_inf, 2)  
    ),
    vjust = 2,  
    color = "black",
    size = 3  
  ) +
  geom_text(
    aes(
      y = lim_sup, 
      label = round(lim_sup, 2)  
    ),
    vjust = -1,  
    color = "black",
    size = 3  
  )+
  theme_bw() +
  labs(y = "Intervalo de Confiança da Média do Valor Venal") +
  guides(color = "none")+
  scale_x_discrete(limits = c("Simples - 1", "Sistemática - 1", "Simples - 2", "Sistemática - 2"))
  #coord_flip()

ggsave("intervalos_conf_media_valor_venal.png", graf_a,
       width = 200, height = 150, units = "mm")


a<- dados %>% 
  group_by(`Técnica de Amostragem`) %>%
  summarise(soma = sum(`Convencional`),
            n = n()) %>%
  mutate(f = n/N,
         p = (n - soma)/n,
         z = qnorm(0.975),
         s2 = 0.25,
         erro_amostral = z*sqrt((1-f)*s2/(n-1)),
         lim_inf = p - erro_amostral,
         lim_sup = p + erro_amostral)

a$`Técnica de Amostragem` <- factor(a$`Técnica de Amostragem`, 
                                    levels = c("AAS_1", "SIS_1", "AAS_2", "SIS_2"),
                                    labels = c("Simples - 1", "Sistemática - 1", "Simples - 2", "Sistemática - 2"))



graf_a <- a %>%
  ggplot(aes(x = `Técnica de Amostragem`, color = `Técnica de Amostragem`)) +
  geom_point(aes(y = p)) +
  geom_errorbar(
    aes(
      ymin = lim_inf,  
      ymax = lim_sup   
    ),
    width = 0.2,
    position = position_dodge(0.6) 
  ) +
  geom_text(
    aes(
      y = lim_inf, 
      label = round(lim_inf, 4)  
    ),
    vjust = 2,  
    color = "black",
    size = 3  
  ) +
  geom_text(
    aes(
      y = lim_sup, 
      label = round(lim_sup, 4)  
    ),
    vjust = -1,  
    color = "black",
    size = 3  
  )+
  theme_bw() +
  labs(y = "Intervalo de Confiança da Proporção de Carros Eletrificados") +
  guides(color = "none")+
  scale_x_discrete(limits = c("Simples - 1", "Sistemática - 1", "Simples - 2", "Sistemática - 2"))
#coord_flip()

ggsave("intervalos_conf_proporção_carros_eletrificados.png", graf_a,
       width = 200, height = 150, units = "mm")
