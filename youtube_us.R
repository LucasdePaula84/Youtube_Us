install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

#Import data
youtube_us <- (read_csv("Downloads/archive-2/USvideos.csv"))

# Analisando as colunas
colnames(youtube_us)

# 1. Somatório de "views" por "channel_title"
soma_views_por_canal <- youtube_us %>%
  group_by(channel_title) %>%
  summarise(somatorio_views = sum(views, na.rm = TRUE))

print(soma_views_por_canal)

# 2. Filtrar os top 10 "channel_title" com mais "views"
top_10_views <- youtube_us %>%
  group_by(channel_title) %>%
  summarise(total_views = sum(views, na.rm = TRUE)) %>%
  arrange(desc(total_views)) %>%
  slice_head(n = 10)

# 3.Criar um gráfico de barras *TOP 10 CHANNEL WITH MORE VIEWS.
ggplot(top_10_views, aes(x = fct_reorder(channel_title, total_views), y = total_views)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 channel_title com mais views",
       x = "channel_title",
       y = "Total de Views") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.Extrair o ano da coluna "publish_time"
youtube_us$ano_publicacao <- substr(youtube_us$publish_time, 1, 4)


# 5.Filtrar os top 5 "channel_title" com mais "views" por ano
top_5_por_ano <- youtube_us %>%
  group_by(ano_publicacao, channel_title) %>%
  summarise(total_views = sum(views, na.rm = TRUE)) %>%
  arrange(ano_publicacao, desc(total_views)) %>%
  group_by(ano_publicacao) %>%
  slice_head(n = 5)

# 6.Visualizar o resultado
print(top_5_por_ano)

# 7.Filtrar os top "channel_title" com mais "views" por ano
top_por_ano <- youtube_us %>%
  group_by(ano_publicacao, channel_title) %>%
  summarise(total_views = sum(views, na.rm = TRUE)) %>%
  arrange(ano_publicacao, desc(total_views)) %>%
  group_by(ano_publicacao) %>%
  slice_head(n = 1)

print(top_por_ano)

# 8.Tops views por ano.
ggplot(top_por_ano, aes(x = as.factor(ano_publicacao), y = total_views, fill = channel_title)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top channel_title com mais views por ano",
       x = "Ano de Publicação",
       y = "Total de Views") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(youtube_us, aes(x = ano_publicacao, y = views)) +
  geom_bar(stat = "sum", fill = "skyblue") +
  labs(title = "Quantidade de Views por Ano",
       x = "Ano de Publicação",
       y = "Total de Views") +
  theme_minimal()


