  # Instalar pacotes necessários
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    install.packages("DiagrammeR")
  }
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Carregar pacotes
library(plotly)
library(ggplot2)

# Dados de entrada
# Comprimento da viga (em metros)
L <- 10 

# Carga distribuída (kN/m)
w <- 5

# Carga pontual (kN) e posição da carga (m a partir do início da viga)
P <- 20
P_pos <- 7

# Suportes (viga simplesmente apoiada)
support_A <- 0
support_B <- L

# Cálculo das reações nos apoios
RA <- (P * (L - P_pos) + w * L * (L / 2)) / L
RB <- (P * P_pos + w * L * (L / 2)) / L

cat("Reação no apoio A (RA):", RA, "kN\n")
cat("Reação no apoio B (RB):", RB, "kN\n")

# Função para cálculo da força cortante ao longo da viga
shear_force <- function(x) {
  if (x < P_pos) {
    return(RA - w * x)
  } else {
    return(RA - w * x - P)
  }
}

# Função para cálculo do momento fletor ao longo da viga
bending_moment <- function(x) {
  if (x < P_pos) {
    return(RA * x - (w * x^2) / 2)
  } else {
    return(RA * x - (w * x^2) / 2 - P * (x - P_pos))
  }
}

# Função para determinar a deformada (aproximação simplificada)
deflection <- function(x, E, I) {
  if (x < P_pos) {
    return((RA * x^3) / (6 * E * I) - (w * x^4) / (24 * E * I))
  } else {
    return((RA * x^3) / (6 * E * I) - (w * x^4) / (24 * E * I) - (P * (x - P_pos)^3) / (6 * E * I))
  }
}

# Função para calcular tensões normais máximas em seções transversais
stress <- function(moment, c, I) {
  return((moment * c) / I)
}

# Parâmetros do material
E <- 210000  # Módulo de elasticidade (MPa)
I <- 0.0001  # Momento de inércia (m^4)
c <- 0.05     # Distância do centroide à fibra mais externa (m)

# Gerar valores para o comprimento da viga
x_vals <- seq(0, L, length.out = 100)

# Calcular esforços
shear_vals <- sapply(x_vals, shear_force)
moment_vals <- sapply(x_vals, bending_moment)
deflection_vals <- sapply(x_vals, deflection, E = E, I = I)
stress_vals <- sapply(moment_vals, stress, c = c, I = I)

# Plotar os resultados interativamente
shear_plot <- plot_ly(x = x_vals, y = shear_vals, type = 'scatter', mode = 'lines', 
                      name = 'Força Cortante', line = list(color = 'blue')) %>%
  layout(title = "Diagrama de Força Cortante",
         xaxis = list(title = "Comprimento da Viga (m)"),
         yaxis = list(title = "Força Cortante (kN)"))

moment_plot <- plot_ly(x = x_vals, y = moment_vals, type = 'scatter', mode = 'lines', 
                       name = 'Momento Fletor', line = list(color = 'red')) %>%
  layout(title = "Diagrama de Momento Fletor",
         xaxis = list(title = "Comprimento da Viga (m)"),
         yaxis = list(title = "Momento Fletor (kN.m)"))

deflection_plot <- plot_ly(x = x_vals, y = deflection_vals, type = 'scatter', mode = 'lines', 
                           name = 'Deformada', line = list(color = 'green')) %>%
  layout(title = "Deformada da Viga",
         xaxis = list(title = "Comprimento da Viga (m)"),
         yaxis = list(title = "Deformada (m)", zeroline = TRUE))

stress_plot <- plot_ly(x = x_vals, y = stress_vals, type = 'scatter', mode = 'lines', 
                       name = 'Tensão Normal', line = list(color = 'purple')) %>%
  layout(title = "Tensão Normal Máxima",
         xaxis = list(title = "Comprimento da Viga (m)"),
         yaxis = list(title = "Tensão (MPa)"))

# Plot final de todos os diagramas juntos
combined_plot <- ggplot() +
  geom_line(aes(x = x_vals, y = shear_vals, color = "Força Cortante")) +
  geom_line(aes(x = x_vals, y = moment_vals, color = "Momento Fletor")) +
  geom_line(aes(x = x_vals, y = deflection_vals * 1000, color = "Deformada x1000")) +
  geom_line(aes(x = x_vals, y = stress_vals, color = "Tensão Normal")) +
  labs(title = "Diagramas Combinados",
       x = "Comprimento da Viga (m)",
       y = "Valores") +
  scale_color_manual(values = c("blue", "red", "green", "purple"),
                     name = "Componentes",
                     labels = c("Força Cortante", "Momento Fletor", "Deformada x1000", "Tensão Normal")) +
  theme_minimal()

# Exibir plots interativos
shear_plot
moment_plot
deflection_plot
stress_plot

# Exibir o grafico combinado
print(combined_plot)
