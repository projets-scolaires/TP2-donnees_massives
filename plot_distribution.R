library(tidyverse)


plot_distribution <- function(data, variable, title = "Distribution d'une série ",couleur="grey")
  
  #Création du graphique
  
  ggplot(data, aes_string(x = variable)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.3,         # Largeur des barres
                 fill = couleur,           #Couleur des barres
                 alpha = 0.6,            #Transparence des barres
                 color = "grey" ) +      #Bordure des barres
  geom_density(color = "black", size = 1) + #Courbe de densité
  labs(title = title, x = variable, y = "Densite") + #Titre et étiquettes des axes
  theme_minimal()                         #Thème visuel simple

plot_distribution(iris, "Sepal.Width")