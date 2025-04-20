library(tidyverse)
outliers <- function(dataset) {
  # On enleve  les valeurs manquantes pour éviter des erreurs
  data <- na.omit(dataset)
  if(is.numeric(dataset)){
    
    Q1 <- quantile(data, 0.25)
    Q3 <- quantile(data, 0.75)
    IQR <- Q3-Q1
    
    # Calcul des bornes
    borne_inf <- Q1 - 1.5 * IQR
    borne_sup <- Q3 + 1.5 * IQR
    
    # Identification des outliers
    extremes <- data[data < borne_inf | data > borne_sup]
    #print(outliers)
    df <- data.frame(outliers = extremes)
    resultat <- df %>%
      count(outliers, name = "occurences")
    
    return(t(resultat)) #on retourne la transposee
  }else{
    
  }
}

# Test avec la variable x
set.seed(42) #pour la reproductibilité des resultats
x <- c(-5, rnorm(100), 6)
valeurs_extremes <- outliers(x)
print("valeurs extremes : ") 
valeurs_extremes
