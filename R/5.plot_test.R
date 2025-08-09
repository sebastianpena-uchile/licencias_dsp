
# 1.cargar librerias ------------------------------------------------------








library(ggplot2)
library(scales)







# 2. basic plot -----------------------------------------------------------

#Totales


ggplot(rechazo_total_2013_2023, 
       aes(x = as.factor(anho), 
           y = `Recházase`/`TOTAL`, 
           color = cei10_familia, 
           group = cei10_familia)) +
  geom_line(size = 1.3) +
  labs(color = "Familia CIE-10") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()



#desagregado por sexo


ggplot(rechazo_2016_2023_por_sexo, 
       aes(x = anho,
           y = `Recházase` / `TOTAL`,
           color = cei10_familia,
           linetype = `Familia de diagnóstico (CIE-10)`,
           group = interaction(cei10_familia, `Familia de diagnóstico (CIE-10)`))) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  labs(color = "Familia CIE-10", linetype = "Sexo",
       x = "Año", y = "Proporción rechazadas") +
  scale_y_continuous(labels = label_percent(accuracy = 0.1, decimal.mark = ",")) +
  theme_minimal()



