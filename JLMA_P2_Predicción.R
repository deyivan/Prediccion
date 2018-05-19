###############################################################
##    P r á c t i c a   B l o q u e  2   ( P r e d i c c i ó n )
###############################################################
##    R e g r e s i ó n    L i n e a l    M ú l t i p l e
###############################################################
##    A u t o r :   J o s é   L u i s   M u ñ o z   A m p u e r o
##    D e s a r r o l l o   d e   S i s .   I n t e l i g e n t e s
##    M á s t e r   e n   I n g e n i e r í a   I n f o r m á t i c a
##    ( E S I - C i u d a d   R e a l ,   U C L M )
###############################################################

###############################################################
##    F u n c i o n e s   a u x i l i a r e s
###############################################################
toN <- function(x) { x[x<0] <- 0; round(x) } # Establece a cero los negativos y redondea el resto

##############################################################
##    C a r g a   d e   l i b r e r í a s   e x t e r n a s
##############################################################
library(MASS)
library(readxl)

##############################################################
##    C a r g a   d e l   C o n j u n t o   d e   D a t o s   
##              d e   E n t r e n a m i e n t o
##############################################################
Training_Set <- read_excel("Data/DengAI_Predicting_Disease_Spread_-_Training_Data_Features_and_Labels.xlsx", 
                           sheet = "Training Set",
                           col_types = c("text", "numeric", "numeric", "date", "numeric",
                                         "numeric", "numeric", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric", "numeric"))

##############################################################
##    A j u s t e   d e l   m o d e l o   
##############################################################
lm_S <- lm(formula = total_cases ~ ndvi_ne + ndvi_nw + ndvi_se + ndvi_sw + 
             precipitation_amt_mm + reanalysis_air_temp_k + reanalysis_avg_temp_k + 
             reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + reanalysis_min_air_temp_k +
             reanalysis_relative_humidity_percent + reanalysis_specific_humidity_g_per_kg + 
             reanalysis_tdtr_k,
          data = Training_Set)

##############################################################
##    S e l e c  c i ó n   a u t o m á t i c a   d e   l o s
## r e g r e s o r e s   q u e   m i n i m i z a n   e l   A I C    
##############################################################
step(lm_S, direction = c("backward"))

##############################################################
##    A j u s t e   d e l   m o d e l o   a n t e r i o r
##############################################################
lm_AIC <- lm(formula = total_cases ~ reanalysis_avg_temp_k + reanalysis_max_air_temp_k + 
              reanalysis_relative_humidity_percent + reanalysis_specific_humidity_g_per_kg + 
              reanalysis_tdtr_k, 
             data = Training_Set)
summary(lm_AIC)

##############################################################
##    C a r g a   d e l   C o n j u n t o   d e   D a t o s   
##                      d e   T e s t
##############################################################
Predicting_Disease_Spread <- read_excel("Data/DengAI_Predicting_Disease_Spread_-_Training_Data_Features_and_Labels.xlsx", 
                                        sheet = "Predicting Disease Spread",
                                        col_types = c("text", "numeric", "numeric", "date", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric"))

##############################################################
##    P r e d i c c i ó n   d e   l o s   c a s o s   t o t a l e s   
##############################################################
pred_AIC <- predict(lm_AIC, Predicting_Disease_Spread, 
                    type="response", se.fit=TRUE)

##############################################################
##    E x t r a c c i ó n   d e   I n f o   r e l e v a n t e   
##############################################################
Predicting_Disease_Spread$fit_AIC <- pred_AIC$fit # Valor estimado
Predicting_Disease_Spread$se_AIC <- pred_AIC$se.fit # Valor estimado del error
Predicting_Disease_Spread$total_cases <- toN(pred_AIC$fit) # Valores estimado (dominio de los números Naturales)

##############################################################
##    E s c r i t u r  a   F i c h e r o   C o m p e t i c i ó n   
##############################################################
write.table(Predicting_Disease_Spread[c("city","year","weekofyear","total_cases")], # Extracción de las columnas a escribir
            file = paste("Data/",                                       # Ruta relativa a carpeta donde almacenar el fichero
                         format(Sys.time(), format = "%Y%m%d-%H%M%S_"), # Se añade un prefijo basado en la fecha de generación al fichero
                         "Predicting_Disease_Spread.csv",               # Nombre del fichero
                         sep = ""),                                     # Sin caracter separador en la concatenación anterior
            row.names = FALSE,                                          # Fichero .csv sin número de líneas
            sep = ",")                                                  # Fichero .csv separado por comas