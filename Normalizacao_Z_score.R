#===========================================
#Normalização dos dados por Z-score


normalizacao.Z = function(x, nulos = 0) {
  
  #Etapa de tipagem dos dados
  if(is.data.frame(teste)) teste = teste[[1]] 
  x = as.vector(x)
  
  #Cálculo das medidas estatísticas
  media = mean(x, na.rm = TRUE)
  desvp = sd(x, na.rm = TRUE)
  
  z = (x - media)/desvp
  
  
  
  #Substituindo nulos por zeros
  z[is.na(z)] = nulos
  
  return(z)
  
}
