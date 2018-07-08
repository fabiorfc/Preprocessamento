#===========================================

#Normalização min - max

normalizacao.Max.Min = function(x, nulos = 0) {
  
  #Etapa de tipagem dos dados
  if(is.data.frame(x)) x = x[[1]] 
  x = as.vector(x)
  
  #Cálculo das medidas estatísticas
  maxim = max(x, na.rm = TRUE)
  minim = min(x, na.rm = TRUE)
  
  z = (x - minim)/(maxim - minim)
  
  #Substituindo nulos por zeros
  z[is.na(z)] = nulos
  
  return(z)
  
}
