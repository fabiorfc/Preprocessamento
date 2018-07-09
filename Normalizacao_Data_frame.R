#===========================================

#Normalização de um data.frame
normalizacao.Data.Frame = function(df, tipo = 'Max-Min') {
  
  #Verificação da tipagem do objeto
  if(!is.data.frame(df)) {stop('O objeto inserido deve ser um data.frame')}
  
  #Seleção da normalização que será utilizada
  if(tipo == 'Max-Min') {funcao = normalizacao.Max.Min
  } else if (tipo == 'Z-score') {funcao = normalizacao.Z
  } else {stop('Os tipos são Max-Min e Z-score')} 
  
  #Iteração do data.frame para normalizar os dados
  df.normalizado = vector(length = nrow(df)) #Adicionando uma coluna de nulos para definir a dimensão do data frame
  vet.normalizado = NULL; vet.nomes = NULL
  for(i in 1:ncol(df)){
    
    #Cálculo dos valores normalizados
    vet.normalizado = funcao(df[[i]])
    df.normalizado = cbind(df.normalizado, vet.normalizado)
    #Atribuição dos nomes das labels
    nomes = names(df[i])
    vet.nomes = c(vet.nomes, nomes)
    
  } #Fim da iteração

  #Conversão do objeto de lista para data.frame
  df.normalizado = as.data.frame(df.normalizado)
  df.normalizado[1] = NULL #Removendo a coluna de Nulos
  colnames(df.normalizado) = vet.nomes
  
  
  #return(list(df.normalizado,vet.nomes))
  return(df.normalizado)
  
} 
