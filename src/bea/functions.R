
generar_indice <- function(serie,fecha, fecha_base){
  
  valor_base <- serie[which(fecha==fecha_base)]
  
  # Check if valor_base is empty
  if (length(valor_base) == 0) {
    warning("No matching date found for fecha_base. Returning NA.")
    return(NA)
  }
  
  return (serie/valor_base)
}