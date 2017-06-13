  server = function(input, output) {
    source("C:/Users/SANDRO/Documents/shiny_blutaca/fserver.R"),  #reemplazar ruta
    output$Recomendacion <- renderTable({pela[[2]]}, rownames = FALSE, colnames = FALSE)
  }
