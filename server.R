  server = function(input, output) {
    #pela<-search_cinemovie(pela= input$RecomPel)
    output$Recomendacion <- renderTable({pela[[2]]}, rownames = FALSE, colnames = FALSE)
  }
  
  
  #todavía falta corregir..
