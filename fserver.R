search_cinemovie <- function(cine=NULL, pela){
  cine <- ifelse(is.null(cine),"", cine)
  cine <- sapply(cine, tolower)
  pela <- sapply(pela, tolower)
  
  all <- ctfh(xmls) #se puede volver global
  pelas <- unique(all[,c(2)])  #distinct pelas
  
  all <- cbind(all,px='S/. 10.00')
  t_all <- tolower(all)
  
  cine_found <- which(t_all== cine, arr.ind=TRUE) #si encuentra devuelve con el indice de la posicion de la matriz all
  pela_found <- which(t_all== pela, arr.ind=TRUE)
  indexes <- c(intersect(cine_found[,1], pela_found[,1])) #interseca los indices pela/cine
  
  if(length(indexes)>0){
    result <- all[indexes,]
  }else {
    
    flag_cine_found <- ifelse(length(cine_found)==0, 0, 1)
    flag_pela_found <- ifelse(length(pela_found)==0, 0, 1)
    
    if(flag_cine_found==0 && flag_pela_found==1){
      #print("hay la pela en estos cines:")
      result <- all[pela_found[,1],]
    }else {
      if(flag_cine_found==1 && flag_pela_found==0){
        #print("quizas te interese lo otro de este cine")
        result <- all[cine_found[,1],]
      }else{
        #print("no te pases")
      }
    }
    
  }
  return (list(pelas,result))
}


tbf <- function(t, x, v){
  r<-rbind(t,cbind(rep(x, length(v)), v))
  r<-gsub("\t","" , gsub("\n","", r))
  r<-r[complete.cases(r),]
  return(r)
}


tbf2 <- function(t, x, v){
  if(is.na(x)==TRUE){
    ff<-matrix(nrow = 1, ncol = 3)
  } else {
    v<-v[-1]
    x<-gsub("\t","" , gsub("\n","", x))
    ff<-matrix(ncol = 3)
    l<-length(v)/2
    
    for(i in 1:l){
      vv<-strsplit(v[i*2],"m")
      vv<-gsub("\t","" , gsub("\n","", vv[[1]]))
      vv<-vv[-length(vv)]
      vv<-paste(vv,"m", sep ="")
      vv<-cbind(rep(x[1],length(vv)),rep(v[i*2-1],length(vv)),vv)
      ff<-rbind(ff, vv)
    }
    ff<-rbind(t, ff)
    ff<-ff[complete.cases(ff),]
  }
  return(ff)
}


sread_html = function(x) {
  require(rvest)
  Page.src = try(read_html(x), silent = T)
  # verificar que el source es correcto
  if (class(Page.src)[1] == "try-error") {
    error.cond = attr(Page.src, "condition")
    # verificar si error contiene la palabra time out
    timed.out = regexpr("Timed out", error.cond, ignore.case = T) != -1
    if (timed.out == TRUE) {
      #continua sólo si es error de time out
      # imprimir consola
      print(paste(x, ": Timed out. Trying to reconnect in 30s. Please wait..."))
      Sys.sleep(30)
      return(sread_html(x))
    }
  }
  return(Page.src)
}



sleepit <- function(x)   
{
  y<-runif(1, x-2, x+2) #simular tiempo
  Sys.sleep(y) #pausar R
}


scrap_fandangoperu<- function(link="https://www.fandango.lat/pe/cines"){
  fandangoperu_lvl1 <- sread_html(link)
  #guardar xml del home
  cines_links <- fandangoperu_lvl1 %>%
    html_nodes("a.movietheater__name") %>%
    html_attr('href')
  cines_links
  #guardar links de cines
  fandangoperu_lvl2<-list()
  for(i in 1:length(cines_links)){
    fandangoperu_lvl2[length(fandangoperu_lvl2)+1]<-list(sread_html(cines_links[i]))
    # sleepit(5)
  } #guardar 
  return(list(fandangoperu_lvl1, fandangoperu_lvl2))
} #ejecutar todas las mañanas una sola vez



ctfh<-function(xmls){  
  fandangoperu_lvl1 <- xmls[[1]]
  fandangoperu_lvl2 <- xmls[[2]]
  
  cine_titulo_tipo_horario<-matrix(ncol = 4) 
  cine_titulo<-matrix(ncol = 2)
  
  cines_nombres <- fandangoperu_lvl1 %>%
    html_nodes("div.item__movietheater a.movietheater__name span") %>%
    html_text()
  #obtener nombres de los cines
  
  for(j in 1:length(cines_nombres)){
    
    peliculas_titulos <- fandangoperu_lvl2[[j]] %>%
      html_nodes("div.movie-info__content a.movie-info__title h3.movie-info__title") %>%
      html_text()
    #obtener títulos de las películas
    
    cine_titulo<-tbf(cine_titulo,cines_nombres[j],peliculas_titulos) #primera tabla
    
    peliculas_horarios_nodos <- fandangoperu_lvl2[[j]] %>%
      html_nodes("div.hidden-sm-down.w-100")
    titulo_tipo_horario<-matrix(ncol = 3)
    if (length(peliculas_titulos)!=0){
      
      for(i in 1:length(peliculas_titulos)){ #longitud peliculas_titulos coincidir con peliculas_horarios_nodos   
        
        peliculas_horarios_texto <- peliculas_horarios_nodos[i] %>% 
          xml_children %>%
          xml_children %>%
          xml_children %>%
          html_text()
        #colocar funcion
        titulo_tipo_horario<- tbf2(titulo_tipo_horario, peliculas_titulos[i], peliculas_horarios_texto) 
        #print(paste("i es", i)) debug
      }
    }
    cine_titulo_tipo_horario<- rbind(cine_titulo_tipo_horario,cbind(rep(cines_nombres[j],dim(titulo_tipo_horario)[1]),titulo_tipo_horario))
    cine_titulo_tipo_horario<- cine_titulo_tipo_horario[complete.cases(cine_titulo_tipo_horario),]
    #print(paste("j es", j)) de
  }
  return(cine_titulo_tipo_horario)
}


cdfp<-function(xmls){
  fandangoperu_lvl1 <- xmls[[1]]
  fandangoperu_lvl2 <- xmls[[2]]
  cines_nombres <- fandangoperu_lvl1 %>%
    html_nodes("div.item__movietheater a.movietheater__name span") %>%
    html_text()
  dias <- c("Lunes y Miércoles", "Jueves a Domingo y Feriados", "Lunes", "Jueves a Feriados", 
            "Martes", "Miércoles", "Jueves", "Viernes", "Sabado")
  formato<-c("2D", "Sala 3D DBOX", "Sala 2D DBOX", "Sala XD 2D","3D", 
             "Sala XD 3D", "Sala XD 3D DBOX", "Sala XD 2D DBOX")
  mr<-matrix(nrow = 1, ncol = 3)
  mr2<-matrix(nrow = 1, ncol = 4)
  for(j in 1:2){
    cine_tarifas<- fandangoperu_lvl2[[j]] %>%
      html_nodes("div.accordion__body") %>%
      html_text()
    rr<-strsplit(cine_tarifas[3], "\r")
    rr<-rr[[1]]
    rr<-gsub("\t","" , gsub("\n","", rr))
    re<-case_when(
      rr %in% dias == TRUE ~ "Día",
      rr %in% formato == TRUE ~ "Formato",
      rr %in% c("") == FALSE ~ "Precio",
      TRUE ~ as.character(rr)
    )
    i<-1
    mm<-matrix(nrow = table(re)[3], ncol = 3)
    for(k in 1:length(re)){
      if(re[k] == "Día"){
        mm[i,1]<-rr[k]
        i<-i+all(!is.na(mm[i,]))*1
      } else if(re[k] == "Formato"){
        if(all(!is.na(mm[i,1]))==FALSE){ #SE EJECUTA CUANDO 1 es vacio
          mm[i,2]<-rr[k]
          mm[i,1]<-mm[i-1,1]
          i<-i+all(!is.na(mm[i,]))*1
        }  
        else if (all(!is.na(mm[i,1]))==TRUE){ 
          mm[i,2]<-rr[k]
          i<-i+all(!is.na(mm[i,]))*1
        }
        i<-i+all(!is.na(mm[i,]))*1
      } else if(re[k] == "Precio"){
        if(all(!is.na(mm[i,c(1,2)]))==TRUE){
          mm[i,3]<-rr[k]
          i<-i+all(!is.na(mm[i,]))*1
        }
        else if(all(!is.na(mm[i,c(1,2)]))==FALSE){
          mm[i,1]<-mm[i-1,1]
          mm[i,2]<-mm[i-1,2]
          mm[i,3]<-rr[k]
          i<-i+all(!is.na(mm[i,]))*1
        }
      }
    }
    mr<-rbind(mr,mm)
    mr<-mr[complete.cases(mr),]
    mr2<-rbind(mr2, cbind(rep(cines_nombres[j]),mr))
    mr2<-mr2[complete.cases(mr2),]
  }
  return(mr2)
}






