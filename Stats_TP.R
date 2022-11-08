Air<-read.delim2("http://tinyurl.com/y39an7ef/DATA86435.csv",na.strings="-",skipNul = TRUE)

options( "digits"=3) 


colnames(Air) <- c("Date",
                   "Lyon-A7_Azote","Lyon-A7_PM10",
                   "Villeurbanne_Azote","Villeurbanne_PM10",
                   "Saint-Exupery_Azote","Saint-Exupery_Ozone","Saint-Exupery_PM10",
                   "Lyon-Centre_Azote","Lyon-Centre_Ozone","Lyon-Centre_PM10",
                   "Croix-Rousse_Azote","Croix-Rousse_PM10",
                   "Peripherique_Azote","Peripherique_PM10",
                   "Place-Jaures_Azote","Place-Jaures_PM10")



##---------------------------------EXERCICE 1-----------------------------------


##  A) Infromations G?n?rales



##    2a. Quel est le nombre d'heures d'observation de l'?chantillon ?

        nb_observation_echantillon = nrow(Air)

##    2b. Quel est le nombre d'heures o? les particules ont ?t? mesur?es dans toutes les stations ?

        ## Nouveau Tableau des positions des valeurs non observ?es
        Tab <- ( c(which(is.na(Air$`Lyon-A7_Azote`)), 
                   which(is.na(Air$`Lyon-A7_PM10`)),
                   which(is.na(Air$Villeurbanne_Azote)),
                   which(is.na(Air$Villeurbanne_Azote)),
                   which(is.na(Air$Villeurbanne_PM10)),
                   which(is.na(Air$`Saint-Exupery_Azote`)),
                   which(is.na(Air$`Saint-Exupery_Ozone`)),
                   which(is.na(Air$`Saint-Exupery_PM10`)),
                   which(is.na(Air$`Lyon-Centre_Azote`)),
                   which(is.na(Air$`Lyon-Centre_Ozone`)),
                   which(is.na(Air$`Lyon-Centre_PM10`)),
                   which(is.na(Air$`Croix-Rousse_Azote`)),
                   which(is.na(Air$`Croix-Rousse_PM10`)),
                   which(is.na(Air$Peripherique_Azote)),
                   which(is.na(Air$Peripherique_PM10)),
                   which(is.na(Air$`Place-Jaures_Azote`)),
                   which(is.na(Air$`Place-Jaures_PM10`))
                   ))

        ##Rangement du tableau par odre croissant
        Tab <- sort(Tab)
        
        ##Rempalcement des doubles par la valeur : NA
        for (i in 1:(length(Tab)-1)) {
          if (Tab[i] == Tab[i+1]) { Tab[i] <- NA }
          i = i+1
        }
        
        ##Rangement du tableau par odre croissant (qui supprime par la m?me occasion les valeurs NA)
        Tab <- sort(Tab)
        
        ##Calcul du nombre d'heures
        nb_observation_all_stations = nb_observation_echantillon - length(Tab)
   
        
             
    
##    3.
      
##      a. Calcul : PM10
       
           ## Nouveau Tableau des positions des valeurs non observ?es
           PM10 <- ( c(which(is.na(Air$`Lyon-A7_PM10`)),
                       which(is.na(Air$Villeurbanne_PM10)),
                       which(is.na(Air$`Saint-Exupery_PM10`)),
                       which(is.na(Air$`Lyon-Centre_PM10`)),
                       which(is.na(Air$`Croix-Rousse_PM10`)),
                       which(is.na(Air$Peripherique_PM10)),
                       which(is.na(Air$`Place-Jaures_PM10`))  ))
           
           ##Rangement du tableau par odre croissant
           PM10 <- sort(PM10)
           
           ##Rempalcement des doubles par la valeur : NA 
           for (i in 1:(length(PM10)-1)) {
              if (PM10[i] == PM10[i+1]) { PM10[i] <- NA }
              i = i+1
           }
           
           ##Rangement du tableau par odre croissant (qui supprime par la m?me occasion les valeurs NA)
           PM10 <- sort(PM10)
           
           
           ##D?claration du Tableau PM10Obs
           PM10obs <- TRUE
           
           ## Appliquer TRUE par d?faut ? toute les valeurs du tableau
           for (j in 1:nb_observation_echantillon) {
              PM10obs[j]<- TRUE
              j = j+1
           }
           
           # Appliquer FALSE ? toutes les positions des valeurs non observ?s obtenu plus haut
           for (k in 1:length(PM10)) {
              m <- PM10[k]
              PM10obs[m]<- FALSE
           }
      
           
           
##      b. Calcul : Azote
           
          ## Nouveau Tableau des positions des valeurs non observ?es
          Azote <- ( c(which(is.na(Air$`Lyon-A7_Azote`)), 
                     which(is.na(Air$Villeurbanne_Azote)),
                     which(is.na(Air$Villeurbanne_Azote)),
                     which(is.na(Air$`Saint-Exupery_Azote`)),
                     which(is.na(Air$`Lyon-Centre_Azote`)),
                     which(is.na(Air$`Croix-Rousse_Azote`)),
                     which(is.na(Air$Peripherique_Azote)),
                     which(is.na(Air$`Place-Jaures_Azote`))  ))
          
          ##Rangement du tableau par odre croissant
          Azote <- sort(Azote)
          
          ##Rempalcement des doubles par la valeur : NA 
          for (i in 1:(length(Azote)-1)) {
            if (Azote[i] == Azote[i+1]) { Azote[i] <- NA }
            i = i+1
          }
          
          ##Rangement du tableau par odre croissant (qui supprime par la m?me occasion les valeurs NA)
          Azote <- sort(Azote)
      
  
          ##D?claration du Tableau AzoteObs
          Azoteobs<- TRUE
          
          ## Appliquer TRUE par d?faut ? toute les valeurs du tableau
          for (j in 1:nb_observation_echantillon) {
            Azoteobs[j]<- TRUE
            j = j+1
          }
          
          # Appliquer FALSE ? toutes les positions des valeurs non observ?s obtenu plus haut
          for (k in 1:length(Azote)) {
            m <- Azote[k]
            Azoteobs[m]<- FALSE
          }

      
          ## Table de contigence des 2 variables nominales
          print(table(PM10obs , Azoteobs))

      
      
  ##    4.

      
          ##Calcul de la moyenne empirique
          mean(na.omit(Air$`Lyon-Centre_Ozone`))
          
          ##Calucl des quartiles 
          quantile(na.omit(Air$`Lyon-Centre_Ozone`), type = 6)
          
          ## il aurait ?galement ?t? possible d'utiliser 
          summary(na.omit(Air$`Lyon-Centre_Ozone`), type=7)
          
          
          
          ##Calcul de la Variance Empirique
          variance = var(na.omit(Air$`Lyon-Centre_Ozone`))
          
          #Calcul de la Variance Empirique Non Biais?
          variance_NB = variance*(nrow(Air)/(nrow(Air)-1))
    

      
      
      
      
##    5. 
          
        ## Initialisation du dataframe dft
        dft = data.frame(
          LyonA7_Azote = character(),
          LyonA7_PM10 = character(),
          Villeurbanne_Azote = character(),
          Villeurbanne_PM10 = character(),
          SE_Azote = character(),
          SE_Ozone = character(),
          SE_PM10 = character(),
          LyonCentre_Azote = character(),
          LyonCentre_Ozone = character(),
          LyonCentre_PM10 = character(),
          CroixRousse_Azote = character(),
          CroixRousse_PM10 = character(),
          Peripherique_Azote = character(),
          Peripherique_PM10 = character(),
          PlaceJaures_Azote = character(),
          PlaceJaures_PM10 = character(),
          stringsAsFactors=FALSE
        )
          
        #Initialisation de la variable moyenne
        moyenne=0
        #Initialisation de la variable de calucl des lignes 
        r=0
        
        ##Calcul des moyennes de 6H en 6H
        for (i in 1:16) {
          for (k in 1:(as.integer(nrow(Air)/6)) ) {
            moyenne = mean(na.omit(c(Air[r+1,i+1],
                                     Air[r+2,i+1],
                                     Air[r+3,i+1],
                                     Air[r+4,i+1],
                                     Air[r+5,i+1],
                                     Air[r+6,i+1])))
            if (is.nan(moyenne) == TRUE) {moyenne = NA}
            dft[k,i] = round(moyenne, digits = 3) ## incr?menter la moyenne arrondi dans le dataframe
            moyenne = 0
            r = r+6
          }
          r =0
        }
        
        
        ## Suppression des mesures avec des tranches encore non observ?s
        for (i in 16:1) {
          if (length(which(is.na(dft[,i]))) >= 1  ) {
            
            dft[,i] = NULL
          }
        }
      
    
    
##  B) Corr?latione t R?gression lin?aire
        
        
        ## Initialisation de toutes les valeurs en valeurs num?riques pour r?gler l'error :
        ## Error in cov(dft) : is.numeric(x) || is.logical(x) n'est pas TRUE
  
        dft$LyonA7_Azote =  as.numeric(dft$LyonA7_Azote)
        dft$LyonA7_PM10 = as.numeric(dft$LyonA7_PM10)
        dft$Villeurbanne_Azote = as.numeric(dft$Villeurbanne_Azote )
        dft$Villeurbanne_PM10 = as.numeric(dft$Villeurbanne_PM10)
        dft$SE_Ozone = as.numeric(dft$SE_Ozone)
        dft$SE_PM10 = as.numeric(dft$SE_PM10)
        dft$LyonCentre_Azote = as.numeric(dft$LyonCentre_Azote)
        dft$LyonCentre_Ozone = as.numeric(dft$LyonCentre_Ozone)
        dft$CroixRousse_Azote = as.numeric(dft$CroixRousse_Azote)
        dft$CroixRousse_PM10 = as.numeric(dft$CroixRousse_PM10)
        dft$Peripherique_Azote = as.numeric(dft$Peripherique_Azote)
        dft$PlaceJaures_PM10 = as.numeric(dft$PlaceJaures_PM10) 
        
        
##    1.
        
        ## Calcul de covariance
        cov <- cov(dft)
        
        ## Calcul de corr?lation
        cor <- cor(dft)


##    2.
        
        ## D?claration de y et x
        y <- dft$CroixRousse_Azote 
        x <- dft$LyonA7_Azote
        
        ## Test de corr?lation sous la m?thode pearson
        cor.test(x, y, method = c("pearson"))


##    3.

        lmyx <- lm(y~x)

        summary(lmyx)
        
        
        ## Tracage du Nuage de Points
        plot(y~x,
             col = rep(1:4), 
             pch = 19, 
             main = "Nuage de Points et Droite de regression des mesures par tranches de 6 heures",
             xlab = "Mesure Dioxyde d'Azote Lyon A7 (en \U00B5g/m3)",
             ylab = "Mesure Dioxyde d'Azote Croix Rousse (en \U00B5g/m3)")
        legend("bottomright", legend = c("Nuit","Matin","Apres Midi","Soir"), col = 1:4, pch = 19, bty = "n",text.font=4)

        ## Tracage de la droite de r?fgression lin?aire
        abline(lmyx,col="orange", lwd=3, lty=2)
        

        
        
##     4.
        
        x2 <- rep(200,length.out = 124)
        t.test(x2,y,conf.level = 0.95)
        
      
##     5.
        
        ## Installation de EnvStats pour utiliser gofTest 
        ##install.packages("EnvStats")
        library(EnvStats)
        
        ## Test du khi 2 de normalit? pour x
        gofTest(y,distribution="norm",test="chisq")
        
        ## Test du khi 2 de normalit? pour y
        gofTest(x,distribution="norm",test="chisq")
        
        
        ## calcul de z et z' = lm(z)
        z <- dft$LyonA7_PM10
        zprime <- log(z)
        
        ## Test du khi 2 de normalit? pour z
        gofTest(z,distribution="norm",test="chisq")
        
        ## Test du khi 2 de normalit? pour z'
        gofTest(zprime,distribution="norm",test="chisq")
        
        
        
##   6.
        
        lmxzprime <- lm(x~zprime)
        summary(lmxzprime)
        
        ## Calcul des intervalles de pr?dictions au niveau 80%
        x_zprime <- data.frame(x,zprime)
        Interval <- predict(lmxzprime,x_zprime, interval="prediction",level = 0.80)
        
        data<-cbind(x_zprime ,Interval)
        
        ## Installation de ggplot2 pour utiliser ggplot 
        ##install.packages("ggplot2")
        library("ggplot2")
        
        #L?gende pour les couleurs des tranches de 6H
        Couleurs = rep(c("Nuit","Matin","Apr?s Midi","Soir"), length.out = 124)
        
        ## Cr?ation du Nuage des points avec droit de r?grssion et Intervalles
        Graphique<-ggplot(data ,aes(zprime, x, color = Couleurs)) +
                   geom_point(size = 3, alpha = 0.6) +
                   geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1.2)+
                   geom_line(aes(y = lwr), color = "red", linetype = "dashed", size = 1.1)+
                   geom_line(aes(y = upr), color = "red", linetype = "dashed", size = 1.1)+
                   ggtitle("Nuage de Points, Droite de regression et Bornes d'intervalle de prediction a 80%)")+
                   xlab("Mesure des particules PM10 sur l'A7 (10 micrometres)")+
                   ylab ("Mesure Dioxyde d'Azote sur l'A7 (en \U00B5g/m3)") +
                   theme(legend.position="right")
        
        ##Affichage du Graphique           
        print(Graphique)
        
        
        
##     7.
        
        Graphique2<-ggplot(data ,aes(zprime, x, color = Couleurs)) +
          geom_point(size = 3, alpha = 0.6) +
          geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1.2)+
          geom_line(aes(y = lwr), color = "red", linetype = "dashed", size = 1.1)+
          geom_line(aes(y = upr), color = "red", linetype = "dashed", size = 1.1)+
          ggtitle("Nuage de Points, Droite de r?gression et Bornes d'intervalle de pr?diction ? 80%)")+
          xlab("Mesure des particules PM10 sur l'A7 (10 microm?tres)")+
          ylab ("Mesure Dioxyde d'Azote sur l'A7 (en ??g/m3)") +
          theme(legend.position="right")
        
        Graphique2
        
        k1 = 1
        k2 = 1
        x7 = 0
        y7 = 0
        zx = 0
        zy = 0
        
        for ( i in 1:nrow(data)) {
        
        if ( (data$x[i] > 50) && (data$x[i] < 80)) {
          x7[k1] <- data$x[i]
          zx[k1] <- z[i]
          k1 = k1+1
        }
        
        if (data$x[i] >= 80) {
          y7[k2] <- data$x[i]
          zy[k2] <- z[i]
          k2 = k2+1
        }
        }
        
        k1 = 0
        k2 = 0
        
        
        lmx7 <- lm(x7~log(zx))
        lmy7 <- lm(y7~log(zy))
        
        datax <- data.frame(x7,zx)
        datay <- data.frame(y7,zy)

        ## Calcul des intervalles de pr?dictions au niveau 95%
        Interval2 <- predict(lmx7,datax, interval="prediction",level = 0.95)
        
        Interval3 <- predict(lmy7,datay, interval="prediction",level = 0.95)
        
        
    
##---------------------------------EXERCICE 2-----------------------------------

        
##      1.
        ## Simulation d'une loi Uniforme sur [-5,2] de taile N = 10000
        U=runif(10000,min=-5,max = 2)
        
        ##Construction du vecteur V demsand?
        V=ifelse(U>0, returnValue(U) ,NA)
        V = na.omit(V)

        ## Choix d'une densit?
        dv = 1/2+5
        density(U)
        density.default(U)
        
        ## Cr?ation et Affichage de l'histogramme
        hist(V,freq = FALSE, 
             main="Histogramme associ? ? V avec la densit? d'une loi Uniform", 
             xlab = "Valeurs", 
             ylab = "Densit?",
             col="lightblue",
             border="white")
        lines(density(U),col='blue',lty = 1, lwd = 5) 
      
        
##      2.
        r = 0
        ##for (i in 0:100 ) {
          ##r[i] = chisq.test(V,V)
        ##}

        
##      3.
        U <- runif (10000 ,min=-1,max=1); V <- runif (10000 ,min=0,max=1) 
        S <- (1>abs (U)+V)
        U0 <- U[ S ] ; V0 <- V[ S ]
        
        #Tracage du nuage de points de l'?chnatillon (U,V)
        plot(U,V,pch='.', main = "Nuage de points de l'?chantillon", col="red")
   
             
##      4. 
        #Tracage du nuage de points de l'?chnatillon (U0,V0)
        plot(U0,V0,pch='.', main = "Nuage de points de l'?chantillon", col="red")

                
##      5. 
        #Tracage de l'histogramme de comparaison avec la densit?
        densite <- density(U0)
        hist(U0,freq = FALSE,col = "lightblue",border="white", main = "Comparaison entre l'histogramme et la densit?", ylab="Densit?")
        lines(densite, col='blue',lty = 1, lwd = 5) 
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        