### TP-NOTÉ MATH-STATS `14/20`

## UE-MAT2071L Statistiques Pour L'Informatique

DABROWSKI YOANN

### Authors

- Allan DE CLERCQ - L2 INFORMATIQUE
- Salah Eddine AIT ALLAOUA – L2 INFORMATIQUE

## 1. Statistiques

### A. Informations Generales

#### Exercice 1

Le type de variable statistique est quantitative continue. \

#### Exercice 2

Le nombre d’heure d’observation de l’échantillon est de 745 h.

```r
nb_observation_echantillon = nrow(Air)
```

Le nombre d’heure où les particules ont été mesurées dans toutes les stations  est de 486 h.

```r
    Tab <-  ( c(which(is.na(Air$`Lyon-A7_Azote`)), 
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
```

#### Exercice 3

Le nombre d’heures où l’azote et les particules ont été observés dans toutes les stations est 486 h .

```r
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
```

Table de contingence :
|  PM10obs/Azoteobs | FALSE  | TRUE |
| :------------: | :------------: | :------------: |
| FALSE  | `198`  | `48` |
| TRUE  |  `13` | `486` |

#### Exercice 4

Non l’émission d’Ozone n’a pas dépassé le seuil d’information.

- Moyenne empirique : `24`
- Variance empirique : `452.795`
- Variance empirique NB : `453.403`
- Quartile a 25% : `3.9`

```r
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
```

#### Exercice 5

```r
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
```

### B. Correlation et Regression Lineaire

#### Exercice 1

```r
    ## Calcul de covariance
    cov <- cov(dft)
    
    ## Calcul de corr?lation
    cor <- cor(dft)
```

Correlation :

```
                   LyonA7_Azote LyonA7_PM10 Villeurbanne_Azote Villeurbanne_PM10 SE_Ozone SE_PM10 LyonCentre_Azote LyonCentre_Ozone CroixRousse_Azote CroixRousse_PM10 Peripherique_Azote PlaceJaures_PM10
LyonA7_Azote              1.000       0.582              0.590             0.246   -0.249  0.1060            0.579           -0.432            0.7005            0.440              0.743            0.345
LyonA7_PM10               0.582       1.000              0.566             0.797   -0.659  0.6908            0.615           -0.700            0.4457            0.775              0.571            0.865
Villeurbanne_Azote        0.590       0.566              1.000             0.555   -0.556  0.2963            0.947           -0.698            0.7066            0.449              0.768            0.544
Villeurbanne_PM10         0.246       0.797              0.555             1.000   -0.723  0.7441            0.586           -0.681            0.3120            0.744              0.385            0.864
SE_Ozone                 -0.249      -0.659             -0.556            -0.723    1.000 -0.7131           -0.609            0.873           -0.3004           -0.660             -0.540           -0.792
SE_PM10                   0.106       0.691              0.296             0.744   -0.713  1.0000            0.282           -0.549            0.0622            0.673              0.314            0.747
LyonCentre_Azote          0.579       0.615              0.947             0.586   -0.609  0.2818            1.000           -0.789            0.6774            0.484              0.725            0.619
LyonCentre_Ozone         -0.432      -0.700             -0.698            -0.681    0.873 -0.5491           -0.789            1.000           -0.4370           -0.656             -0.585           -0.762
CroixRousse_Azote         0.700       0.446              0.707             0.312   -0.300  0.0622            0.677           -0.437            1.0000            0.523              0.634            0.400
CroixRousse_PM10          0.440       0.775              0.449             0.744   -0.660  0.6728            0.484           -0.656            0.5232            1.000              0.425            0.856
Peripherique_Azote        0.743       0.571              0.768             0.385   -0.540  0.3138            0.725           -0.585            0.6344            0.425              1.000            0.467
PlaceJaures_PM10          0.345       0.865              0.544             0.864   -0.792  0.7465            0.619           -0.762            0.4005            0.856              0.467            1.000
```

Covariance :

```
                   LyonA7_Azote LyonA7_PM10 Villeurbanne_Azote Villeurbanne_PM10 SE_Ozone SE_PM10 LyonCentre_Azote LyonCentre_Ozone CroixRousse_Azote CroixRousse_PM10 Peripherique_Azote PlaceJaures_PM10
LyonA7_Azote              607.8       205.9                296              78.4     -119    22.5            245.6           -216.5             448.0              137              533.2             94.9
LyonA7_PM10               205.9       206.0                166             147.9     -184    85.5            151.9           -204.2             166.0              140              238.6            138.4
Villeurbanne_Azote        296.3       165.5                415             146.1     -220    52.0            332.0           -289.1             373.3              115              454.9            123.5
Villeurbanne_PM10          78.4       147.9                146             166.9     -182    82.9            130.4           -178.8             104.6              121              144.9            124.4
SE_Ozone                 -119.5      -184.0               -220            -181.6      378  -119.5           -203.9            345.3            -151.6             -162             -305.6           -171.6
SE_PM10                    22.5        85.5                 52              82.9     -120    74.3             41.8            -96.3              13.9               73               78.7             71.7
LyonCentre_Azote          245.6       151.9                332             130.4     -204    41.8            296.3           -276.4             302.5              105              363.4            118.8
LyonCentre_Ozone         -216.5      -204.2               -289            -178.8      345   -96.3           -276.4            413.6            -230.6             -168             -346.1           -172.6
CroixRousse_Azote         448.0       166.0                373             104.6     -152    13.9            302.5           -230.6             673.1              171              479.0            115.8
CroixRousse_PM10          136.5       140.0                115             121.0     -162    73.0            105.0           -167.9             171.0              159              155.7            120.1
Peripherique_Azote        533.2       238.6                455             144.9     -306    78.7            363.4           -346.1             479.0              156              846.8            151.4
PlaceJaures_PM10           94.9       138.4                123             124.4     -172    71.7            118.8           -172.6             115.8              120              151.4            124.2
```

#### Exercice 2

Hypothèse initiale : les mesures du dioxyde d’azote ont la croix rousse et sur l’A7 ne sont pas corrélé.

```r
    ## D?claration de y et x
    y <- dft$CroixRousse_Azote 
    x <- dft$LyonA7_Azote
    
    ## Test de corr?lation sous la m?thode pearson
    cor.test(x, y, method = c("pearson"))
```

```
        Pearson's product-moment correlation

    data:  x and y
    t = 11, df = 122, p-value <2e-16
    alternative hypothesis: true correlation is not equal to 0
    95 percent confidence interval:
    0.598 0.780
    sample estimates:
    cor 
    0.7 
```

`P-value < 2e-16` donc on rejette l’hypothèse initiale avec une forte présomption contre les mesures de la C-R et de l’A7 sont fortement corrélé

#### Exercice 3

```r
    lmyx <- lm(y~x)

    summary(lmyx)
```

```
    Call:
    lm(formula = y ~ x)

    Residuals:
    Min     1Q Median     3Q    Max 
    -35.35 -11.89  -0.63   8.69  69.28 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   34.881      4.326    8.06  5.8e-13 ***
    x              0.737      0.068   10.84  < 2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    Residual standard error: 18.6 on 122 degrees of freedom
    Multiple R-squared:  0.491, Adjusted R-squared:  0.486 
    F-statistic:  118 on 1 and 122 DF,  p-value: <2e-16
```

La droite de régression `aX+b` : `b = 34,881` et `a = 0,737`

Les 2 `P-value < 2e-16` indique que les coefficients a et b sont non nuls.

`48,6` de la variance de y est capturé par la variable de régression liniaire `aX+b`

```r
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
```

![](/Statistiques_project/img/Rplot-1.png)

#### Exercice 4
Intervalle de prédiction au niveau de `95%` à la sortie du tunnel de la croix rousse : `[117-126]`. 

```r
    x2 <- rep(200,length.out = 124)
    t.test(x2,y,conf.level = 0.95)
```
```
        Welch Two Sample t-test

    data:  x2 and y
    t = 52, df = 123, p-value <2e-16
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
    117 126
    sample estimates:
    mean of x mean of y 
        200.0      78.1 
```

#### Exercice 5

- Pour x : `p value = 0`
- Pour y : `p value = 0.135`

On peut en conclure que on peut conserver l’hypothèse de normalité de y mais rejeter cette hypothèse pour x  

- Pour z : `p value = 0.042`
- Pour z’ : `p value = 0.304`

On peut en conclure que on peut conserver les deux hypothèses  

```r
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
```

#### Exercice 6

La droite de régression `az’+b` : `b = -30.82` et `a = 27.52` 

Les 2 `P-value < 1,22e-13` indique que les coefficients a et b sont non nuls. 

`48,6` de la variance de y est capturé par la variable de régression linéaire `az’+b`

```r
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
```

![](/Statistiques_project/img/Rplot-2.png)

#### Exercice 7

Selon nous, la mesure de particules peut servir de de référence pour la mesure de pollution car elle à pratiquement les mêmes intervalles.

```r
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
```


## 2. Hypotheses

#### Exercice 1

N = 10000 tests.

On déduit que V suit une loi uniforme sur `[0,2]`, la densité est a `0,5` (sur le graphe) donc en calculant `1/b-a = 0.5`.

```r
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
```

![](/Statistiques_project/img/Rplot-3.png)

#### Exercice 2

...

#### Exercice 3

L’ensemble d’après le graphique est de `[0,1]` pour V(y) et `[-1,1]` pour U(x) donc l’intervalle est compris entre `[-1,2]`.

```r
    U <- runif (10000 ,min=-1,max=1); V <- runif (10000 ,min=0,max=1) 
    S <- (1>abs (U)+V)
    U0 <- U[ S ] ; V0 <- V[ S ]
    
    #Tracage du nuage de points de l'?chnatillon (U,V)
    plot(U,V,pch='.', main = "Nuage de points de l'?chantillon", col="red")
```

![](/Statistiques_project/img/Rplot-4.png)

#### Exercice 4 :

```r
    #Tracage du nuage de points de l'?chnatillon (U0,V0)
    plot(U0,V0,pch='.', main = "Nuage de points de l'?chantillon", col="red")
```

L’intervalle pour U0 et V0 est de `[-1,1]`

![](/Statistiques_project/img/Rplot-5.png)

#### Exercice 4 :

La densité de U0 est de `1`. 

Le programme calcule la valeur absolue de U puis additionne a V (qui a des valeur comprise entre `[0,1]` ) et rejette les résultats plus grand que `1` et ce résultats  et renvoyer dans S. 

```r
    #Tracage de l'histogramme de comparaison avec la densit?
    densite <- density(U0)
    hist(U0,freq = FALSE,col = "lightblue",border="white", main = "Comparaison entre l'histogramme et la densit?", ylab="Densit?")
    lines(densite, col='blue',lty = 1, lwd = 5) 
```

![](/Statistiques_project/img/Rplot-6.png)