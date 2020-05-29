#install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(jsonlite)
library(ggplot2)

#preguntar a l'usuari la línia a obtenir dades i el tipus de dia
line <- readline(prompt="Escollir línia (1, 2, 3, 4, 5, 91(L9S), 101(L10S), 94(L9N), 104(L10N)):")
day <- readline(prompt="Escollir dia setmana (1-5 laborable, 6 dissabte, 7 festiu):")


#obtenció de dades
url <- paste(c("https://api.tmb.cat/v1/ocupacio/estacions/linia/", line, "?app_id=4c132798&app_key=8504ae3a636b155724a1c7e140ee039f&dia_setmana=", day), collapse = '')
demand <- fromJSON(url)

demandprop <- demand$features$properties
newdemand <- demandprop[order(demandprop$ID_SENTIT, demandprop$ORDRE_ESTACIO),]


#eliminació de franges horàries on el metro no opera
demandV1 <- newdemand[newdemand$ID_SENTIT==1,]
demandV1 <- demandV1[demandV1$HORA!="00:00",]
demandV1 <- demandV1[demandV1$HORA!="01:00",]
demandV1 <- demandV1[demandV1$HORA!="02:00",]
demandV1 <- demandV1[demandV1$HORA!="03:00",]
demandV1 <- demandV1[demandV1$HORA!="04:00",]

demandV2 <- newdemand[newdemand$ID_SENTIT==2,]
demandV2 <- demandV2[demandV2$HORA!="00:00",]
demandV2 <- demandV2[demandV2$HORA!="01:00",]
demandV2 <- demandV2[demandV2$HORA!="02:00",]
demandV2 <- demandV2[demandV2$HORA!="03:00",]
demandV2 <- demandV2[demandV2$HORA!="04:00",]


#substituir NULL per 0 als valors d'ocupació
demandV1$PERCENTATGE_OCUPACIO[is.na(demandV1$PERCENTATGE_OCUPACIO)] <- 0
demandV2$PERCENTATGE_OCUPACIO[is.na(demandV2$PERCENTATGE_OCUPACIO)] <- 0


#obtenció de noms de les estacions per a mostrar-los al gràfic
stationsV1 <- unique(demandV1$NOM_ESTACIO)
stationsV2 <- unique(demandV2$NOM_ESTACIO)


#gràfic de V1
ggplot(data=demandV1,
         aes(x=ORDRE_ESTACIO, y=PERCENTATGE_OCUPACIO, colour=HORA)) +
         geom_line() + 
     labs(color='Franja horària') +
     scale_y_continuous(name="Percentatge d'ocupació dels trens") +
		 scale_x_continuous(breaks=seq(1,length(stationsV1)), name="Estacions", labels=stationsV1) + 
		 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		 theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_line(colour = "white",size=1))


#gràfic de V2		 
ggplot(data=demandV2,
         aes(x=ORDRE_ESTACIO, y=PERCENTATGE_OCUPACIO, colour=HORA)) +
         geom_line() + 
     labs(color='Franja horària') +
     scale_y_continuous(name="Percentatge d'ocupació dels trens") +
		 scale_x_continuous(breaks=seq(1,length(stationsV2)), name="Estacions", labels=stationsV2) + 
		 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		 theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_line(colour = "white",size=1))

