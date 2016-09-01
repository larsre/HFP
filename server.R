library(shiny)
library(plyr)
library(ggplot2)
library(plotly)
library(RJDBC)
library(stringr)
library(scales)

##SUGERØRET INN I HØNSEFUGLPORTALEN##
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
            "/etc/sqljdbc_3.0/sqljdbc4.jar")

conn_pt <-dbConnect(drv, url="jdbc:sqlserver://ninsql05.nina.no;Database=SmaviltTest", "", "")
dbListTables(conn_pt) #VISER ALLE TABELLENE I PORTALEN
temp <- dbReadTable (conn_pt, "Taksering") # DATA KNYTTTET TIL TAKSERINGSLINJENE (IKKE OBSERVASJONER).

#Data som ligger i tabellen "Taksering"
Taks <- dbGetQuery(conn_pt,"SELECT TakseringID,   
                   FK_LinjeID, 
                   Temperatur,
                   SettSmagnager,
                   SettRev,
                   SettHare, 
                   Aar,
                   FK_NedborID,
                   LengdeTaksert,
                   StartKL,
                   SluttKL,
                   FK_HundeforholdID FROM Taksering")

#Henter inn diverse fra ulike tabeller
TaksLin <- dbGetQuery(conn_pt, "SELECT LinjeID, FK_OmradeID FROM Takseringslinje") 
TaksOmr <- dbGetQuery(conn_pt, "SELECT OmradeID, FK_Fylkekomnr, OmradeNavn FROM Takseringsomrade") 
Kommune <- dbReadTable(conn_pt, "FYLKEKOMMUNE")

#struper sugerørert inn i portalen slik at de som bruker appen jobber med ett datasett
dbDisconnect(conn_pt) 

### Merging data, lager datasett "d"

d <- merge(Taks, TaksLin, by.x="FK_LinjeID", by.y="LinjeID", all.x=T, all.y=F)
d <- merge(d, TaksOmr, by.x="FK_OmradeID", by.y="OmradeID", all.x=T, all.y=F)
d <- merge(d, Kommune, by.x="FK_Fylkekomnr", by.y="Fylkekomnr", all.x=T, all.y=F)
d<- subset(d, OmradeNavn!="Kursområde")

#--> Jeg vet ikke helt hva denne var til, men funket ikke med den.
d$Fylkesnavn <- str_trim(d$Fylkesnavn) 

#Aar til År resten av dagen...
colnames(d)[which(names(d) == "Aar")] <- "År"


#Lager tabellen DogCon som her inneholder en summary med snitt og sd av Hundeforhold
DogCon <- ddply(d, .(Fylkesnavn, År), summarize, snitt=round(mean(FK_HundeforholdID),2),sd=round(sd(FK_HundeforholdID),2)
)

#Lager tabellen LineTemp som inneholder gjennomsnittsverdier og sd for temperatur
LineTemp <- ddply(d, .(Fylkesnavn, År), summarize, snitt=round(mean(Temperatur),2), sd=round(sd(Temperatur),2))

#Lager tabell SmaGnagLin med andel linjer med smågnagerobservasjoner
d$SG <- as.numeric(d$SettSmagnager)   # Ny kolonne - sett smågnager som numerisk variabel

SmaGnagLin <-ddply(d,.(Fylkesnavn, År),summarize,
                   prosent=round(mean(SG), 2)*100)

#Lager tabell RevLin som viser andel linjer med reveobservasjoner 
d$SR <- as.numeric(d$SettRev)   # Ny kolonne - sett rev som numerisk variabel

RevLin <-ddply(d,.(Fylkesnavn, År),summarize,
               prosent=round(mean(SR), 2)*100)

##Lager tabell HarLin som viser andel linjer med hareobservasjoner 
d$SH <- as.numeric(d$SettHare)   # Ny kolonne - sett hare som numerisk variabel

HarLin <-ddply(d,.(Fylkesnavn, År),summarize,
               prosent=round(mean(SH), 2)*100)

#Lager tabellen NedbørLin - den verdien som oftest opprtrer (jf. takseringsskjema)
d$NB <- as.numeric(d$FK_NedborID)   # Ny kolonne - sett Nedbør som numerisk variabel
#Mode <- function(x) names(which.max(table(x)))
NedborLin<- ddply(d, .(Fylkesnavn, År), summarise,
                  oftest=names(which.max(table(NB))))
NedborLin$oftest<- as.numeric(NedborLin$oftest)  # Ny kolonne - sette Nedbør som numerisk variabel igjen

#Lager tabellen LengdeTaks som er total lengde taksert i fylke_år
LengdeTaks <- ddply(d, .(Fylkesnavn, År), summarize, snitt=round(sum(LengdeTaksert)), km=round(snitt/1000))

#Lager tabellen AntLin som er antall linjer i fylke_år
d$LinjeID <- as.numeric(d$FK_LinjeID)
d$LinjeID=1
d$LinjeID <- as.numeric(d$LinjeID)
AntLin <- ddply(d, .(Fylkesnavn, År), summarize, antall=round(sum(LinjeID)))




#############################################################
# Serverfunkskjon

shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlotly({
    tempo <- subset(DogCon, Fylkesnavn==input$Forhold)
    #theme_set(theme_grey(base_size = 10))
    gg <- ggplot(data=tempo, aes(x=År, y=snitt))+
      coord_cartesian(ylim = c(0.5, 3.5))+
      scale_y_continuous(limits = c(1,3),breaks = c(1,2,3), labels = c("Vanskelig", "Middels", "Gode" ))+
      scale_x_continuous(limits= c(2013, max(DogCon$År)), breaks=seq(2013, max(DogCon$År)))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title ="Forhold for hund") +
      geom_point(aes(x=År, y=snitt), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=snitt),col="darkorange3", size=0.7) 
    p <- ggplotly(gg, tooltip = c("x", "y"))
    p
  })
  
  output$distPlot1 <- renderPlotly({
    tempo1 <- subset(LineTemp, Fylkesnavn==input$Forhold)
    gg1<-  ggplot(data=tempo1, aes(x=År, y=snitt))+
      coord_cartesian(ylim = c(0, 25))+
      labs(y="", x="",title="Temperatur") +
      scale_x_continuous(limits= c(2013, max(LineTemp$År)), breaks=seq(2013, max(LineTemp$År)))+
      scale_y_continuous(limits = c(0,max(LineTemp$snitt)),breaks = c(0,5,10,15,20),labels = c("0","5","10","15","20"))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      geom_point(aes(x=År, y=snitt), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=snitt),col="darkorange3", size=0.7) 
    #geom_errorbar(aes(x=År, ymin=snitt-standardavvik, ymax=snitt+standardavvik),col="dark red", size=0.5)
    p1 <- ggplotly(gg1, tooltip = c("x", "y"))
    p1
  })
  
  output$distPlot2 <- renderPlotly({
    tempo2 <- subset(NedborLin, Fylkesnavn==input$Forhold)
    gg2<-  ggplot(data=tempo2, aes(x=År, y=oftest))+ 
      coord_cartesian(ylim = c(0.5, 4.5))+
      scale_y_continuous(limits = c(1,4),breaks = c(1,2,3,4), labels = c("Mye regn", "Lett regn","Overskyet","Sol" ))+
      scale_x_continuous(limits= c(2013, max(NedborLin$År)), breaks=seq(2013, max(NedborLin$År)))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title = "Værforhold")+
      geom_point(aes(x=År, y=oftest), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=oftest),col="darkorange3", size=0.7) 
    p2 <- ggplotly(gg2, tooltip = c("x", "y"))
    p2
    
  })
  
  output$distPlot3 <- renderPlotly({
    tempo3 <- subset(LengdeTaks, Fylkesnavn==input$Forhold)
    gg3<-   ggplot(data=tempo3, aes(x=År, y=km))+ 
      scale_x_continuous(limits= c(2013, max(LengdeTaks$År)), breaks=seq(2013, max(LengdeTaks$År)))+
      coord_cartesian(ylim = c(0, 2500))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title = "Kilometer taksert") +
      geom_point(aes(x=År, y=km), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=km),col="darkorange3", size=0.7) 
    p3 <- ggplotly(gg3, tooltip = c("x", "y"))
    p3
    
  })
  
  output$distPlot4 <- renderPlotly({
    tempo4 <- subset(AntLin, Fylkesnavn==input$Forhold)
    gg4<-   ggplot(data=tempo4, aes(x=År, y=antall))+ 
      scale_x_continuous(limits= c(2013, max(AntLin$År)), breaks=seq(2013, max(AntLin$År)))+
      coord_cartesian(ylim = c(0, 750))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title = "Antall linjer") +
      geom_point(aes(x=År, y=antall), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=antall),col="darkorange3", size=0.7) 
    p4 <- ggplotly(gg4, tooltip = c("x", "y"))
    p4
    
  })
  
  output$distPlot5 <- renderPlotly({
    tempo5 <- subset(SmaGnagLin, Fylkesnavn==input$Forhold)
    gg5 <- ggplot(data=tempo5, aes(x=År, y=prosent))+ 
      coord_cartesian(ylim = c(0, 100))+
      scale_y_continuous(breaks = c(25,50,75,100),labels = c("25%","50%","75%","100%"))+
      scale_x_continuous(limits= c(2013, max(SmaGnagLin$År)), breaks=seq(2013, max(SmaGnagLin$År)))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title="Sett Smågnager") +
      geom_point(aes(x=År, y=prosent), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=prosent),col="darkorange3", size=0.7) 
    p5 <- ggplotly(gg5, tooltip = c("x", "y"))
    p5
    
  })
  
  
  
  
  
  output$distPlot6 <- renderPlotly({
    tempo6 <- subset(RevLin, Fylkesnavn==input$Forhold)
    gg6 <- ggplot(data=tempo6, aes(x=År, y=prosent))+ 
      coord_cartesian(ylim = c(0, 100))+
      scale_y_continuous(limits= c(0, 100), breaks = c(25,50,75,100),labels = c("25%","50%","75%","100%"))+
      scale_x_continuous(limits= c(2013, max(RevLin$År)), breaks=seq(2013, max(RevLin$År)))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title="Sett Rev") +
      geom_point(aes(x=År, y=prosent), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=prosent),col="darkorange3", size=0.7) 
    p6 <- ggplotly(gg6, tooltip = c("x", "y"))
    p6
    
  })
  
  output$distPlot7 <- renderPlotly({
    tempo7 <- subset(HarLin, Fylkesnavn==input$Forhold)
    gg7 <- ggplot(data=tempo7, aes(x=År, y=prosent))+ 
      coord_cartesian(ylim = c(0, 100))+
      scale_y_continuous(limits= c(0, 125), breaks = c(25,50,75,100),labels = c("25%","50%","75%","100%"))+
      scale_x_continuous(limits= c(2013, max(HarLin$År)), breaks=seq(2013, max(HarLin$År)))+
      theme(axis.text.y = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = rel(1.3), hjust=0), 
            axis.title.x = element_text(size = rel(1.1)),
            axis.title.y = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1), colour="darkorange3"),
            panel.grid.major.y = element_line(colour = "grey50", linetype="dotted"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour="grey99")) +
      labs(y="", x="", title="Sett Hare") +
      geom_point(aes(x=År, y=prosent), col="darkorange3", size=3) +
      geom_line(aes(x=År, y=prosent),col="darkorange3", size=0.7) 
    p7 <- ggplotly(gg7, tooltip = c("x", "y"))
    p7
    
  })
  
  
})
