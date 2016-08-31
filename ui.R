



library(shiny)
library(plotly)
#ui
ui <- fluidPage(
  
  # Application title #Finne bilde
  br(),
  br(),
  img(src=(href = "http://honsefugl.nina.no/Innsyn/Content/images/logoweb_liten.png"), width = "300px", height = "50px"),
  br(),
  br(),
  titlePanel("V�rforhold, innsats og sm�gnagerindeks under takseringene"),
  br(),
  hr(),
  
  sidebarLayout(position = "left",
                sidebarPanel("Trykk p� et fylke for � se hvordan forholdene var under takseringene", width = 3,
                             radioButtons("Forhold", label = "", choices = Fylke1, selected = NULL),
                             
                             #tags$hr(),
                             helpText("V�r opmerksom p� at i fylker der det er liten innsats (det vil si f� linjer og f� kilometer taksert) kan man f�
                                      uventede utslag i dataene"),
                             conditionalPanel(
                               "$('li.active a').first().html()==='Takseringsforhold'",    
                               br(),
                               helpText(strong("Forhold for hunden"), "er en subjektiv vurdering av vitringsforholdene under takseringen. 
                                        Det er taks�rene selv som vurderer om forholdene er 1 = vanskelige, 2 =middels eller 
                                        3 = gode for hunden. Figuren til h�yre viser gjennomsnittet under takseringene i fylket."),
                               br(),
                               helpText(strong("Temperatur (omtrentelig)"),"under takseringene vurderes ogs� av taks�rene. Det m�les ikke temperatur i felt.
                                        Figuren viser gjennomsnittet for fylket." ),
                               
                               br(),
                               helpText(strong("V�rforhold"),"under takseringene registreres ogs� av taks�rene. Dersom v�ret er skiftende
                                        brukes den dominerende v�rtypen. V�rforholdene vurderes subjektivt som; 1=Mye nedb�r, 
                                        2=Lett nedb�r (duskregn), 3=Overskyet oppholdsv�r, 4=sol. I figuren til h�yre vises den v�rtypen som hyppigst opptrer under
                                        tasksering i fylket.")
                               ),
                             
                             conditionalPanel(
                               "$('li.active a').first().html()==='Innsats'",    
                               br(),
                               helpText(strong("Antall linjer"),"er antall linjer taksert i fylket det enkelte �r."),
                               br(),
                               helpText(strong("Kilometer taksert"), "er anntall kilometer linjer taksert samlet for fylket. Denne henger sammen med antallet linjer,
                                        og begge variablene gir en god ideks p� omfanget av taksering i fylket" )
                               
                               ),
                             
                             conditionalPanel(
                               "$('li.active a').first().html()==='Sm�gnagerobservasjon'",    
                               br(),
                               helpText(strong("Sm�gnagere, rev og hare sett under takseringene.")," Verdiene i figuren til h�yre viser prosent av linjene i
                                        et fylke der disse artene ble observert. Som man ser observeres det lite rev og hare, men i 'sm�gnager�r' observeres
                                        det sm�gnagere p� mange linjer. 'Sett sm�gnagere' er en grov indeks for sm�gnagertetthet og toppene har vist seg �
                                        sammenfalle med gode prosuksjons�r for lirype"))
                             
                               ), 
                
                
                
                # Ulike plot legges under et "tabsetpanel"
                mainPanel(width=5,
                          br(),
                          br(),
                          tabsetPanel(type="pills",
                                      tabPanel(("Takseringsforhold"),
                                               hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%","50%"),
                                                             plotlyOutput(outputId = "distPlot"), 
                                                             plotlyOutput(outputId = "distPlot1"),
                                                             plotlyOutput(outputId = "distPlot2")))),
                                      
                                      tabPanel(("Innsats"),
                                               hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%","50%"),
                                                             plotlyOutput(outputId = "distPlot3" ),
                                                             plotlyOutput(outputId = "distPlot4" )))),
                                      
                                      tabPanel(("Sm�gnagerobservasjon"),
                                               hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%","50%"),
                                                             plotlyOutput(outputId = "distPlot5" ),
                                                             plotlyOutput(outputId = "distPlot6" ),
                                                             plotlyOutput(outputId = "distPlot7" ))))
                                      
                                      
                          ))
                
                             ))