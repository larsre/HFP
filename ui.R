



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
  titlePanel("Værforhold, innsats og smågnagerindeks under takseringene"),
  br(),
  hr(),
  
  sidebarLayout(position = "left",
                sidebarPanel("Trykk på et fylke for å se hvordan forholdene var under takseringene", width = 3,
                             radioButtons("Forhold", label = "", choices = Fylke1, selected = NULL),
                             
                             #tags$hr(),
                             helpText("Vær opmerksom på at i fylker der det er liten innsats (det vil si få linjer og få kilometer taksert) kan man få
                                      uventede utslag i dataene"),
                             conditionalPanel(
                               "$('li.active a').first().html()==='Takseringsforhold'",    
                               br(),
                               helpText(strong("Forhold for hunden"), "er en subjektiv vurdering av vitringsforholdene under takseringen. 
                                        Det er taksørene selv som vurderer om forholdene er 1 = vanskelige, 2 =middels eller 
                                        3 = gode for hunden. Figuren til høyre viser gjennomsnittet under takseringene i fylket."),
                               br(),
                               helpText(strong("Temperatur (omtrentelig)"),"under takseringene vurderes også av taksørene. Det måles ikke temperatur i felt.
                                        Figuren viser gjennomsnittet for fylket." ),
                               
                               br(),
                               helpText(strong("Værforhold"),"under takseringene registreres også av taksørene. Dersom været er skiftende
                                        brukes den dominerende værtypen. Værforholdene vurderes subjektivt som; 1=Mye nedbør, 
                                        2=Lett nedbør (duskregn), 3=Overskyet oppholdsvær, 4=sol. I figuren til høyre vises den værtypen som hyppigst opptrer under
                                        tasksering i fylket.")
                               ),
                             
                             conditionalPanel(
                               "$('li.active a').first().html()==='Innsats'",    
                               br(),
                               helpText(strong("Antall linjer"),"er antall linjer taksert i fylket det enkelte år."),
                               br(),
                               helpText(strong("Kilometer taksert"), "er anntall kilometer linjer taksert samlet for fylket. Denne henger sammen med antallet linjer,
                                        og begge variablene gir en god ideks på omfanget av taksering i fylket" )
                               
                               ),
                             
                             conditionalPanel(
                               "$('li.active a').first().html()==='Smågnagerobservasjon'",    
                               br(),
                               helpText(strong("Smågnagere, rev og hare sett under takseringene.")," Verdiene i figuren til høyre viser prosent av linjene i
                                        et fylke der disse artene ble observert. Som man ser observeres det lite rev og hare, men i 'smågnagerår' observeres
                                        det smågnagere på mange linjer. 'Sett smågnagere' er en grov indeks for smågnagertetthet og toppene har vist seg å
                                        sammenfalle med gode prosuksjonsår for lirype"))
                             
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
                                      
                                      tabPanel(("Smågnagerobservasjon"),
                                               hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%","50%"),
                                                             plotlyOutput(outputId = "distPlot5" ),
                                                             plotlyOutput(outputId = "distPlot6" ),
                                                             plotlyOutput(outputId = "distPlot7" ))))
                                      
                                      
                          ))
                
                             ))
