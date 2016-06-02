
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(shinyBS)

modalHelp <- function(name, header = "[?]", content = "Pomoc niedostępna.") {
  tags$html(actionButton(name, "[?]"),
            bsModal(paste(c("window",name), collapse="_"), header, name, tags$div(content)))
}

interface_help <- tags$html(
  tags$div("Korzystasz z narzędzia służącego do wizualizacji zależności pomiędzy wynikami 
           osiągniętymi przez uczniów na poprzednim szczeblu edukacji, a wynikami egzaminu."),
  tags$div("Na początku możesz wybrać:"),
  tags$ul(
    tags$li("Egzamin którym jesteś zainteresowany."),
    tags$li("Arkusz egzaminu który ma zostać rozpatrzony. Ten sam egzamin może posiadać różne arkusze,
            na przykład wersje dla dyslektyków lub \"stare\"/\"nowe\" matury. Sprawdzić, który z nich cię
            interesuje możesz np. na stronie", tags$a(href="http://zpd.ibe.edu.pl/doku.php?id=bazatestypytania", "arkusze"),
            "lub wybierając arkusz i wchodząc w link do jego treści."),
    tags$li("Poprzedni egzamin - czyli ten, z którym będziemy porównywać wyniki."),
    tags$li("Jak bardzo szczegółowy ma być wykres. Pytania egzaminacyjne podzielone są na:" 
            , tags$ul(
              tags$li("Kryteria ocen - najbardziej atomiczne cząstki oceny. Może to być np. pojedyncze
                      pytanie zamknięte, lub ocena za interpunkcję w wypracowaniu."),
              tags$li("Pytania - jedno pytanie składa się z jednego lub większej liczby kryterium oceny.
                      Odpowiadają rzeczywistym pytaniom występującym na arkuszach."),
              tags$li("Wiązki pytań - pytania są czasami, choć nie zawsze, podzielone na tematyczne grupy
                      nazywane wiązkami pytań.")
              )
              )
    )
  )

glowny_wykres_help <- tags$html(
  tags$div("Na tym wykresie przedstawione są wyniki poszczególnych kryteriów / pytań / wiązek pytań z wybranego egzaminu."),
  tags$div("Wyniki uczniów z danego kryterium niemal zawsze rosną wraz ze wzrostem wyników z poprzedniego egzaminu. Szerokość
           elementu pokazuje, jak szybki jest ten wzrost."),
  tags$ul(
    tags$li("Szeroka część oznacza, że wzrost wyniku z poprzedniego egzaminu nie przekładał się  znacząco na wzrost wyniku z kryterium."),
    tags$li("Wąska część oznacza, że wraz z wynikiem z poprzedniego egzaminu pyniki z kryterium znacznie się poprawiały.")
  ),
  tags$div("Z częściami egzaminu z wykresem szerokim u dołu słabi i średni uczniowie poradzili sobie zatem tak samo słabo, a
           różnica pojawiła się dla lepszych uczniów. Z częściami egzaminu szerokimi u góry gorzej poradzili sobie tylko słabsi
           uczniowie, zaś uczniowie średni i lepsi poradzili sobie z nimi tak samo dobrze."),
  tags$div("Aby zapoznać się ze szczegółowym wykresem danej części egzaminu, kliknij na jej wykres.")
  )

wykres_header <- function(nazwa, pomoc) {
  tags$div(
    tags$h3(style = "display:inline", nazwa),
    modalHelp("Pomoc", header = "Pomoc", content = pomoc
    )
  )
}

glowny_wykres_header <- wykres_header("Wykres egzaminu", glowny_wykres_help)
wykres_kryterium_header <- wykres_header("Szczegółowy wykres części", "")

shinyUI(pageWithSidebar(
  
  headerPanel("Czwarta, finalna iteracja"),
  
  sidebarPanel(
    uiOutput("egz_wybor"),
    uiOutput("arkusz_wybor"),
    uiOutput("egz_wybor_poprz"),
    selectInput("poziom", label = "Grupuj po:", list(
      "wiązkach pytań" = "wia",
      "pytaniach" = "pyt",
      "kryteriach oceny" = "kry"
    )
    )
  ),
  mainPanel(
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Wprowadzenie",
        interface_help
      ),
      tabPanel(
        "Wykresy",
        value = "poprzednie",
        glowny_wykres_header,
        htmlOutput("linki_do_arkusza"),
        plotOutput("poprz_plot", click="poprz_click", hover="poprz_hover"),
        wykres_kryterium_header,
        plotOutput("poprz_plot_jedno"),
        htmlOutput("arkusze_zawierajace")
      ),
      tabPanel(
        "Tabele",
        dataTableOutput("poprz_tabela")
        )
    ))
))