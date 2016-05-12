
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)

wybory_testow <- typy_testow %>% distinct(rok, rodzaj_egzaminu, czesc_egzaminu)
nazwy_testow <- apply(wybory_testow %>% select(-id), 1, (function(x) paste(x, collapse=" ")))

t_list <- setNames(wybory_testow$id, nazwy_testow)

shinyUI(pageWithSidebar(
  
  headerPanel("Trzecia iteracja"),
  
  sidebarPanel(
    selectInput("egzamin", label = "Egzamin:",
                choices = t_list),
    
    selectInput("poprzedni_egzamin", label = "Poprzedni egzamin:",
                choices = t_list),
    
    selectInput("poziom", label = "Prezentuj po:",
                choices = list(Kryteriach = "kryt", Pytaniach = "pyt", Wiązkach = "wia")),
    
    checkboxInput("wykresy_plec", "Wykres zależności dla płci?", 
                  value = FALSE),
    checkboxInput("wykresy_poprzedni", "Wykres zależności od poprzednich egzaminów?", 
                  value = FALSE),
    actionButton("gen", "Generuj")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Podsumowanie",
          conditionalPanel(
            condition = "input.wykresy_plec == true",
            plotOutput("plec_plot")
          ),
          conditionalPanel(
            condition = "input.wykresy_poprzedni == true",
            plotOutput("poprz_plot")
          ),
          conditionalPanel(
            condition = "input.wykresy_plec == true",
            plotOutput("wies_plot")
          ),
          conditionalPanel(
            condition = "input.wykresy_ == true",
            plotOutput("szkola_plot")
          )
        ),
        tabPanel("Szczegóły",
            plotOutput("szczegoly_ui", height="4000px")
        )
      )
  )
))