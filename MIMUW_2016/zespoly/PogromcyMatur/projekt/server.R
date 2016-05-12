
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(ZPD)
library(ggplot2)
library(scales)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

rysuj_wykres_plec <- function(egzamin, poziom = NULL) {
  d_egz = typy_testow %>% filter(id == egzamin) %>%
    select(rok, rodzaj_egzaminu, czesc_egzaminu)
  
  wyniki_plec = wyniki_po_plci %>%
    inner_join(d_egz)
  
  if (poziom == "pyt") {
    wyniki_plec = wyniki_plec %>%
      inner_join(kryteria, by = c("id_kryterium" = "id")) %>%
      group_by(plec, id_pytania) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_pytania)
  } else if (poziom == "wia") {
    wyniki_plec = wyniki_plec %>%
      inner_join(kryteria, by = c("id_kryterium" = "id")) %>%
      group_by(plec, id_wiazki) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_wiazki)
  } else {
    wyniki_plec = wyniki_plec %>%
      inner_join(kryteria, by = c("id_kryterium" = "id")) %>%
      rename(id = id_kryterium)
  }
  wyniki_plec$wynik = wyniki_plec$wynik / wyniki_plec$max_punktow
  
  return (ggplot(wyniki_plec, aes(x=factor(id), y = wynik)) + 
            geom_bar(stat = "identity", aes(fill=plec), position = "dodge") +
            scale_fill_discrete(name="Płeć", breaks=c("k","m"), labels=c("Kobieta", "Mężczyzna")))
}

rysuj_wykres_poprzedni <- function(egzamin, poprzedni, poziom = NULL) {
  d_egz = typy_testow %>% filter(id == egzamin) %>%
    select(rok, rodzaj_egzaminu, czesc_egzaminu)
  d_poprz = typy_testow %>% filter(id == poprzedni) %>%
    select(rodzaj_egzaminu, czesc_egzaminu) %>%
    rename(rodzaj_poprzedni = rodzaj_egzaminu, czesc_poprzedni = czesc_egzaminu)
  
  wyniki_egz = wyniki_po_egz %>%
    inner_join(d_egz) %>%
    inner_join(d_poprz)
  
  if (poziom == "pyt") {
    wyniki_egz = wyniki_egz %>%
      inner_join(kryteria, by = c("id_kryterium" = "id")) %>%
      group_by(poprzedni_wynik, id_pytania, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_pytania)
  } else if (poziom == "wia") {
    wyniki_egz = wyniki_egz %>%
      inner_join(kryteria, by = c("id_kryterium" = "id")) %>%
      group_by(poprzedni_wynik, id_wiazki, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_wiazki)
  } else {
    wyniki_egz = wyniki_egz %>%
      inner_join(kryteria, by = c("id_kryterium" = "id")) %>%
      rename(id = id_kryterium)
  }
  
  wyniki_egz$wynik = wyniki_egz$wynik / wyniki_egz$max_punktow
  
  return (ggplot(wyniki_egz, aes(x = factor(id), y = wynik)) +
          geom_point(aes(color = poprzedni_wynik, size = liczba)))
}

shinyServer(function(input, output) {
  observeEvent(input$gen, {
    if (input$wykresy_plec)
      output$plec_plot <- renderPlot(rysuj_wykres_plec(input$egzamin, input$poziom))
    if (input$wykresy_poprzedni)
      output$poprz_plot <- renderPlot(rysuj_wykres_poprzedni(input$egzamin, input$poprzedni_egzamin, input$poziom))
  })
})
