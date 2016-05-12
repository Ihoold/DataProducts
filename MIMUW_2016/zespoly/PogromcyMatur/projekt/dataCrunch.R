library(ZPD)
library(tidyr)

#load("bigData.RData")
#load("smallData.RData")

zapisz_male_dane <- function() {
  save(ostatnie_przystapienia, uczniowie, regiony, szkoly, typy_testow, kryteria, normy, file="smallData.RData")
}

zapisz_duze_dane <- function() {
  save(wyniki_szkol, wyniki_po_egz, wyniki_po_plci, oceny_uczniow, file="bigData.RData")
}

przeladuj_regiony <- function() {
  src = polacz()
  reg = pobierz_szkoly(src) %>%
        select(gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly) %>%
        distinct() %>%
        rename(gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly) %>%
        collect() %>%
        as.data.frame()
  regiony <<- reg
}

zaladuj_nowe_szkoly <- function(p_rok) {
  src = polacz()
  nowe = pobierz_szkoly(src) %>%
         filter(rok == p_rok) %>%
         #filter(!dla_doroslych, is.na(artystyczna), (is.na(specjalna) | !specjalna)) %>% #Odfiltruj bardzo nietypowe szkoly
         select(id_szkoly, gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly, nazwa_szkoly, typ_szkoly, rok) %>%
         rename(id = id_szkoly, gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly,
                nazwa = nazwa_szkoly, typ = typ_szkoly) %>%
         collect() %>%
         as.data.frame()
  szkoly <<- rbind(szkoly, nowe)
}

zaladuj_nowe_testy <- function(p_rok) {
  src = polacz()
  nowe = pobierz_testy(src) %>%
         filter(rok == p_rok, czy_egzamin == TRUE, dane_ewd == TRUE) %>%
         filter(rodzaj_egzaminu != "matura poprawkowa") %>% # TODO - jakos uwzgledniac
         select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok) %>%
         rename(id = id_testu) %>%
         collect() %>%
         as.data.frame()
  typy_testow <<- rbind(typy_testow, nowe)
}

przeladuj_kryteria <- function() {
  src = polacz()
  kryt = pobierz_kryteria_oceny(src) %>%
         select(kryterium, l_punktow,
                numer_pytania, numer_kryterium, id_pytania, id_wiazki, tresc_pytania, tresc_wiazki) %>%
         rename(id = kryterium, max_punktow = l_punktow) %>%
         collect() %>%
         distinct() %>%
         as.data.frame()
  kryteria <<- kryt
}

zaladuj_normy <- function() {
  src = polacz()
  skalowania = pobierz_skale(src) %>%
    filter(rodzaj_skali == 'zrównywanie', posiada_normy) %>%
    select(id_skali, id_testu, skalowanie, opis_skalowania)

  normy <<- pobierz_normy(src) %>%
    inner_join(skalowania) %>%
    select(id_testu, wartosc_zr, wartosc) %>%
    collect()
}

zaladuj_uczniow <- function() {
  src = polacz()
  uczniowie <<- pobierz_uczniow(src) %>%
    select(id_obserwacji, plec) %>%
    collect()
}

zaladuj_male_dane <- function () {
  przeladuj_regiony()
  przeladuj_kryteria()
  zaladuj_normy()
  zaladuj_uczniow()
}

zaladuj_male_dane_rok <- function(rok) {
  zaladuj_nowe_szkoly(rok)
  zaladuj_nowe_testy(rok)
}

zaladuj_ostatnie_przystapienia <- function() {
  # Nie ma odczuwalnej roznicy miedzy sciaganiem calych danych i robieniem selecta, wiec sciagamy cale dane
  src = polacz()
  ostatnie_przystapienia <<- NULL

  przystapienia = filtruj_przystapienia(src, FALSE, 'matura', NULL, TRUE) %>% collect()
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% rbind(przystapienia)
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'egzamin gimnazjalny', NULL, TRUE) %>% collect()
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% rbind(przystapienia)
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'sprawdzian', NULL, TRUE) %>% collect()
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% rbind(przystapienia)
  
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% select(id_obserwacji, rodzaj_egzaminu, rok)
}

dodaj_wyniki_szkol <- function(src, dane, row) {
  nowe_wyniki_szkol = dane %>%
    group_by(id_szkoly) %>%
    summarise_each(funs(mean(., na.rm=TRUE)),
                   -id_szkoly, -id_obserwacji, -id_testu) %>%
    gather(id_kryterium, wynik, -id_szkoly, na.rm = TRUE) %>%
    rename(id = id_szkoly) %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)

  wyniki_szkol <<- rbind(wyniki_szkol, nowe_wyniki_szkol)
}

dodaj_sumaryczne_oceny_uczniow <- function(src, dane, row) {
  # Sumaryczne wyniki uczniow, zrownane
  sumy = rowSums(dane %>% select(starts_with("k_")))
  nowe_oceny_uczniow <- dane %>%
    mutate(wynik = sumy) %>%
    select(id_obserwacji, id_testu, id_szkoly, wynik) %>%
    inner_join(normy, by = c("id_testu" = "id_testu", "wynik" = "wartosc")) %>%
    select(-wynik) %>%
    rename(wynik=wartosc_zr) %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)

  oceny_uczniow <<- oceny_uczniow %>% rbind(nowe_oceny_uczniow)
}

dodaj_oceny_po_poprzednich <- function(src, dane, row) {
  uczniowie <- dane %>%
    select(id_obserwacji) %>%
    distinct()

  poprzednie_oceny = oceny_uczniow %>%
    inner_join(uczniowie) %>%
    filter(rodzaj_egzaminu != row$rodzaj_egzaminu) %>% # Rozważaj tylko scisle wczesniejsze egzaminy
    inner_join(ostatnie_przystapienia)

  poprzednie_egzaminy = poprzednie_oceny %>%
    select(rodzaj_egzaminu, czesc_egzaminu) %>%
    distinct()

  # Posortuj uczniow po poprzednich pisanych egzaminach, posumuj procenty
  for (j in 1:nrow(poprzednie_egzaminy)) {
    egz = poprzednie_egzaminy[j,]

    szer = poprzednie_oceny %>%
      inner_join(egz) %>%
      rename(rodzaj_poprzedni=rodzaj_egzaminu, czesc_poprzedni=czesc_egzaminu) %>%
      select(id_obserwacji, rodzaj_poprzedni, czesc_poprzedni, wynik) %>%
      inner_join(dane) %>%
      arrange(wynik) %>%
      rename(poprzedni_wynik = wynik)

    szer = szer %>%
      group_by(poprzedni_wynik, rodzaj_poprzedni, czesc_poprzedni) %>%
      mutate(liczba = n()) %>%
      mutate_each(funs(mean(., na.rm = TRUE)),
                     starts_with("k_")) %>%
      select(poprzedni_wynik, rodzaj_poprzedni, czesc_poprzedni, starts_with("k_"), liczba) %>%
      distinct() %>%
      gather(id_kryterium, wynik, starts_with("k_"), na.rm = TRUE) %>%
      mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)

    wyniki_po_egz <<- wyniki_po_egz %>% rbind(szer)
  }
}

dodaj_oceny_po_plci <- function(dane, row) {
  nowe = uczniowie %>%
    select(id_obserwacji, plec) %>%
    filter(!is.na(plec)) %>% # Niektorzy uczniowie nie maja informacji o plci
    inner_join(dane)
  nowe = nowe %>%
    group_by(plec) %>%
    summarise_each(funs(mean(., na.rm = TRUE)),
                   starts_with("k_"))
    nowe = nowe %>%
    gather(id_kryterium, wynik, starts_with("k_"), na.rm = TRUE)
    nowe = nowe %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
  wyniki_po_plci <<- wyniki_po_plci %>% rbind(nowe)
}

dodaj_oceny_po_dysleksji <- function(src, dane, row) {
  ut = pobierz_dane_uczniowie_testy(src)
  testy = pobierz_testy(src) %>%
    filter(rok = row$rok, rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu) %>%
    inner_join(ut) %>%
    select(id_obserwacji, dysleksja) %>%
    distinct() %>%
    collect()
  
  nowe = dane %>%
    inner_join(uczniowie) %>%
    group_by(dysleksja) %>%
    summarise_each(funs(mean(., na.rm = TRUE)),
                   starts_with("k_")) %>%
    gather(id_kryterium, wynik, starts_with("k_"), na.rm = TRUE) %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
  wyniki_po_dysleksji <<- wyniki_po_dysleksji %>% rbind(nowe)
}

zaladuj_nowe_wyniki_szkol <- function(p_rok, t_rodzaj = NULL, t_czesc = NULL) {
  src = polacz()

  #Testy z podanego roku
  testy = pobierz_testy(src) %>%
          filter(rok == p_rok, czy_egzamin == TRUE, dane_ewd == TRUE) %>%
          filter(rodzaj_egzaminu != "matura poprawkowa") %>% # TODO - jakos uwzgledniac
          select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok)
  
  #Typowe szkoly
  t_szkoly = szkoly %>%
    filter(rok == p_rok) %>%
    select(id) # Gwarantowany distinct
  
  # Dla kazdego typu egzaminu pobierz dane oddzielnie, by zmiescic sie w pamieci
  testy_iter = testy %>%
               collect() %>%
               select(rodzaj_egzaminu, czesc_egzaminu, rok) %>%
               distinct()
  if (!is.null(t_rodzaj)) {
    testy_iter = testy_iter %>% filter(rodzaj_egzaminu == t_rodzaj)
  }

  if (!is.null(t_czesc)) {
    testy_iter = testy_iter %>% filter(czesc_egzaminu == t_czesc)
  }
  
  for (i in 1:nrow(testy_iter)) {
    row = testy_iter[i,]
    
    # Wyniki egzaminu
    dane = pobierz_wyniki_egzaminu(src, row$rodzaj_egzaminu, row$czesc_egzaminu, row$rok, TRUE) %>%
      collect() %>%
      select(-rok) %>%
      inner_join(t_szkoly, by = c("id_szkoly" = "id"))
   
    dodaj_wyniki_szkol(src, dane, row)
    dodaj_sumaryczne_oceny_uczniow(src, dane, row)
    dodaj_oceny_po_poprzednich(src, dane, row)
    dodaj_oceny_po_plci(dane, row)
  }
}

zaladuj_duze_dane <- function() {
  zaladuj_ostatnie_przystapienia()
}

zaladuj_duze_dane_rok <- function(rok) {
  zaladuj_nowe_wyniki_szkol(rok)
}
