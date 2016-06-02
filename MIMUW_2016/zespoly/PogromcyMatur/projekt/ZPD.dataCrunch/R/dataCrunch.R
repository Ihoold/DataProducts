.zaladuj_testy <- function(ctxt) {
  src = polacz()
  nowe = pobierz_testy(src) %>%
    filter(czy_egzamin == TRUE, dane_ewd == TRUE) %>%
    filter(rodzaj_egzaminu != "matura poprawkowa") %>% # TODO - jakos uwzgledniac
    select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok, arkusz) %>%
    rename(id = id_testu) %>%
    collect() %>%
    as.data.frame()
  ctxt@typy_testow <- nowe
  return(ctxt)
}

.przeladuj_kryteria <- function(ctxt) {
  src = polacz()
  kryt_d = pobierz_kryteria_oceny(src) %>%
    collect()
  
  kryt = kryt_d %>%
    select(kryterium, l_punktow, numer_kryterium, id_pytania,
           id_wiazki, tresc_pytania, tresc_wiazki) %>%
    rename(id = kryterium, max_punktow = l_punktow) %>%
    distinct() %>%
    as.data.frame()
  ctxt@kryteria <- kryt
  
  kryt = kryt_d %>%
    select(kryterium, numer_pytania, id_testu) %>%
    rename(id = kryterium) %>%
    distinct() %>%
    as.data.frame()
  
  ctxt@kryteria_testy <- kryt
  return(ctxt)
}

.zaladuj_normy <- function(ctxt) {
  src = polacz()
  skalowania <- pobierz_skale(src) %>%
    filter(rodzaj_skali == 'zrównywanie', posiada_normy) %>%
    select(id_skali, id_testu, skalowanie, opis_skalowania)
  
  ctxt@normy <- pobierz_normy(src) %>%
    inner_join(skalowania) %>%
    select(id_testu, wartosc_zr, wartosc) %>%
    collect() %>%
    as.data.frame()
  return(ctxt)
}

#' Pobiera dane pomocnicze niezależne od roku.
#' 
#' Część przetwarzanych danych pełni funkcję pomocniczą w przetwarzaniu
#' wyników egzaminów. Ta funkcja wczytuje je.
#' 
#' Dane pobierane w tej funkcji mogą zmieniać się co roku. Wywołanie jej
#' zastąpi poprzednie dane w dazie danych - aby zaktualizować dane, wystarczy
#' więc wywołać tę funkcję raz.
#' @param ctxt Obiekt klasy DataCrunch, w którym mamy umieścić dane.
#' @return Obiekt DataCrunch z umieszczonymi nowymi danymi.
#' @export
zaladuj_male_dane <- function (ctxt) {
  ctxt = ctxt %>%
    .przeladuj_kryteria() %>%
    .zaladuj_normy() %>%
    .zaladuj_ostatnie_przystapienia() %>%
    .zaladuj_testy()
  return(ctxt)
}

.zaladuj_ostatnie_przystapienia <- function(ctxt) {
  # Nie ma odczuwalnej roznicy miedzy sciaganiem calych danych i robieniem selecta, wiec sciagamy cale dane
  src = polacz()
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'matura', NULL, TRUE) %>% 
    select(id_obserwacji, rodzaj_egzaminu, rok) %>% collect() %>% as.data.frame()
  ctxt@ostatnie_przystapienia <- przystapienia
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'egzamin gimnazjalny', NULL, TRUE) %>%
    select(id_obserwacji, rodzaj_egzaminu, rok) %>% collect() %>% as.data.frame()
  ctxt@ostatnie_przystapienia <- ctxt@ostatnie_przystapienia %>% rbind(przystapienia)
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'sprawdzian', NULL, TRUE) %>%
    select(id_obserwacji, rodzaj_egzaminu, rok) %>% collect() %>% as.data.frame()
  ctxt@ostatnie_przystapienia <- ctxt@ostatnie_przystapienia %>% rbind(przystapienia)
  
  return(ctxt)
}

.dodaj_sumaryczne_oceny_uczniow <- function(ctxt, src, dane, row) {
  # Sumaryczne wyniki uczniow, zrownane
  sumy = rowSums(dane %>% select(starts_with("k_")))
  nowe_oceny_uczniow <- dane %>%
    mutate(wynik = sumy) %>%
    select(id_obserwacji, id_testu, wynik) %>%
    inner_join(ctxt@normy, by = c("id_testu" = "id_testu", "wynik" = "wartosc")) %>%
    select(-wynik) %>%
    rename(wynik=wartosc_zr) %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
  
  ctxt@oceny_uczniow <- ctxt@oceny_uczniow %>% rbind(as.data.frame(nowe_oceny_uczniow))
  return(ctxt)
}

.dodaj_oceny_po_poprzednich <- function(ctxt, src, dane, row) {
  uczniowie <- dane %>%
    select(id_obserwacji) %>%
    distinct()
  
  poprzednie_oceny = ctxt@oceny_uczniow %>%
    inner_join(uczniowie) %>%
    filter(rodzaj_egzaminu != row$rodzaj_egzaminu) %>% # Rozważaj tylko scisle wczesniejsze egzaminy
    inner_join(ctxt@ostatnie_przystapienia)
  
  poprzednie_egzaminy = poprzednie_oceny %>%
    select(rodzaj_egzaminu, czesc_egzaminu) %>%
    distinct()
  
  # Posortuj uczniow po poprzednich pisanych egzaminach
  for (j in 1:nrow(poprzednie_egzaminy)) {
    egz = poprzednie_egzaminy[j,]

    # Wybierz uczniów, którzy pisali ten poprzedni egzamin
    szer = poprzednie_oceny %>%
      inner_join(egz) %>%
      rename(rodzaj_poprzedni=rodzaj_egzaminu, czesc_poprzedni=czesc_egzaminu) %>%
      select(id_obserwacji, rodzaj_poprzedni, czesc_poprzedni, wynik) %>%
      inner_join(dane) %>%
      arrange(wynik) %>%
      rename(poprzedni_wynik = wynik)
    
    # Zagreguj wyniki tych uczniow po liczbie punktow w poprzednim egzaminie
    szer = szer %>%
      select(poprzedni_wynik, rodzaj_poprzedni, czesc_poprzedni, starts_with("k_")) %>%
      gather(id_kryterium, wynik, starts_with("k_"), na.rm = TRUE) %>%
      group_by(poprzedni_wynik, rodzaj_poprzedni, czesc_poprzedni, id_kryterium) %>%
      mutate(wynik = mean(wynik)) %>%
      mutate(liczba = n()) %>%
      distinct() %>%
      mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
    
    ctxt@wyniki_po_egz <- ctxt@wyniki_po_egz %>% rbind(as.data.frame(szer))
  }
  return(ctxt)
}

#' Umieszcza w danych wyniki danego egzaminu w zależności od sumarycznych wyników
#' wszystkich poprzednich.
#' 
#' UWAGA: dane, które w bazie już się znajdują zostaną zignorowane. Jeśli chcesz wprowadzić
#' je ponownie, użyj funkcji usun_wyniki.
#' 
#' UWAGA: zalecane jest chronologiczne wprowadzanie danych.
#' Patrz vignette(package="ZPD.dataCrunch", topic="wyniki_po_egz").
#' @param ctxt Obiekt klasy DataCrunch, w którym mamy umieścić dane.
#' @param p_rok Rok egzaminu.
#' @param t_rodzaj Rodzaj egzaminu.
#' @param t_czesc Część egzaminu.
#' @param l_dane Dane pobrane przez pakiet ZPD funkcją pobierz_wyniki_egzaminu(..., czy_ewd = TRUE).
#' 
#' @return Obiekt danych DataCrunch z dodanymi danymi, jeśli dane zostały poprawnie pobrane
#' i wprowadzone do bazy. Niezmieniony obiekt, jeśli dane egzaminu już zostały wprowadzone.
#' @export
zaladuj_nowe_wyniki <- function(ctxt, p_rok, t_rodzaj, t_czesc, l_dane) {
  
  src = polacz()
  
  row = data.frame(
    rok = p_rok,
    rodzaj_egzaminu = t_rodzaj,
    czesc_egzaminu = t_czesc,
    stringsAsFactors = FALSE)
    
  if (nrow(inner_join(row, ctxt@zapisane_testy)) > 0) # Mamy juz ten test
    return(ctxt)
  
    ctxt = ctxt %>%
      .dodaj_oceny_po_poprzednich(src, l_dane, row)
    ctxt@zapisane_testy <- ctxt@zapisane_testy %>% rbind(as.data.frame(row))
  return(ctxt)
}

#' Umieszcza w danych sumaryczne wyniki z danego egzaminu. Te wyniki będą używane do
#' generowania wyników z egzaminów względem tego egzaminu.
#' 
#' UWAGA: dane, które w bazie już się znajdują zostaną zignorowane. Jeśli chcesz wprowadzić
#' je ponownie, użyj funkcji usun_sumaryczne_oceny.
#' 
#' @param ctxt Obiekt klasy DataCrunch, w którym mamy umieścić dane.
#' @param p_rok Rok egzaminu.
#' @param t_rodzaj Rodzaj egzaminu.
#' @param t_czesc Część egzaminu.
#' @param l_dane Dane pobrane przez pakiet ZPD funkcją pobierz_wyniki_egzaminu(..., czy_ewd = TRUE).
#' 
#' @return Obiekt danych DataCrunch z dodanymi danymi, jeśli dane zostały poprawnie pobrane
#' i wprowadzone do bazy. Niezmieniony obiekt, jeśli dane egzaminu już zostały wprowadzone.
#' @export
zaladuj_sumaryczne_oceny <- function(ctxt, p_rok, t_rodzaj, t_czesc, l_dane) {
  
  src = polacz()
  
  row = data.frame(
    rok = p_rok,
    rodzaj_egzaminu = t_rodzaj,
    czesc_egzaminu = t_czesc,
    stringsAsFactors = FALSE)
  
  if (nrow(inner_join(row, ctxt@zapisane_testy_oceny)) > 0) # Mamy juz ten test
    return(ctxt)
  
  ctxt = ctxt %>%
    .dodaj_sumaryczne_oceny_uczniow(src, l_dane, row)
  ctxt@zapisane_testy_oceny <- ctxt@zapisane_testy_oceny %>% rbind(as.data.frame(row))
  return(ctxt)
}

#' Usuwa wyniki danego egzaminu w zależności od poprzednich z danych.
#' @param p_rok Rok egzaminu do usunięcia.
#' @param t_rodzaj Rodzaj egzaminu do usunięcia.
#' @param t_czesc Część egzaminu do usunięcia.
#' @param ctxt Obiekt klasy DataCrunch, z którego mamy usunąć dane.
#' @return Obiekt DataCrunch z usuniętymi danymi.
#' @export
usun_wyniki <- function(ctxt, p_rok, t_rodzaj, t_czesc) {
  ctxt@wyniki_po_egz <- ctxt@wyniki_po_egz %>% filter(!(rok == p_rok &&
                                                        rodzaj_egzaminu == t_rodzaj &&
                                                        czesc_egzaminu == t_czesc))
  ctxt@zapisane_testy <- ctxt@zapisane_testy %>% filter(!(rok == p_rok &&
                                                          rodzaj_egzaminu == t_rodzaj &&
                                                          czesc_egzaminu == t_czesc))
  return(ctxt)
}

#' Usuwa sumaryczne oceny uczniów z danego egzaminu.
#' @param p_rok Rok egzaminu do usunięcia.
#' @param t_rodzaj Rodzaj egzaminu do usunięcia.
#' @param t_czesc Część egzaminu do usunięcia.
#' @param ctxt Obiekt klasy DataCrunch, z którego mamy usunąć dane.
#' @return Obiekt DataCrunch z usuniętymi danymi.
#' @export
usun_sumaryczne_oceny <- function(ctxt, p_rok, t_rodzaj, t_czesc) {
  ctxt@oceny_uczniow <- ctxt@oceny_uczniow %>% filter(!(rok == p_rok &&
                                                          rodzaj_egzaminu == t_rodzaj &&
                                                          czesc_egzaminu == t_czesc))
  ctxt@zapisane_testy_oceny <- ctxt@zapisane_testy_oceny %>% filter(!(rok == p_rok &&
                                                                      rodzaj_egzaminu == t_rodzaj &&
                                                                      czesc_egzaminu == t_czesc))
  return(ctxt)
}