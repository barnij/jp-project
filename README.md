# Projekt z przedmiotu Języki Programowania
Bartosz Jaśkiewicz, 307893

### Zakres wykonanej pracy
Wykonane elementy w projekcie: lekser, parser, normalizacja termów, implementacja rachunku lambda jako języka programowania, sprawdzanie Beta-równości w naiwny sposób, odczytywanie i wypisywanie do plików.

## Uruchomienie
```sh
    bash setup.sh
```
```sh
    ./main
```
Program `main` bez argumentów pobiera znaki do interpretera ze standardowego wejścia.

Podając JEDEN argument - ścieżkę do pliku - program odczyta, zapisany w nim term i uruchomi normalizację.

Podając DWA argumenty - ścieżki do plików - program odczyta z nich termy i sprawdzi ich Beta-równownoważność.

W każdym przypadku, wynik zostanie umieszczony w pliku `result.txt`, a dodatkowe informacje na standardowe wyjście.

### Czyszczenie plików
```
    make clean
```

## Testowanie
Uruchom testy:
```
    bash test.sh
```
Uruchom testy z dodatkowymi informacjami:
```
    bash test.sh -v
```

## Składnia języka
W języku obowiązują termy zdefiniowane jako *e* w opisie zadania projektu. Polecane jest nawiasowanie termów. Term **λ** może zostać zastąpiony słowem **lambda**.

---
2022
