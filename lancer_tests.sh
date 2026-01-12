#!/bin/bash

echo "========================================"
echo "  COMPILATION DU ROUTEUR"
echo "========================================"
gnatcleanall
gnatmake routeur_LA.adb
if [ $? -ne 0 ]; then
    echo "ERREUR DE COMPILATION"
    exit 1
fi
echo

echo "========================================"
echo "  TEST 1: FIFO avec cache=5"
echo "========================================"
./routeur_LA -c 5 -p FIFO -t table.txt -r resultats_test1.txt test_paquets.txt
echo

echo "========================================"
echo "  TEST 2: LRU avec cache=3"
echo "========================================"
./routeur_LA -c 3 -p LRU -t table.txt -r resultats_test2.txt test_paquets.txt
echo

echo "========================================"
echo "  TEST 3: LFU avec cache=10"
echo "========================================"
./routeur_LA -c 10 -p LFU -t table.txt -r resultats_test3.txt test_paquets.txt
echo

echo "========================================"
echo "  TEST 4: Cache minimal (1)"
echo "========================================"
./routeur_LA -c 1 -p FIFO -t table.txt -r resultats_test4.txt paquets.txt
echo

echo "========================================"
echo "  TEST 5: Sans affichage stats (-S)"
echo "========================================"
./routeur_LA -c 5 -p FIFO -t table.txt -r resultats_test5.txt -S paquets.txt
echo

echo "========================================"
echo "  TOUS LES TESTS TERMINES"
echo "========================================"
