@ECHO OFF
cls
tasm /z main /r /w2 /q /p
tlink main /x /3
main

