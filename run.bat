@echo off
cd ..
IF "%1"=="64" GOTO UNO
IF "%1"=="32" GOTO DUE

:UNO
ECHO 64
layer-64.bat

:DUE
ECHO 32
layer-32.bat