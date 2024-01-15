@echo off
powershell.exe -Command "Start-Process powershell.exe -ArgumentList '-NoExit','-Command sbcl --script recetas.lisp'"
