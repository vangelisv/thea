@echo off

set THEA_PATH=%~dp0\..

swipl.exe -L0 -G0 -T0 -q -g main,halt -t halt -s %THEA_PATH%\bin\thea-owl-i -- %*