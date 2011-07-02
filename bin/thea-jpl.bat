@echo off
REM This script assumes the bin directory of SWI-Prolog to be 
REM in %PATH%.  If this is not the case, you may wish to set %PATH%
REM in this script.

set THEA_PATH=%~dp0\..
set THEA_JARS_PATH=%THEA_PATH%\jars

REM Find the Prolog coordinates
swipl.exe -dump-runtime-variables=cmd > %TEMP%\plrtvars.bat
call %TEMP%\plrtvars.bat
del %TEMP%\plrtvars.bat
set SWI_HOME_DIR=%PLBASE%

REM Find classpath for jpl.jar.  First case holds if we are in the source tree.
if exist ..\..\..\jpl.jar (
  set CLASSPATH=..\..\..\jpl.jar
) else (
  set CLASSPATH=%PLBASE%\lib\jpl.jar
)

REM add thea jars to classpath (owlapi, pellet etc.)
setLocal EnableDelayedExpansion

set CLASSPATH="%CLASSPATH%
for /R %THEA_JARS_PATH% %%a in (*.jar) do (
  set CLASSPATH=!CLASSPATH!;%%a
)
set CLASSPATH=!CLASSPATH!"

echo %CLASSPATH%

REM run thea
swipl.exe -L0 -G0 -T0 -q -g main,halt -t halt -s %THEA_PATH%\bin\thea-owl-i -- --ensure_loaded "library(thea/owl2_java_owlapi)" %*

REM test with: 
REM C:\thea2>bin\thea-jpl.bat --reasoner pellet testfiles/country.owl --reasoner-query select "p(P,A,B)" where "propertyAssertion(P,A,B)" --prolog
REM
REM if you get an error saying:...
REM ERROR: c:/programme/prolog/library/jpl.pl:4637:
REM        setenv/2: error in system call (Filename too long)
REM ... then your %CLASSPATH% is too long (try moving thea to C:\thea)

pause