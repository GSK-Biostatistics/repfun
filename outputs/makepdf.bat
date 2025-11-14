@echo off

SET USERNAME=youruid

REM: ==================================================================
REM: Find the most recently downloaded file.  Get the extension.
REM: ==================================================================
FOR /F "eol=| delims=" %%I IN ('DIR "C:\Users\%USERNAME%\Downloads\*.*" /A-D /B /O-D /TW 2^>nul') DO (
  SET "NewestFile=%%I"
  SET "Extension=%%~xI"
  GOTO FoundFile
)
ECHO No file file found!
GOTO :EOF

:FoundFile
ECHO Newest file is: "%NewestFile%"
ECHO Extension of file is: "%Extension%"
xcopy /s /Y "C:\Users\%USERNAME%\Downloads\%NewestFile%" "C:\Users\%USERNAME%\Documents\rtf_files"

REM: ==================================================================
REM: Process single rtf file.
REM: ==================================================================
if "%Extension%" == ".rtf" (
  echo Processing a single rtf file ...
)

REM: ==================================================================
REM: Process zip file.
REM: ==================================================================
if "%Extension%" == ".zip" (
  echo Processing a zip file ...
  powershell.exe -c "Expand-Archive 'C:\Users\%USERNAME%\Documents\rtf_files\%NewestFile%' -DestinationPath 'C:\Users\%USERNAME%\Documents\rtf_files' -Force"
)

REM: ==================================================================
REM: Convert the files.
REM: ==================================================================
CD /D "C:\Users\%USERNAME%\Documents\rtf_files"
for %%i in (*.rtf) do (
  powershell.exe -NoProfile -File ..\scripts\rtf2pdf.ps1 "C:\Users\%USERNAME%\Documents\rtf_files\%%i"
)

REM: ==================================================================
REM: Convert the files.
REM: ==================================================================
powershell -Command "Compress-Archive .\*.pdf -f converted.zip"

REM: ===========================================================
REM: Remove all files in directory except the converted file.
REM: ===========================================================
FOR %%a IN (".\*") DO (
  IF /i NOT "%%~nxa"=="converted.zip" DEL "%%a"
)

exit
