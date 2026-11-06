@echo off
setlocal

REM Launch a IdrAgra simulation that pauses on termination and saves console output to IdrAgra_console.log

REM launching this .bat file runs a IdrAgra simulation in the current directory.
REM It looks for the exe in these locations, in order:
REM 1. the current directory;
REM 2. the "release" folder in the parent directory (useful for the repo's demo);
REM 3. the parent directory.
REM The exe can be named anything starting with "idragra", e.g. IdrAgra.exe, IdrAgra_v2.3.1.exe...

REM Optionally, add IdrAgra command-line arguments here.
REM Example: set "IDRAGRA_ARGS=-verbose -f" (or "IDRAGRA_ARGS=" to disable them)
set "IDRAGRA_ARGS="

set "LOGFILE=%~dp0IdrAgra_console.log"

REM Try current directory
for %%F in ("%~dp0IdrAgra*.exe") do (
    if exist "%%~fF" (
        call :run "%%~fF"
        goto :done
    )
)

REM Try parent\release
for %%F in ("%~dp0..\release\IdrAgra*.exe") do (
    if exist "%%~fF" (
        call :run "%%~fF"
        goto :done
    )
)

REM Try parent directory
for %%F in ("%~dp0..\IdrAgra*.exe") do (
    if exist "%%~fF" (
        call :run "%%~fF"
        goto :done
    )
)

echo No IdrAgra executable was found.
echo Searched:
echo   %~dp0IdrAgra*.exe
echo   %~dp0..\release\IdrAgra*.exe
echo   %~dp0..\IdrAgra*.exe

goto :done


REM after finding IdrAgra, launch it
:run
echo Logging console output to:
echo   %LOGFILE%
echo.

powershell.exe -NoProfile -Command ^
    "& '%~1' 2>&1 | Tee-Object -FilePath '%LOGFILE%'"

exit /b


:done
pause