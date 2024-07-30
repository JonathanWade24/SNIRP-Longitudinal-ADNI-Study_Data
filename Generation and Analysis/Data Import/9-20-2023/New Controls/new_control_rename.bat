@echo off
setlocal enabledelayedexpansion

set "directory=C:\Users\Jonathan Wade\Documents\GitHub\SNIRP-Thesis\ROI-Flat File Generation\Data Import\9-20-2023\New Controls"

for /r "%directory%" %%f in (*.csv) do (
    set "filename=%%~nf"
    set "extension=%%~xf"
    ren "%%f" "!filename!_new_controls!extension!"
)

echo All CSV files in %directory% and its subdirectories have been renamed.
pause
