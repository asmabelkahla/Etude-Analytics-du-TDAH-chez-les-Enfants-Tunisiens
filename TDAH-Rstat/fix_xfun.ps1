# Script PowerShell pour forcer la réinstallation de xfun

Write-Host "🔧 Arrêt de tous les processus R..." -ForegroundColor Yellow
Get-Process | Where-Object {$_.ProcessName -like "*R*" -or $_.ProcessName -like "*rsession*"} | Stop-Process -Force -ErrorAction SilentlyContinue

Write-Host "🗑️ Suppression du dossier xfun..." -ForegroundColor Yellow
$xfunPath = "C:\Users\GIGABYTE\AppData\Local\R\win-library\4.5\xfun"
$lockPath = "C:\Users\GIGABYTE\AppData\Local\R\win-library\4.5\00LOCK"

if (Test-Path $lockPath) {
    Remove-Item -Path $lockPath -Recurse -Force -ErrorAction SilentlyContinue
    Write-Host "✅ Dossier LOCK supprimé" -ForegroundColor Green
}

if (Test-Path $xfunPath) {
    Remove-Item -Path $xfunPath -Recurse -Force -ErrorAction SilentlyContinue
    Write-Host "✅ Dossier xfun supprimé" -ForegroundColor Green
}

Start-Sleep -Seconds 2

Write-Host "📥 Réinstallation de xfun..." -ForegroundColor Yellow
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "install.packages('xfun', repos='https://cloud.r-project.org/', type='binary', INSTALL_opts='--no-lock')"

Write-Host ""
Write-Host "📥 Installation de knitr..." -ForegroundColor Yellow
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "install.packages('knitr', repos='https://cloud.r-project.org/', type='binary')"

Write-Host ""
Write-Host "📥 Installation de rmarkdown..." -ForegroundColor Yellow
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "install.packages('rmarkdown', repos='https://cloud.r-project.org/', type='binary')"

Write-Host ""
Write-Host "✅ Vérification..." -ForegroundColor Yellow
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "if (requireNamespace('xfun', quietly=TRUE)) cat('✅ xfun OK\n') else cat('❌ xfun ÉCHEC\n')"
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "if (requireNamespace('knitr', quietly=TRUE)) cat('✅ knitr OK\n') else cat('❌ knitr ÉCHEC\n')"
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "if (requireNamespace('rmarkdown', quietly=TRUE)) cat('✅ rmarkdown OK\n') else cat('❌ rmarkdown ÉCHEC\n')"

Write-Host ""
Write-Host "✨ Terminé!" -ForegroundColor Green
