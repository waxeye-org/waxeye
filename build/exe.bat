SET PLTCOLLECTS=";%~dp0..\src\scheme\"
"C:\Program Files\Racket\raco.exe" exe "%~dp0..\src\waxeye\waxeye.rkt"
"C:\Program Files\Racket\raco.exe" distribute . "%~dp0..\src\waxeye\waxeye.exe"
DEL "%~dp0..\src\waxeye\waxeye.exe"
