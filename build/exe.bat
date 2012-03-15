
MKDIR tmp
C:\"Program Files\Racket\mzc.exe" --exe waxeye src\waxeye\waxeye.scm
MOVE waxeye.exe tmp\waxeye.exe
C:\"Program Files\Racket\mzc.exe" --exe-dir . tmp\waxeye.exe
RMDIR /S tmp
