set -euo pipefail

run_waxeye() {
  racket -S src/scheme ./src/waxeye/waxeye.scm "$@"
}
