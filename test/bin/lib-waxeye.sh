set -euo pipefail

run_waxeye() {
  racket -S src/racket ./src/waxeye/waxeye.rkt "$@"
}
