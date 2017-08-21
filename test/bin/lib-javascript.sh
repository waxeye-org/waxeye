set -euo pipefail
source "${BASH_SOURCE%/*}/lib-waxeye.sh"

: ${NODE_BIN:="$(which node || which nodejs || log_error Please install nodejs && false)"}
: ${NODE_CMD:="env NODE_PATH=src/javascript/" "$NODE_BIN"}

gen_javascript_parser() {
  rm -rf tmp/js/test_parser.js
  mkdir -p tmp/js
  run_waxeye -g javascript tmp/js/ -p test ./test/fixtures/env1.waxeye > /dev/null
}
