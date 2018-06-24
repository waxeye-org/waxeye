set -euo pipefail
source "${BASH_SOURCE%/*}/lib-waxeye.sh"

: ${NODE_BIN:="$(which node || which nodejs || log_error Please install nodejs && false)"}
: ${NODE_CMD:="env NODE_PATH=src/javascript/" "$NODE_BIN"}
: ${TS_NODE_BIN:="src/javascript/node_modules/.bin/ts-node"}
: ${TS_NODE_CMD:="env NODE_PATH=src/javascript/" "$TS_NODE_BIN" "--project test/javascript/tsconfig.json"}

gen_javascript_parser() {
  rm -rf tmp/js/test_parser.js
  mkdir -p tmp/js
  run_waxeye -g javascript tmp/js/ -p test ./test/fixtures/env1.waxeye > /dev/null
}

gen_typescript_parser() {
  rm -rf tmp/ts/test_parser.ts
  mkdir -p tmp/ts
  run_waxeye -g typescript tmp/ts/ -p test ./test/fixtures/env1.waxeye > /dev/null
}
