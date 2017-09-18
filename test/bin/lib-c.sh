set -euo pipefail
source "${BASH_SOURCE%/*}/lib-waxeye.sh"

C_DEFAULT_PARSER_NAME=test

build_c_parser() {
  local -r grammar_path="${1:-./test/fixtures/env1.waxeye}"
  local -r parser_name="${2:-$C_DEFAULT_PARSER_NAME}"
  local -r parser_dir="$(c_parser_dir "$parser_name")"
  rm -rf "$parser_dir"
  mkdir -p "$parser_dir"
  local -r abs_parser_dir="$(abspath "$parser_dir")"

  run_waxeye -g c "$parser_dir" "$grammar_path" > /dev/null

  PARSER_C="${abs_parser_dir}/parser.c" \
  DISPLAY_AST_OUT="${abs_parser_dir}/display_ast" \
  C_INCLUDE_PATH=":$abs_parser_dir" \
    make -s --directory=src/c display_ast
}

c_parser_dir() {
  local -r parser_name="${1:-$C_DEFAULT_PARSER_NAME}"
  echo "tmp/c/${parser_name}_parser"
}

display_c_ast() {
  local -r parser_name="${1:-$C_DEFAULT_PARSER_NAME}"
  "$(c_parser_dir "$parser_name")/display_ast"
}
