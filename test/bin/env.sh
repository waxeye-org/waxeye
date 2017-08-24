set -euo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

source "${BASH_SOURCE%/*}/lib-waxeye.sh"

# Path manipulation

abspath() {
  echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
}

# Logging

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET_COLOR='\033[0m'

TEST_NAME="$(ps --no-headers -o command $PPID | cut -d' ' -f3 -s)"
LOG_PREFIX="$TEST_NAME"
if [ ! -z "$LOG_PREFIX" ]; then
  LOG_PREFIX+=': '
fi

log_success() {
  echo >&2 -e "${GREEN}${LOG_PREFIX}${@}${RESET_COLOR}"
}

log_error() {
  echo >&2 -e "${RED}${LOG_PREFIX}${@}${RESET_COLOR}"
}

# Status reporting

_report() {
  if [ $? -eq 0 ]; then
    log_success PASS
  else
    if [[ ! -z "$_ERR_COMMAND" ]]; then
      log_error "ERROR: Aborting at line: $_ERR_LINENO on command: $_ERR_COMMAND";
    fi
    log_error FAIL
  fi
}

_CURRENT_COMMAND=
_ERR_COMMAND=
_ERR_LINENO=
_remember_error() {
  _CURRENT_COMMAND="$BASH_COMMAND";
  if [[ -z "$_ERR_COMMAND" ]]; then
    _ERR_COMMAND="$_CURRENT_COMMAND";
    _ERR_LINENO="$LINENO";
  fi
}

if [ ! -z "${VERBOSE+1}" ]; then
  trap _remember_error ERR
fi
trap _report EXIT

