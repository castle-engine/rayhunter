#! /bin/sh
set -eu

# This is automatically generated script that should compile
# all programs in this archive. It simply calls FPC
# with proper command-line options.
#
# We must do cd ../castle_game_engine/ (and call FPC from that directory)
# because castle-fpc.cfg file is there and it contains paths relative
# to that directory.

cd ../castle_game_engine/

fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../rayhunter/rayhunter.lpr
