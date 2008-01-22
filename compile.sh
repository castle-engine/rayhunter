#! /bin/sh
set -eu

# This is automatically generated script that should compile
# all programs in this archive. It simply calls FPC
# with proper command-line options.
#
# We must do cd ../kambi_vrml_game_engine/ (and call FPC from that directory)
# because kambi.cfg file is there and it contains paths relative
# to that directory.

cd ../kambi_vrml_game_engine/

fpc -dRELEASE @kambi.cfg ../rayhunter/rayhunter.pasprogram
