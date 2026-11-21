#!/bin/sh
set -eu

cd "$(dirname "$0")"
mkdir -p output
cp input/* output/
cd output

# Order follows the PREPRO 2026 VERIFY-ALL.BAT supplied with RUN.LINUX.
for program in endf2c linear recent sigma1 activate legend fixup sixpak \
               spectra dictin groupie virgin evalhard1 comhard1; do
    printf 'Running %s\n' "$program"
    if ! ../../bin/"$program" > "$program.log" 2>&1; then
        tail -n 30 "$program.log"
        exit 1
    fi
    if [ "$program" = evalhard1 ] || [ "$program" = comhard1 ]; then
        mv PLOT0001.ps "$program.ps"
    fi
done

for file in ENDFB.OUT ENDFB.LINEAR ENDFB.RECENT ENDFB.SIGMA1 \
            ENDFB.ACTIVATE ENDFB.LEGEND ENDFB.FIXUP ENDFB.DICTIN \
            GROUPIE.LST evalhard1.ps comhard1.ps; do
    if [ ! -s "$file" ]; then
        printf 'Missing or empty output: %s\n' "$file" >&2
        exit 1
    fi
done

if [ "${1:-}" = graphics ]; then
    for program in evalplot complot; do
        printf 'Running %s\n' "$program"
        if ! ../../bin/"$program" > "$program.log" 2>&1; then
            tail -n 30 "$program.log"
            exit 1
        fi
    done
fi

printf 'Verification finished; logs and outputs are in test/output.\n'
