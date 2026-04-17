#!/bin/bash

if [ $# -ne 2 ]; then
    echo "Use: $0 <file_with_list> <final_directory>"
    echo "Example: $0 names.txt ./my_directory"
    exit 1
fi

LIST=$1
DIRECTORY=$2
EXT="ml"

if [ ! -d "$DIRECTORY" ]; then
    echo "The directory does not exist."
    exit 1
fi

while IFS= read -r name || [ -n "$name" ]; do
    if [ -z "$name" ]; then
        continue
    fi

    if [ -n "$name" ]; then
        FILE="$DIRECTORY/${name}.${EXT}"

        echo "let ${name} t = failwith \"TODO\"" > "$FILE"
        echo "Generated: $FILE"
    fi
done < "$LIST"

echo "--- Finished process ---"
