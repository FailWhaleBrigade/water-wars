#!/bin/bash 
i=2
for file in *.png
do
    mv -i "mermaid$i.png" "mermaid$((i-1)).png"
    i=$((i+1))
done
