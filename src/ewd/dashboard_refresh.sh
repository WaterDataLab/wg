#!/bin/bash

echo "Rendering the dashboard..."
Rscript -e "source('./src/ewd/05_render.R');"
echo "done."

echo "Encrypting the dashboard..."
Rscript -e "source('./src/ewd/06_encrypt.R');"
echo "done."

if [[ "$(git status --porcelain)" != "" ]]; then
    git config --global user.name 'RichPauloo'
    git config --global user.email 'richpauloo@gmail.com'
    git add content/ewd/index.html
    git commit -m "Auto update dashboard"
    git push
fi
