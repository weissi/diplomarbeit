#!/bin/bash

echo '\\'
echo '{\tiny'
echo '\hfill Repository Hashes: '
git log --pretty='\texttt{tree:\ %T}, \texttt{last commit:\ %H} \\' -n1 2>&1
echo '\begin{verbatim}'
git status --short 2>&1
echo '\end{verbatim}'
echo '}'
