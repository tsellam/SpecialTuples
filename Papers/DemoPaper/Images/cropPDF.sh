#!/bin/bash

for file in *.pdf
do
    pdfcrop ${file} TMP
    mv TMP ${file}
done
