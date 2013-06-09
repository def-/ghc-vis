#!/usr/bin/env zsh

rm *.hi *.o
ghci -ghci-script create.ghci
rm *.hi *.o

for i in *svg; do
  inkscape -z -e ${i:r}.png -d 60 -b white $i

  #gzip -9 $i
  #mv $i.gz ${i:r}.svgz

  pngnq ${i:r}.png
  optipng -o7 ${i:r}-nq8.png
  mv ${i:r}-nq8.png ${i:r}.png
done

for i in **/*.txt; do
  asciidoc -b xhtml11 -f asciidoc.conf -a disable-javascript -a data-uri -a stylesheet=$PWD/asciidoc-toc.css -a idprefix -o - $i | hxtoc -l 2 > ${i:r}.html
done
