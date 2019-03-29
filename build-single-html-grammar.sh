#!/bin/sh
FN='sqlite-full-grammar.html'
echo "<html><head><style>h1{text-align:left;}div{page-break-inside:avoid;}</style></head><body>" > $FN
for FILE in sqlite-doc/images/syntax/*gif; do
  RULE=$(basename ${FILE%%.*} | sed s/-/\ /g)
  DATA=$(base64 $FILE)
  echo "<div><h1>$RULE:</h1><img src='data:image/png;base64, $DATA'/></div>" >> $FN
done
echo "</body></html>" >> $FN

