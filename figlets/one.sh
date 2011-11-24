
touch "$1.htmlpart"
echo "" > "$1.htmlpart"

echo "<p>$1</p>"   >> "$1.htmlpart"
echo "<pre>"       >> "$1.htmlpart"
echo ""            >> "$1.htmlpart"
cat  "$1"          >> "$1.htmlpart"
echo ""            >> "$1.htmlpart"
echo "</pre>"      >> "$1.htmlpart"
echo ""            >> "$1.htmlpart"
