
ls `figlet -I2`/*.flf | parallel --no-notice "figlet -w 100 -f {.} Conjure > {/.}.figlet"
ls *.figlet | xargs -I {} sh one.sh {}

cat index-header.html >  index.html
cat *.htmlpart        >> index.html
cat index-footer.html >> index.html
