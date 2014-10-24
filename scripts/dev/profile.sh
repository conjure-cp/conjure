
echo "p"  ; time conjure "$1" +RTS -s -p
echo "hc" ; time conjure "$1" +RTS -s -hc && mv conjure.hp conjure-hc.hp && hp2ps -c conjure-hc.hp
echo "hm" ; time conjure "$1" +RTS -s -hm && mv conjure.hp conjure-hm.hp && hp2ps -c conjure-hm.hp
echo "hd" ; time conjure "$1" +RTS -s -hd && mv conjure.hp conjure-hd.hp && hp2ps -c conjure-hd.hp
echo "hy" ; time conjure "$1" +RTS -s -hy && mv conjure.hp conjure-hy.hp && hp2ps -c conjure-hy.hp
echo "hr" ; time conjure "$1" +RTS -s -hr && mv conjure.hp conjure-hr.hp && hp2ps -c conjure-hr.hp
echo "hb" ; time conjure "$1" +RTS -s -hb && mv conjure.hp conjure-hb.hp && hp2ps -c conjure-hb.hp
