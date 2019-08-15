rm -rf conjure-output

conjure modelling 1.essence ; head -n30 conjure-output/model000001.eprime
conjure modelling 2.essence ; head -n30 conjure-output/model000001.eprime
conjure modelling 3.essence ; head -n30 conjure-output/model000001.eprime

# conjure solve 1.essence m_is_3.param ; cat *.solution ; rm -f *.solution
# conjure solve 2.essence m_is_3.param ; cat *.solution ; rm -f *.solution
conjure solve 3.essence m_is_3.param ; cat *.solution ; rm -f *.solution

rm -rf conjure-output *.solution
