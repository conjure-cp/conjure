rm -rf conjure-output *.solution
conjure solve --verbose-trail --rewrites-trail 517.essence 2>&1 | grep "Malformed JSON in a cached Essence Prime model."
rm -rf conjure-output *.solution
