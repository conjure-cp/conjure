# timestamp: 20140227-160145
function check_errors {
    find experiments -name "*.error*" ; parallel head -n 1 {} ::: $(find experiments -name "*.stderr") | grep -v allocated
}

# timestamp: 20140227-160255
function rsync_from_eno {
    time rsync -av --exclude "*.minion" --exclude "*.minion.aux" --exclude ".MINIONS*" ozgur@eno.cs.st-andrews.ac.uk:repos/stacs_cp/conjure/experiments . 
}

# timestamp: 20140227-162511
function nb_eprimes {
    parallel -k "echo {} ; ls -l {}/*.eprime 2> /dev/null | wc -l" ::: $(find experiments -type d -name "*DontCare")
}

# timestamp: 20140227-163023
function nb_eprimes {
    parallel -k "echo -n {} ; ls -l {}/*.eprime 2> /dev/null | wc -l" ::: $(find experiments -type d -name "*DontCare")
}

# timestamp: 20140227-163315
function nb_eprimes {
    parallel "echo -ne '{}\t' ; ls -l {}/*.eprime 2> /dev/null | wc -l | tr -d ' '" ::: $(find experiments -type d -name "*DontCare") 
}

# timestamp: 20140228-074725
function check_errors {
    find experiments -name "*.error*" ; parallel head -n 1 {} ::: $(find experiments -name "*stderr") | grep -v allocated
}

# timestamp: 20140228-111632
function forTable_minionTimedOut {
    grep "Time out." experiments/dontcare/all-combinations/*/compact/*DontCare.minion-stdout | sort
}

# timestamp: 20140228-111707
function forTable_minionNodes {
    grep "Nodes" experiments/dontcare/all-combinations/*/compact/*DontCare.minion-stdout | sort
}

# timestamp: 20140228-111740
function forTable_minionSolutions {
    grep "Solutions Found" experiments/dontcare/all-combinations/*/compact/*DontCare.minion-stdout | sort
}

function forTable_updateNumbers {
    forTable_minionSolutions > ../numbers/forTable_minionSolutions.txt
    forTable_minionNodes     > ../numbers/forTable_minionNodes.txt
    forTable_minionTimedOut  > ../numbers/forTable_minionTimedOut.txt
}
# timestamp: 20140301-104009
function rsync_from_eno {
    time rsync -av --checksum --exclude "*.minion" --exclude "*.minion.aux" --exclude ".MINIONS*" ozgur@eno.cs.st-andrews.ac.uk:repos/stacs_cp/conjure/experiments .
}

