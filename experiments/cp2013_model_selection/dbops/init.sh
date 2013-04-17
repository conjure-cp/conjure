
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.db <<EOF
    CREATE TABLE attributes
        ( SPEC
        , MODEL
        , PARAM
        , ATTRIBUTE
        , VALUE REAL
        );
EOF

