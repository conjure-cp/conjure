
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.db <<EOF
    CREATE TABLE IF NOT EXISTS attributes
        ( SPEC
        , MODEL
        , PARAM
        , ATTRIBUTE
        , VALUE REAL
        , PRIMARY KEY (SPEC, MODEL, PARAM, ATTRIBUTE)
        );
EOF

