
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.dba <<EOF
    CREATE TABLE attributes
        ( SPEC
        , MODEL
        , PARAM
        , ATTRIBUTE
        , VALUE INT
        );
EOF

