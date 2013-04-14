
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.dba <<EOF
    INSERT INTO attributes
        ( SPEC
        , MODEL
        , PARAM
        , ATTRIBUTE
        , VALUE
        )
    VALUES
        ( "$1"
        , "$2"
        , "$3"
        , "$4"
        , "$5"
        );
EOF

