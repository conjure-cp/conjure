
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.dba <<EOF
    SELECT
          SPEC
        , MODEL
        , PARAM
        , ATTRIBUTE
        , VALUE INT
    FROM attributes;
EOF

