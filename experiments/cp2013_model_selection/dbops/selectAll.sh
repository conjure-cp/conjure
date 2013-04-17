
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.db <<EOF
    SELECT
          SPEC
        , MODEL
        , PARAM
        , ATTRIBUTE
        , VALUE INT
    FROM attributes;
EOF

