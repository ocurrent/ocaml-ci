CREATE TABLE IF NOT EXISTS ci_build_index (
    owner     TEXT NOT NULL,
    name      TEXT NOT NULL,
    hash      TEXT NOT NULL,
    variant   TEXT NOT NULL,
    job_id    TEXT,
    gref      TEXT NOT NULL DEFAULT "-",
    PRIMARY KEY (owner, name, hash, variant)
);
