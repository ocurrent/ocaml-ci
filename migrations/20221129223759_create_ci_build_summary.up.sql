CREATE TABLE IF NOT EXISTS ci_build_summary (
    owner TEXT NOT NULL,
    name  TEXT NOT NULL,
    hash  TEXT NOT NULL,
    gref  TEXT NOT NULL,
    build_number INT NOT NULL DEFAULT 1,
    status INT8 NOT NULL DEFAULT 0,
    started_at FLOAT,
    total_ran_for FLOAT,
    ran_for FLOAT,
    total_queued_for FLOAT NOT NULL DEFAULT 0,
    created_at DATETIME NOT NULL,
    PRIMARY KEY (owner, name, hash, build_number)
);