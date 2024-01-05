SELECT cb.

SELECT cbi.variant, cbs.status, COUNT(*) FROM ci_build_index cbi
INNER JOIN (SELECT owner, name, hash, status, MAX(build_number) FROM
ci_build_summary GROUP BY owner, name, hash) cbs ON cbi.owner = cbs.owner
AND cbi.name = cbs.name AND cbi.hash = cbs.hash GROUP BY cbi.variant, cbs.status;

-- Complete
SELECT cbi.variant, cbs.status, COUNT(*) FROM ci_build_index cbi INNER JOIN (SELECT owner, name, hash, MAX(build_number), status FROM ci_build_summary GROUP BY owner, name, hash) cbs ON cbi.owner = cbs.owner AND cbi.name = cbs.name AND cbi.hash = cbs.hash GROUP BY cbi.variant, cbs.status;

-- summary select
SELECT owner, name, hash, MAX(build_number), status FROM ci_build_summary GROUP BY owner, name, hash;

-- Inner join
SELECT cbi.variant, cbs.status FROM ci_build_index cbi INNER JOIN (SELECT owner, name, hash, status, MAX(build_number) FROM ci_build_summary GROUP BY owner, name, hash) cbs ON cbi.owner = cbs.owner AND cbi.name = cbs.name AND cbi.hash = cbs.hash;

CREATE INDEX index_summary ON ci_build_summary (build_number, owner, name, hash);

* e4dc7bc - (HEAD -> prometheus-step-aggregate) Query research, fix dune and gitignore (3 hours ago)
* 22a2c83 - Take only latest build (5 days ago)
* a0bba18 - Add aggregated variant state metric (6 days ago)
