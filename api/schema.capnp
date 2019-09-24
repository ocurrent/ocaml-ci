@0x9ac524e0ec04d45e;

using OCurrent = import "ocurrent.capnp";

struct RefInfo {
  ref  @0 :Text;
  hash @1 :Text;
}

interface Repo {
  refs         @0 () -> (refs :List(RefInfo));
  # Get the set of branches and PRs being monitored.

  jobOfCommit  @1 (hash :Text) -> (job :OCurrent.Job);
  # The hash doesn't need to be the full hash, but must be at least 6 characters long.

  jobOfRef     @2 (ref :Text) -> (job :OCurrent.Job);
  # ref should be of the form "refs/heads/..." or "refs/pull/4/head"
}

interface Org {
  repo         @0 (name :Text) -> (repo :Repo);
}

interface CI {
  org          @0 (owner :Text) -> (org :Org);
}
