@0x9ac524e0ec04d45e;

using OCurrent = import "ocurrent.capnp";

enum BuildStatus {
  notStarted @0;
  passed     @1;
  failed     @2;
  pending    @3;
}

struct RefInfo {
  ref         @0 :Text;
  hash        @1 :Text;
  state       @2 :BuildStatus;
  started     :union {
    ts        @3 :Float64;
    none      @4 :Void;
  }
  # The state of the ref's head commit
}

struct JobInfo {
  variant @0 :Text;
  state :union {
    notStarted @1 :Void;

    passed     @2 :Void;

    failed     @3 :Text;
    # The text is the error message.

    active     @4 :Void;
    # The job is still running.

    aborted    @5 :Void;
    # This means we couldn't find any record of the job. It probably means
    # that the server crashed while building, and when it came back up we
    # no longer wanted to test that commit anyway.
  }

  queuedAt  :union {
    ts         @6 :Float64;
    # timestamp as seconds since epoch
    none       @7 :Void;
  }

  startedAt :union {
    ts         @8 :Float64;
    # timestamp as seconds since epoch
    none       @9 :Void;
  }

  finishedAt :union {
    ts         @10 :Float64;
    # timestamp as seconds since epoch
    none       @11 :Void;
  }
}


interface Commit {
  jobs  @0 () -> (jobs :List(JobInfo));

  jobOfVariant @1 (variant :Text) -> (job :OCurrent.Job);

  refs @2 (hash :Text) -> (refs :List(Text));
  # Get the set of branches and PRs with this commit at their head.

  status @3 () -> (status :BuildStatus);
}

interface Repo {
  refs         @0 () -> (refs :List(RefInfo));
  # Get the set of branches and PRs being monitored.

  obsoleteJobOfCommit  @1 (hash :Text) -> (job :OCurrent.Job);
  obsoleteJobOfRef     @2 (ref :Text) -> (job :OCurrent.Job);
  obsoleteRefsOfCommit @3 (hash :Text) -> (refs :List(Text));

  commitOfHash @4 (hash :Text) -> (commit :Commit);
  # The hash doesn't need to be the full hash, but must be at least 6 characters long.

  commitOfRef @5 (ref :Text) -> (commit :Commit);
  # ref should be of the form "refs/heads/..." or "refs/pull/4/head"

  historyOfRef @6 (ref :Text) -> (refs :List(RefInfo));
  # ref should be of the form "refs/heads/..." or "refs/pull/4/head"
}

struct RepoInfo {
  name @0 :Text;
  masterState @1 :BuildStatus;
  # The status of the repository's master branch (notStarted if there isn't one)
}

interface Org {
  repo         @0 (name :Text) -> (repo :Repo);

  repos        @1 () -> (repos :List(RepoInfo));
  # Get the list of tracked repositories for this organisation.
}

interface CI {
  org          @0 (owner :Text) -> (org :Org);

  orgs         @1 () -> (orgs :List(Text));
  # Get the list of organisations for this CI capability.
}

interface Log {
  write @0 (msg :Text);
}

interface Solver {
  solve @0 (request :Text, log :Log) -> (response :Text);
}
