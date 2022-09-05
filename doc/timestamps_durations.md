## Timestamps and durations

Notes to cover design decisions and conventions adopted for the calculation of timestamps and durations in ocaml-ci.

1. All timestamps are encoded as floats representing seconds from epoch. Durations are floats representing seconds (between timestamps).
2. For a step -
    - `queued_at` is the timestamp that the step was first created and thus enqueued.
    - `started_at` is the timestamp that the step started running at. Note that the step may never run so this may be empty.
    - `finished_at` is the timestamp that the step finished. It may have successfully finished but it may also have been aborted or cancelled. This timestamp indicates the point at which it transitioned to a final state.
    - A step can be `Cached` -- that is, it does not run but previous results are used. This is indicated by the `finished_at` timestamp of the step being less than the `build-created-at` timestamp.
    - A step has durations `queued_for` and `ran_for` to indicate the time spent waiting to be run, and then running, respectively. A cached step will have 0 durations (since it will not have run).
    - A step's `run-time` is a duration that is the sum of its `queued_for` and `ran_for` durations.
2. For a build (described by a list of steps):
    - The `build-created-at` timestamp is the `queued_at` timestamp that of the __Analysis__ step. This has to do with the characteristic that all ocaml-ci pipelines start with the `analysis` step.
    - The `build-ran-for` duration is the total of the run-times of the steps that constitute the build.

### Implementation details:
- Timestamps are persisted at the `current_cache` layer. They are not part of the `index`

- There is also a `Job_map` that collects information about running jobs (including timestamps).

- A build is derived from information in the `index` -- specifically, a list of jobs (aka steps) that correspond to the _most recent runs_ of each variant that correspond to an (owner, name, hash).

- As of the first implementation of timestamps and durations, it is not clear when timestamps are first recorded and thus available to the system. It is possible that a step will run but not show any timestamps until it has finished. This needs revisiting.

- When we come to calculating the average run-time of a build (over the last n commits), we may want to use the `build-number` that is persisted in the `cache` to calculate the largest `build-ran-for` duration per commit.