include Git_forge_intf

module Make (F : Forge) : View = struct
  module Repo = Repo.Make (F)
  module Ref = Ref.Make (F)
  module Step = Step.Make (F)
  module History = History.Make (F)

  let prefix = F.prefix
  let request_prefix = F.request_prefix
  let list_history = History.list
  let list_repos = Repo.list
  let list_refs = Ref.list
  let list_steps = Step.list
  let show_step = Step.show
  let show_step_raw = Step.show_raw

  include Message
end
