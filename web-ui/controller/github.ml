module View : Git_forge_s.View = struct
  module Build_status = View.Build_status

  let prefix = "github"
  include View.Github
end

include Git_forge.Make(View)

