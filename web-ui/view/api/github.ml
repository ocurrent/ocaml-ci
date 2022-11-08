module M_Github = struct
  let prefix = "github"
end

include Git_forge.Make (M_Github)
