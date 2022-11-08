module M_Gitlab = struct
  let prefix = "gitlab"
end

include Git_forge.Make (M_Gitlab)
