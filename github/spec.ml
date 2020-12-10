(* Converting OBuilder Spec to Github Workflow *)
open Workflow
open Yaml_util 


type ctx = {
  user : Obuilder_spec.user;
}

let default_ctx = {
  user = Obuilder_spec.root;
}

type job = { job : Types.job; }

type t = job Types.t 

let job_to_yaml t : Yaml.value = `O [ ("job", Types.job_to_yaml t.job) ]
let job_of_yaml yaml : job = match yaml with 
  | `O [ ("job", job) ] -> (
    match Types.job_of_yaml job with 
      | Ok v -> { job = v }
      | Error (`Msg m) -> failwith m
  )
  | _ -> failwith "Bad yaml"


let cp src dest = Format.(fprintf str_formatter "cp %a %s" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "%s" " ") pp_print_string) src dest); Format.flush_str_formatter ()

let working_dir d st = match d with  
  | Some s -> st |> with_step_workdir s 
  | None -> st 

let rec of_op wd acc env ctx (ts : Obuilder_spec.op list) = match ts with
  | (`Comment _) :: xs -> of_op wd acc env ctx xs
  | (`Workdir x) :: xs -> of_op (Some x) ((step |> with_step_run ("mkdir -p " ^ x)) :: acc) env ctx xs
  | (`Shell _) :: xs -> of_op wd acc env ctx xs
  | (`Run { shell; _ }) :: xs -> of_op wd ((step |> with_step_run shell |> working_dir wd) :: acc) env ctx xs 
  | (`Copy { src; dst; exclude = _ }) :: xs -> of_op wd ((step |> with_step_run @@ cp src dst)::acc) env ctx xs 
  | (`User (_ as u)) :: xs -> of_op wd acc env { user = u } xs 
  | (`Env b) :: xs -> of_op wd acc ((fst b, `String (snd b)) :: env) ctx xs 
  | [] -> (List.rev acc, env, ctx)

let container image = 
  container 
  |> with_image image 
  |> with_options "--user 1000" 
  |> with_container_env 
    (simple_kv [
      ("HOME", `String "/home/opam");
    ])

let dockerless_worfklow ~oses ~ovs ops =  
  let matrix =
    simple_kv
      [
        ("operating-system", list string oses);
        ("ocaml-version", list string ovs);
      ]
  in
  let checkout = { step with uses = Some Conf.checkout } in
  let setup =
    step
    |> with_uses "avsm/setup-ocaml@v1"
    |> with_with
         (simple_kv [ ("ocaml-version", string (expr "matrix.ocaml-version")) ]) in 
  let steps = [ checkout; setup ] @ ops in 
    job (expr "matrix.operating-system")
    |> with_strategy (strategy |> with_matrix matrix)
    |> with_steps steps 

let workflow_of_spec ?(oses=Conf.oses) ?(ovs=["4.11.0"]) ?(use_docker=true) { Obuilder_spec.from; ops } =
  let (ops', env, _) = of_op None [] [] default_ctx ops in
  let on = simple_event ["push"; "pull_request"] in 
  let job = if use_docker then (
    job "ubuntu-latest" 
    |> with_steps ops' 
    |> with_container (container from)
    |> with_job_env (simple_kv (("HOME", `String "/home/opam") :: env)) 
    |> with_job_defaults (with_default_run (run |> with_run_workdir "/home/opam"))) 
  else 
    dockerless_worfklow ~oses ~ovs ops' 
    |> with_job_env (simple_kv env)
    |> with_job_defaults (with_default_run (run |> with_run_shell "bash")) in 
    t { job } |> with_name "Github Action Workflow" |> with_on on

let pp ppf t = Pp.workflow ~drop_null:true job_to_yaml ppf t

let to_string t = 
  Pp.workflow ~drop_null:true job_to_yaml Format.str_formatter t;
  Format.flush_str_formatter ()
