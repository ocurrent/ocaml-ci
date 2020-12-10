open Workflow 

type job = { job : Types.job; }

val job_to_yaml : job -> Yaml.value 

val job_of_yaml : Yaml.value -> job

type t = job Types.t

val pp : Format.formatter -> t -> unit

val to_string : job Types.t -> string

val workflow_of_spec : 
  ?oses:string list ->
  ?ovs:string list -> 
  ?use_docker:bool -> 
  Obuilder_spec.stage -> job Types.t