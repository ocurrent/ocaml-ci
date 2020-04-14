open Current.Syntax

let pull ~schedule platform =
  Current.component "docker pull" |>
  let> { Platform.builder; variant; label = _ } = platform in
  Builder.pull builder ("ocurrent/opam:" ^ variant) ~schedule

let build ~repo ~analysis ~platform ~base source =
  Current.component "build" |>
  let> { Platform.builder; variant; _ } = platform
  and> analysis = analysis
  and> base = base
  and> source = source
  and> repo = repo in
  let base = Current_docker.Raw.Image.hash base in
  let dockerfile =
    if Analyse.Analysis.is_duniverse analysis then
      Duniverse_build.dockerfile ~base ~repo ~variant
    else (
      let opam_files = Analyse.Analysis.opam_files analysis in
      if opam_files = [] then failwith "No opam files found!";
      Opam_build.dockerfile ~base ~info:analysis ~variant
    )
  in
  Builder.build builder source ~dockerfile:(`Contents dockerfile)

let v ~platform ~schedule ~repo ~analysis source =
  let base = pull ~schedule platform in
  let+ _ : Current_docker.Raw.Image.t = build ~analysis ~repo ~platform ~base source in
  `Built
