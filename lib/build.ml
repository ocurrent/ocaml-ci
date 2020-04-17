open Current.Syntax

let pull ~schedule platform =
  Current.component "docker pull" |>
  let> { Platform.builder; variant; label = _ } = platform in
  Builder.pull builder ("ocurrent/opam:" ^ variant) ~schedule

let pread ~platform image ~args =
  Current.component "pread" |>
  let> { Platform.builder; _ } = platform in
  Builder.pread builder ~args image

let build ~platform ~base ~revdep ~with_tests ~pkg source =
  Current.component "build" |>
  let> { Platform.builder; variant; _ } = platform
  and> base = base
  and> source = source in
  let base = Current_docker.Raw.Image.hash base in
  let dockerfile =
    Opam_build.dockerfile ~base ~variant ~revdep ~with_tests ~pkg
  in
  Builder.build builder source ~dockerfile:(`Contents dockerfile)

let v ~platform ~schedule ~revdep ~with_tests ~pkg source =
  let base = pull ~schedule platform in
  build ~platform ~base ~revdep ~with_tests ~pkg source
