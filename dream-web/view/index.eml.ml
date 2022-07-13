let contents =
  <p>Welcome to OCaml-CI!</p>
                 <p>See <a href='https://github.com/apps/ocaml-ci'>The Ocaml-CI GitHub App</a> for details</p>
                 <ul>
                 <li><a href='/github'>Registered GitHub organisations</a></li>
  </ul>

let render =
  Template.instance contents
