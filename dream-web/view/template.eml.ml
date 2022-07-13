let instance contents =
  <html>
    <head>
      <title>OCaml-CI</title>
      <meta charset="UTF-8">
      <link rel="stylesheet" href="/css/normalize.css">
      <link rel="stylesheet" href="/css/ansi.css">
      <link rel="stylesheet" href="/css/github.css">
      <link rel="stylesheet" href="/css/style.css">
    </head>
    <body>
      <nav>
        <ul>
          <li>
            <a href="/">OCaml-CI</a>
          </li>
        </ul>
      </nav>
      <div id="main">
        <%s! contents %>
      </div>
    </body>
  </html>
