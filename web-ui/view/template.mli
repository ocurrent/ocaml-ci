val html_to_string : Tyxml_html.doc -> string

val instance :
  ?flash_messages:(string * string) list ->
  [< Html_types.div_content_fun ] Tyxml_html.elt list ->
  string
