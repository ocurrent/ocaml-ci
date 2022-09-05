let x_cloak = Tyxml.Html.Unsafe.string_attrib "x-cloak" ""
let x_transition = Tyxml.Html.Unsafe.string_attrib "x-transition" ""
let x_data a = Tyxml.Html.Unsafe.string_attrib "x-data" a
let x_show a = Tyxml.Html.Unsafe.string_attrib "x-show" a
let at_click a = Tyxml.Html.Unsafe.string_attrib "@click" a
let at_click_outside a = Tyxml.Html.Unsafe.string_attrib "@click.outside" a
let a_svg_custom x y = Tyxml.Xml.string_attrib x y |> Tyxml.Svg.to_attrib
