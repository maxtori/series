type theme = {
  t_navbar : string option;
  t_bg : string;
  t_icon : string;
  t_icon_variant : string option;
  t_tx : string;
} [@@deriving jsoo]

let light_theme = {
  t_navbar = None;
  t_bg = "light";
  t_icon = "moon";
  t_icon_variant = None;
  t_tx = "dark";
}

let dark_theme = {
  t_navbar = Some "navbar-dark";
  t_bg = "dark";
  t_icon = "sun";
  t_icon_variant = Some "light";
  t_tx = "light";
}
