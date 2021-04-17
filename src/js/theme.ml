type theme = {
  t_navbar_type : string option;
  t_navbar_variant : string option;
  t_body : string option;
  t_card_bg : string option;
  t_card_text : string option;
  t_icon : string;
  t_icon_variant : string option;
  t_btn_outline : string;
  t_text : string;
} [@@deriving jsoo]

let light_theme = {
  t_navbar_type = None;
  t_navbar_variant = None;
  t_body = None;
  t_card_bg = None;
  t_card_text = None;
  t_icon = "moon";
  t_icon_variant = None;
  t_btn_outline = "dark";
  t_text = "text-dark";
}

let dark_theme = {
  t_navbar_type = Some "dark";
  t_navbar_variant = Some "dark";
  t_body = Some "bg-dark";
  t_card_bg = Some "dark";
  t_card_text = Some "white";
  t_icon = "sun";
  t_icon_variant = Some "light";
  t_btn_outline = "light";
  t_text = "text-light"
}
