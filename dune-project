(lang dune 3.2)
(generate_opam_files true)
(name series)
(source (github maxtori/series))
(license MIT)
(authors "maxtori")
(maintainers "maxtori")
(homepage https://github.com/maxtori/series)
(bug_reports https://github.com/maxtori/series/issues)
(documentation https://github.com/maxtori/series)

(package
 (name series)
 (synopsis "series")
 (depends
   ez_api
   calendar
   ezjs_idb
   ezjs_fetch
   ezjs_push
   (ppx_deriving_encoding :build)
   (ppx_deriving_jsoo :build)
   (ppx_deriving_empty :build)
   (vue-ppx :build)))
