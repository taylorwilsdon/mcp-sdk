(lang dune 3.24)

(name mcp-sdk)

(generate_opam_files true)

(source
 (github taylorwilsdon/mcp-sdk))

(authors "Taylor Wilsdon <tbarrettwilsdon@gmail.com>")

(maintainers "Taylor Wilsdon <tbarrettwilsdon@gmail.com>")

(license MIT)

(package
 (name mcp-sdk)
 (synopsis "Model Context Protocol (2026-07-28) SDK for OCaml 5")
 (description
  "A stateless, multicore Model Context Protocol server and client for OCaml 5 built on Eio. Targets protocol revision 2026-07-28 with multi round-trip requests, subscriptions, cacheable results, stdio and HTTP/1.1 transports.")
 (depends
  (ocaml
   (>= 5.2))
  (eio
   (>= 1.2))
  (eio_main
   (>= 1.2))
  (jsont
   (>= 0.2))
  (bytesrw
   (>= 0.1))
  (digestif
   (>= 1.2))
  (base64
   (>= 3.5))
  (httpun-eio
   (>= 0.2))
  (alcotest :with-test))
 (tags
  (mcp "model context protocol" llm eio)))
