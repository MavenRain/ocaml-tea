(lang dune 3.0)
; Step 23 (R7): a module may reference only libraries its own stanza lists.
; Under the default (true), any module could name any TRANSITIVELY linked
; library - an example could call Irmin without listing irmin - so the
; stanzas were documentation, not a boundary. False makes each (libraries)
; field the audited compile-time surface; the sink_gate test stanza audits
; what dune's granularity cannot see (module-level use INSIDE a library).
(implicit_transitive_deps false)
