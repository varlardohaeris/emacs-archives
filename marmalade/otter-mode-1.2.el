;;; otter-mode.el --- Major mode for source files of the Otter automated theorem prover

;; Copyright 2012 Alexandru Scvortov

;; Author: Alexandru Scvortov <scvalex@gmail.com>
;; Version: 1.2
;; URL: https://github.com/scvalex/script-fu/blob/master/otter-mode.el

;; For the exact syntax rules, see Section 4 (Syntax) of::
;;
;;   http://www.mcs.anl.gov/research/projects/AR/otter/otter33.pdf

(require 'generic-x)

(define-generic-mode
    'otter-mode
  ;; comments start with '%'
  '("%")
  ;; keywords
  '("set" "list" "end_of_list" "clear" "sos" "binary_res"
    "factor" "back_sub" "for_sub" "usable" "order_hyper"
    "lex" "passive" "include" "op" "make_evaluable" "assign"
    "formula_list" "weight_list" "skolem" "demodulators"
    "lrpo_multiset_status" "max_seconds" "sos_queue" "sos_stack"
    "weight" "pick_given" "purge_gen" "pick_and_purge" "terms"
    "input_sos_first" "interactive_given" "print_given"
    "print_lists_at_end" "unit_deletion" "hyper_res"
    "neg_hyper_res" "ur_res" "para_into" "para_from" "demod_inf"
    "order_hyper" "unit_res" "ur_last" "para_into_left"
    "para_from_left" "para_from_right" "para_into_right"
    "para_from_vars" "para_into_vars" "para_from_units_only"
    "para_skip_skolem" "para_ones_rule" "para_all"
    "detailed_history" "order_history" "back_unit_deletion"
    "delete_identical_nested_skolem" "sort_literals"
    "demod_history" "order_eq" "eq_unit_both_ways"
    "demod_linear" "demod_out_in" "dynamic_demod"
    "dynamic_demod_all" "dynamic_demod_lex_dep" "back_demod"
    "anl_eq" "knuth_bendix" "lrpo" "lex_order_vars"
    "symbol_elim" "rewriter" "check_arity" "prolog_style_variables"
    "echo_included_files" "simplify_fol" "process_input"
    "tptq_eq" "very_verbose" "print_kept" "print_proofs"
    "build_proof_object_1" "print_proof_object_2"
    "print_new_demod" "print_back_sub" "display_terms"
    "pretty_print" "bird_print" "formula_history"
    "index_for_back_demod" "for_sub_fpa" "no_fapl" "no_fanl"
    "control_memory" "propositional" "really_delete_clauses"
    "atom_wt_max_args" "term_wt_max_args" "free_all_mem"
    "sigint_interact" "max_gen" "max_kept" "max_given"
    "max_levels" "max_mem" "max_literals" "max_weight"
    "max_distinct_vars" "max_answers" "fpa_literals"
    "fpa_terms" "pick_given_ration" "age_factor"
    "distinctive_vars_factor" "interrupt_given"
    "demod_limit" "max_proofs" "min_bit_width" "neg_weight"
    "pretty_print_indent" "stats_level"
    "dynamic_demod_depth" "dynamic_demod_rhs"
    "new_symbol_lex_position")
  ;; highlighting
  '(("\\b[uvwxyz][A-z_0-9$]*\\b" . 'font-lock-variable-name-face)
    ("\\b[p]\\b" . 'font-lock-type-face)
    ("-" . 'font-lock-warning-face)
    ;; The following overlaps with the previous patterns, so it *must*
    ;; be the last one.
    ("\\b[A-z]+\\b" . 'font-lock-constant-face)
    )
  ;; extension
  '("\\.ot$")
  ;; other commands
  nil
  ;; help
  "A mode for Otter files"
  )

(provide 'otter-mode)

;;; otter-mode.el ends here
