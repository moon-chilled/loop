(define files '("tagbody.scm"
                "primitives.scm"

                "generics-and-globals.scm"
                "utilities.scm"
                "combinatory-parsing.scm"
                "parse-common.scm"
                "clause.scm"
                "expansion.scm"
                "main-clause.scm"
                "variable-clause.scm"
                "selectable-clause.scm"
                "unconditional-clause.scm"
                "accumulation-clause.scm"
                "termination-test-clause.scm"
                "var-spec.scm"
                "type-spec.scm"
                "name-clause.scm"
                "initial-clause.scm"
                "final-clause.scm"
                "with-clause.scm"
                "return-clause.scm"
                "do-clause.scm"
                "collect-clause.scm"
                ;"append-clause.scm"
                ;"nconc-clause.scm"
                ;"count-clause.scm"
                ;"sum-clause.scm"
                ;"maximize-clause.scm"
                ;"minimize-clause.scm"
                ;"conditional-clause.scm"
                "while-until-clauses.scm"
                ;"repeat-clause.scm"
                ;"always-clause.scm"
                ;"never-clause.scm"
                ;"thereis-clause.scm"
                "for-as-clause.scm"
                "for-as-arithmetic-clause.scm"
                "for-as-list-clause.scm"
                "for-as-equals-then-clause.scm"
                ;"for-as-across-clause.scm"
                ;"for-as-hash-clause.scm"
                ;"for-as-package-clause.scm"

                ;"analysis.scm"
                "run-time-support.scm"))

(for-each load files)

(define-macro (loop . forms)
  (expand-body forms))