":";exec snowrun -- "$0" "$@"

(package* usematch v1.0.0
  (require: snow-match)
 )

;;;============================================================================

(newline)(display "RUNNING: ")
;(printf "RUNNING ~s ~s \n" oblist (snow-fxand 4 88))

(display (match 3 (5 "zeroth")(3 "first") (4 "second")))
(newline)
