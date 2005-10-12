(deglobalize-lang
  '(program
     (bindings)
     (socpgm (bindings) (call spread-global))
     (nodepgm
       (tokens
         (f_token_tmpworld_10 () (call m_token_tmpworld_10))
         (spark-world () (call m_token_tmpworld_10))
         (tmpfunc_9 (a_2) (let* ((result_5 (local-sense))) result_5))
         (m_token_tmpworld_10 () (activate f_token_tmprmap_11))
         (f_token_tmprmap_11
           ()
           (call m_token_tmprmap_11 (call tmpfunc_9 this))
           (timed-call 100 f_token_tmprmap_11))
         (tmpfunc_8 (a_3 b_4) (let* ((result_6 (+ a_3 b_4))) result_6))
         (m_token_tmprmap_11 (v) (call f_token_result_7 v))
         (f_token_result_7
           (v)
           (return
             v
             (to m_token_result_7)
             (via global-tree)
             (seed '0)
             (aggr tmpfunc_8)))
         (m_token_result_7 (v) (soc-return v))
         (leaf-pulsar_tmpworld_10
           ()
           (call f_token_tmpworld_10)
           (timed-call 1000 leaf-pulsar_tmpworld_10))
         (spread-global () (emit global-tree) (timed-call 1000 spread-global))
         (global-tree () (relay)))
       (startup leaf-pulsar_tmpworld_10 spark-world))))
