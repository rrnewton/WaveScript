(deglobalize-lang
  '(program
     (bindings)
     (socpgm (bindings) (call spread-global))
     (nodepgm
       (tokens
         (f_token_tmpworld_8 () (call m_token_tmpworld_8))
         (spark-world () (call m_token_tmpworld_8))
         (tmpfunc_9 (a_2) (let* ([result_5 (local-sense)]) result_5))
         (m_token_tmpworld_8 () (activate f_token_tmprmap_10))
         (f_token_tmprmap_10
           ()
           (call m_token_tmprmap_10 (call tmpfunc_9 this))
           (timed-call 100 f_token_tmprmap_10))
         (tmpfunc_11
           (a_4 b_3)
           (let* ([result_6 (cons a_4 b_3)]) result_6))
         (m_token_tmprmap_10 (v) (call f_token_result_7 v))
         (f_token_result_7
           (v)
           (return
             v
             (to m_token_result_7)
             (via global-tree)
             (seed '())
             (aggr tmpfunc_11)))
         (m_token_result_7 (v) (soc-return v))
         (leaf-pulsar_tmpworld_8
           ()
           (call f_token_tmpworld_8)
           (timed-call 1000 leaf-pulsar_tmpworld_8))
         (spread-global
           ()
           (emit global-tree)
           (timed-call 1000 spread-global))
         (global-tree () (relay)))
       (startup leaf-pulsar_tmpworld_8 spark-world))))
