#! /usr/bin/env scheme -q --debug-on-exception

(parameterize ([compile-profile 'source]
               [generate-allocation-counts #t]
               ;; [generate-instruction-counts #t]

               )
    (load "run.ss"))
(run-with-cost)
(profile-dump-html)
(exit)
