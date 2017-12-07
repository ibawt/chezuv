#! /usr/bin/env scheme -q --debug-on-exception

(parameterize ([compile-profile 'source])
    (load "run.ss"))
(run)
(profile-dump-html)
(exit)
