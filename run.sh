#! /usr/bin/env scheme -q --optimize-level 3

(profile-load-data "profile")
(parameterize ([compile-profile 'block])
    (load "run.ss"))
(run)
(profile-dump-data "profile")
(profile-dump-html)
(exit)
