#! /usr/bin/env scheme -q --debug-on-exception --libdirs vendor:. --compile-imported-libraries

;; (profile-load-data "profile")
(parameterize ([compile-profile 'source])
  (load "run.ss"))
(run)
;; (profile-dump-data "profile")
(profile-dump-html)
(exit)
