(defsystem "sister"
    :author "pete"
    :maintainer "pete@ohler.com"
    :license "MIT"
    :version "v0.1.0"
    :homepage "https://github.com/ohler55/slip"
    :bug-tracker "https://github.com/ohler55/slip/issues"
    :source-control "https://github.com/ohler55/slip"
    :description "Just a sample."
    :cache "testout"
    :components '("sister")
    :in-order-to '((:sample (+ (sys-test) six))
                   (:just-eval 3)))
