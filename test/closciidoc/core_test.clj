(ns closciidoc.core-test
  (:require [clojure.test :refer :all]
            [closciidoc.core :refer :all])
  (:use midje.sweet)
)

(facts "low level construction functions"
      (document :title "a" :author "b") => "= a\nb\n"
      (section :title "a" :level 1) => "\n== a\n"
)

(facts "typing"
       (it "a") => "_a_"
       (bf "a") => "*a*"
       (mo "a") => "`a`"
       (sup "a") => "^a^"
       (sub "a") => "~a~"
)

(facts "admonitions"
       (note "a") => "\nNOTE: a\n"
       (important "a") => "\nIMPORTANT: a\n"
       (tip "a") => "\nTIP: a\n"
       (warning "a") => "\nWARNING: a\n"
       (caution "a") => "\nCAUTION: a\n"
)

(facts "image"
       (image :src "a") => "image::a[]"
       (image :src "a" :options "150,150") => "image::a[150,150]"
)

(facts "video"
       (video :src "a") => "video::a[]"
       (video :src "a" :options "150,150") => "video::a[150,150]"
)


(facts "boxes"
       (literal "a") => "\n....\na\n....\n\n"
       (sidebar "a") => "\n****\na\n****\n\n"
       (blockquote "a") => "\n----\na\n----\n\n"
)


