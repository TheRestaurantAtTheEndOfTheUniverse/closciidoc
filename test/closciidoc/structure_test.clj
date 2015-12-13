(ns closciidoc.structure-test
  (:require [closciidoc.structure :refer :all]
            [midje.sweet :refer :all]))



(facts "doc element"
      (asciidoc [:doc :title "test"]) => "= test\n"
      (asciidoc [:doc :title "test" :author "name"]) => "= test\nname\n"
)

(facts "sections"
       (asciidoc [:section :title "test"]) => "\n== test\n"
       (asciidoc [:section :title "test" "content"]) => "\n== test\ncontent"
       (asciidoc [:section :title "outer" [:section :title "inner"]]) => "\n== outer\n\n=== inner\n"
       (asciidoc [:doc :title "test"
                  [:section :title "first"] 
                  [:section :title "second"]]) => "= test\n\n== first\n\n== second\n"
)

(facts "lists"
       (asciidoc [:ul "test"]) => "\n* test\n"
       (asciidoc [:ul "one" "two"]) => "\n* one\n\n* two\n"
       (asciidoc [:ul [:text "outer" [:ul "inner"]]]) => "\n* outer\n** inner\n\n"
       (asciidoc [:dl "one" "definition for one" "two" "definition for two"]) => "\none::definition for one\n\ntwo::definition for two\n"
)


(facts "admonitions"
       (asciidoc [:important "test"]) => "\nIMPORTANT: test\n"
       (asciidoc [:tip "test"]) => "\nTIP: test\n"
       (asciidoc [:warning "test"]) => "\nWARNING: test\n"
       (asciidoc [:caution "test"]) => "\nCAUTION: test\n"
)


