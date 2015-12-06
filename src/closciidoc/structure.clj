(ns closciidoc.structure
  (:use [closciidoc.core])
)

(defn- attributes[element]
  (if (map? (second element))
    (second element)
    {})
)

(declare convert)

(def list-marks {:unordered "*"
                 :ordered "."
})

(defn- process[elements opts]
  (apply str (mapcat #(convert %1 opts) elements))
)

(defn- handle-list [type element opts]
  (let [mark (get list-marks type)
        depth (get opts type)]
    (apply str (map #(list-item mark
                                depth   
                                (convert %1 (assoc opts type (inc depth))))
                    (subvec element 1)))))

(defn- attrs?[element]
  (and (vector? element)
       (map? (second element)))
)

(defn asciidoc[doc]
  (convert doc {:section 1 :ordered 1 :unordered 1})
)

(defn convert [element opts]
  (if (string? element)
    element
    (let [attrs (attributes element)]
      (println "attrs=" attrs " " (second element))
      (condp = (first element)
       :doc (str (document :title (:title attrs) :author (:author attrs))
                 (process (subvec element 2) opts))
       :section (str (section :title (second element) :level (:section opts))
                     (process (subvec element (if (attrs? element) 
                                                3
                                                2))
                              (assoc opts :section (inc (:section opts)))))
       :ul (handle-list :unordered element opts)
       :ol (handle-list :ordered element opts)
       :note (note (process (subvec element 1) opts))
       :important (important (process (subvec element 1) opts))
       :tip (tip (process (subvec element 1) opts))
       :warning (warning (process (subvec element 1) opts))
       :caution (caution (process (subvec element 1) opts))
       :text (apply str (process (subvec element 1) opts))
       :comment (str "\n//" (second element) "\n")
       ""
      )
)))

(spit "/tmp/mist.adoc"
      (asciidoc [:doc {:title "Hey there" 
                       :author "Ingo Kessinger"}
                 [:section "Bla"]
                 [:section "Bla2"
                  [:ul "a sdafj asdjkf alksdjf aslkjdf akjalösdf aölsdkjf asljdkfaösl" 
                   [:text "b"
                    [:ul "ul1" "ul2"]]]
                  [:comment "Separator"]
           [:ol "ol1" [:text "ol2"
                       [:ol "ol2.1" "ol2.2"]]]
                  [:note "This is a note"]
                  [:image "test.jpg"]
                  [:section "Sub"]]
                 ]
                ))

(section :title "Bla")

(handle-list "---" [:ol "x" "y"] {})
