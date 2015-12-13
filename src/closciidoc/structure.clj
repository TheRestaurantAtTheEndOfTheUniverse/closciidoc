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

(defn- handle-list [type content opts]
  (let [mark (get list-marks type)
        depth (get opts type)]
    (apply str (map #(list-item mark
                                depth   
                                (convert %1 (assoc opts type (inc depth))))
                    content))))


(defn- handle-dl[content opts]
  (loop [result ""
         content content]
    (if (< (count content) 2)
      result
      (recur (str result "\n" (convert (first content) opts)
                ":: " (convert (second content) opts) "\n")
             (subvec content 2)
           
    ))))

(defn- separate-content [element-vec]
  (loop[attrs {} sub-elements [] element-vec element-vec]
    (cond (empty? element-vec) [attrs sub-elements]
          (keyword? (first element-vec)) (recur (assoc attrs (first element-vec) 
                                                       (second element-vec))
                                                sub-elements
                                                (subvec element-vec 2))
          (map? (first element-vec)) (recur (merge attrs (first element-vec))
                                            sub-elements
                                            (subvec element-vec 1))
          :else (recur attrs (conj sub-elements (first element-vec))
                       (subvec element-vec 1)))
    )
)

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
    (let [[attrs content] (separate-content (subvec element 1))]
      (condp = (first element)
       :doc (str (document :title (:title attrs) :author (:author attrs))
                 (process content opts))
       :section (str (section :title (:title attrs) :level (:section opts))
                     (process content                              
                              (assoc opts :section (inc (:section opts)))))
       :ul (handle-list :unordered content opts)
       :ol (handle-list :ordered content opts)
       :note (note (process content opts))
       :important (important (process content opts))
       :tip (tip (process content opts))
       :warning (warning (process content opts))
       :caution (caution (process content opts))
       :text (apply str (process content opts))
       :comment (str "\n//" (first content) "\n")
       :dl (handle-dl content opts)
       ""
      )
)))

(spit "/tmp/mist.adoc"
      (asciidoc [:doc :title "Hey there" 
                       :author "Ingo Kessinger"

                 [:dl "so" "soso" "dies" "das"]

                 [:section :title "Bla"]
                 [:section :title "Bla2"
                  [:ul "a sdafj asdjkf alksdjf aslkjdf akjalösdf aölsdkjf asljdkfaösl" 
                   [:text "b"
                    [:ul "ul1" "ul2"]]]
                  [:comment "Separator"]
                  [:comment]
           [:ol "ol1" [:text "ol2"
                       [:ol "ol2.1" "ol2.2"]]]
                  [:note "This is a note"]
                  [:image "test.jpg"]
                  [:section :title "Sub"]]
                 ]
                ))




