(ns closciidoc.core
  (:require [clojure.string :as str]))


(defn document [& {:keys [title author] :or {title "Untitled" author ""}}]

  (str "= " title (if (> (count author) 0)
                    "\nauthor"
                   )
       "\n")
)

(defn section [doc & {:keys [title level] :or {title "" level 1}}]
  (str doc "\n"
       (apply str (repeat (inc level) "=")) 
       title "\n")
  )


(defn it[content] (str "_" content "_"))
(defn bf[content] (str "*" content "*"))
(defn mo[content] (str "'" content "'"))

(defn ol [& elems]
  (str "\n"
       (apply str (mapcat #(str ". " %1 "\n") elems))
       "\n"
))

(defn- ilist [itemchar level & [elems]]
  (let [prefix (str (apply str (repeat level itemchar)))]
    (str "\n"
         (apply str (mapcat #(str prefix " " %1 "\n") elems))
         "\n")))


(defn ul [& elems]
  (ilist "*" 1 elems)
)


(str 
 (section (document :title "Hello" :author "Ingo") :title "eee" )
 (it "xxx")
 (mo "xxx")
 (bf "fdsksfjd")
 (ul "a" "b"))
