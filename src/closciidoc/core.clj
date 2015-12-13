(ns closciidoc.core
  (:require [clojure.string :as str]))


(defn document [& {:keys [title author] :or {title "Untitled" author ""}}]

  (str "= " title (if (> (count author) 0)
                    (str "\n" author)
                   )
       "\n")
)

(defn section [& {:keys [title level] :or {title "" level 1}}]
  (str "\n"
       (apply str (repeat (inc level) "="))
       " "
       title "\n")
  )


(defn- delimited [delimiter content]
  (str delimiter content delimiter))

(defn it[content] (str (delimited "_" content)))
(defn bf[content] (str (delimited "*" content)))
(defn mo[content] (str (delimited "`" content)))
(defn sup[content] (str (delimited "^" content)))
(defn sub[content] (str (delimited "~" content)))

(defn ol [& elems]
  (str (apply str (mapcat #(str ". " %1 "\n") elems))
       "\n"))

(defn list-item [itemchar level content]
  (let [prefix (str (apply str (repeat level itemchar)))]
    (str "\n"
         prefix " "
         content
         "\n")))

(defn- ilist [doc itemchar level & [elems]]
  (let [prefix (str (apply str (repeat level itemchar)))]
    (str doc "\n"
         (if (not (.endsWith doc "\n"))
           "\n")
         (apply str (mapcat #(str prefix " " %1 "\n") elems))
         "\n")))


(defn ul [doc & elems]
  (ilist doc "*" 1 elems)
)


(defn- finish-paragraph[doc]
  (str doc
       (if (not (.endsWith doc "\n"))
         "\n")))

(defn- new-paragraph[doc]
  (str (finish-paragraph doc) "\n"))


(defn- admon[type content]
  (finish-paragraph (str "\n" type " " content))
)

(defn note[content]
  (admon "NOTE:" content)
)

(defn important[content]
  (admon "IMPORTANT:" content)
)

(defn tip [content]
  (admon "TIP:" content)
)

(defn warning[content]
  (admon "WARNING:" content)
)

(defn caution[content]
  (admon "CAUTION:" content)
)

(defn image[& {:keys [src options] :or {options ""}}]
  (str "image::" src "[" options "]"))

(defn video[& {:keys [src options] :or {options ""}}]
  (str "video::" src "[" options "]"))

(defn literal[content]
  (str "\n....\n"
       content
       "\n....\n\n"))

(defn sidebar[content]
  (str  "\n****\n"
       content
       "\n****\n\n"))

(defn blockquote[content]
  (str "\n----\n"
       content
       "\n----\n\n"))

(defn anchor[name]
  (str "[[" name "]]"))

(defn internal-link[target]
  (str "[[" target "]]"))

