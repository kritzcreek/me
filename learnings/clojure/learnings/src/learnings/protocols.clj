(ns learnings.protocols
  "Learnings with respect to protocols")

;; http://adambard.com/blog/structured-clojure-protocols-and-multimethods/
;; http://www.ibm.com/developerworks/library/j-clojure-protocols/

;; In Java, you have interfaces which dictate how an object will
;; interact with other things. Protocols are kinda the same thing.
;; They define a type and its methods, without giving any implementation
;; details. However, protocols differ from interfaces in one major
;; way: they can be applied to types that are *already declared*.
;;
;; The following is the same function as in `learnings.multimethods',
;; but implemented as a protocol:

(defprotocol DoesAThing
  (do-a-thing [in] "Do a thing"))

(extend-protocol DoesAThing
  clojure.lang.PersistentVector
  (do-a-thing [in] "A vector (via protocol)")
  clojure.lang.PersistentArrayMap
  (do-a-thing [in] "A map (via protocol)"))

;; Protocols generally are more performant than multimethods because
;; they work directly with Java's dispatch.
;;
;; Protocols are useful because they also work directly with types
;; and records.
;;
;; Let's define a protocol for formatting a bibliographic reference
;; into the MLA standard. We will use a record to store the data that
;; is implemented by the protocol.
;;
;; Data fields:
;; - article title
;; - contributors
;;   - author, editor, compiler, or translator
;;   - first name
;;   - middle initial
;;   - last name
;;   - suffix
;; - journal title
;; - volume
;; - issue
;; - series
;; - year published
;; - pages
;;   - start
;;   - end
;

(comment
  ; The other fields
  compiler translator
  first-name middle-initial last-name suffix
  journal-title volume issue series year
  start-page end-page)


(defprotocol Citation
  (^String update-title   [this input] "Updates the title of the reference.")
  (^String update-journal [this input] "Updates the journal of the reference.")
  (^Long update-year    [this input] "Updates the year of the reference.")
  (add-contributor [this & {:keys [kind first mi last suffix]
                            :or   [kind 'author first nil mi nil last nil suffix nil]}]
    "Takes a map of :kind, :first, :mi, :last, and :suffix for a contributor of
    the reference, where :kind is one of :author, :compiler, :translator, or
    :editor. :kind defaults to :author, other keys default to nil.")
  (^Long add-volume [this input] "Adds a volume to the ref's journal.")
  (^Long add-issue  [this input] "Adds an issue to the ref's journal.")
  (^Long add-series [this input] "Adds a series to the ref's journal.")
  (^Long add-start-page [this input] "Adds a start page to the ref.")
  (^Long add-end-page [this input] "Adds an end page to the ref."))


(defrecord Article [title journal year]
  Citation
  (update-title [this input]
    (assoc this :title (str input)))
  (update-journal [this input]
    (assoc this :journal (str input)))
  (update-year [this input]
    (assoc this :year input))
  (add-contributor [this & {:keys [kind first mi last suffix]
                            :or   [kind 'author first nil mi nil last nil suffix nil]}]
    (assoc this (keyword kind) {:first (str first) :mi (str mi)
                                :last (str last) :suffix (str suffix)})))

(comment
  (add-volume [this input]
              (assoc this :volume input))
  (add-issue [this input]
             (assoc this :issue input))
  (add-start-page [this input]
                  (assoc this :pages {:start input}))
  (add-end-page [this input]
                (assoc (:pages this) {:end input})))
