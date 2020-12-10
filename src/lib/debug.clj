(ns lib.debug)

(defmacro w
  "Watch - macro"
  [id & body]
  `(let [value# ~body] (.debug js/console (str " " '~id ">" (pr-str value#) "<" '~id " ")) value#))

(defmacro ws
  "Watch - macro"
  [id & body]
  `(let [value# (seq ~body)] (.debug js/console (str " " '~id ">" (pr-str value#) "<" '~id " ")) value#))
  
(defmacro wl
  "Watch - macro"
  [id & body]
  `(let [v# ~body]
     (lib.log.debug log (str ~id ">") v# (str "<" ~id))
     v#))

(defmacro we
  ([v]
   `(let [v# ~v]
      (lib.log.debug log "#>" v# "<#")
      v#))
  ([id v]
   `(let [v# ~v]
      (lib.log/debug log (str ~id ">") v# (str "<" ~id))
      v#))
  ([id f v]
   `(let [v# ~v]
      (lib.log/debug log (str ~id ">") (~f v#) (str "<" ~id))
      v#)))
