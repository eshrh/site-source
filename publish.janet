(import sh)
(defmacro do-shell [& cmds]
  (map (fn [cmd]
         (def qcmd ~(quote ,cmd))
         ~(sh/run* ,qcmd)) cmds))

(defn build []
  (do-shell
   (stack build)
   (stack exec site build)))

(defn upload []
  (do-shell
   (git add *)
   (git commit -m "website update")
   (git push origin master))

  (build)
  (os/cd "_site")
  (do-shell
   (git add *)
   (git commit -m "website update")
   (git push origin master)))

(defn run []
  (build)
  (sh/$ stack exec site watch))

(defn main [& args]
  (match (in args 1)
    "upload" (upload)
    "run" (run)
    _ (print "did nothing.")))

