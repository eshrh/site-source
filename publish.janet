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
  (build)
  (do-shell
   (git add *)
   (git commit -m "website update")
   (git push origin master))

  (os/cd "_site")

  (if (not (os/stat ".git"))
    (do-shell
     (git init)
     (git remote add origin "https://github.com/eshrh/site")))

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

