(defn upload []
  (os/shell "git add *")
  (os/shell "git commit -m 'website update'")
  #(os/shell "git push origin master")

  (os/shell "stack build")
  (os/shell "stack exec site build"))

  #(os/cd "_site")
  #(os/shell "git add *")
  #(os/shell "git commit -m 'website update'")
  #(os/shell "git push origin master"))

(defn run [] (print "run"))

(defn main [& args]
  (each arg args (print arg))
  (match (in args 1)
    "upload" (upload)
    "run" (run)
    _ (print "did nothing.")))

