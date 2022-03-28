(defun build []
  ($*
   (stack build)
   (stack exec site build)))

(defun upload []
  (build)
  ($*
   (git add *)
   (git commit -m "website update")
   (git push origin master))

  (os-cd "_site")

  (if (not (os-stat ".git"))
    ($*
     (git init)
     (git remote add origin "https://github.com/eshrh/site")))

  ($*
   (git add *)
   (git commit -m "website update")
   (git push origin master --force)))

(defun run []
  (build)
  ($ stack exec -- site watch --port 8001 :sh))

(defun main [& args]
  (match (in args 1)
    "upload" (upload)
    "run" (run)
    _ (print "did nothing.")))

