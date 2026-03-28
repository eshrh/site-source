#lang mogra

(require racket/file
         racket/match)

(define (in-cwd dir thunk)
  (define prev (current-directory))
  (dynamic-wind
    void
    (lambda ()
      (current-directory dir)
      (thunk))
    (lambda ()
      (current-directory prev))))

(define (build)
  ($ "stack run -- build"))

(define (commit-and-push-tracked!)
  ($ "git add css files favicon.ico images pages posts publish.mts site.hs templates index.html package.yaml nix")
  ($ "git commit -m 'website update'")
  ($ "git push origin master"))

(define (ensure-site-git!)
  (when (not (directory-exists? ".git"))
    ($ "git init")
    ($ "git remote add origin 'https://github.com/eshrh/site'")))

(define (commit-and-force-push-site!)
  ($ "git add .")
  ($ "git commit -m 'website update'")
  ($ "git push origin master --force"))

(define (upload)
  (build)
  (commit-and-push-tracked!)
  (in-cwd "_site"
    (lambda ()
      (ensure-site-git!)
      (commit-and-force-push-site!))))

(define (run-watch)
  (build)
  ($ "stack run -- watch --port 8001 :sh"))

(define (main args)
  (match args
    [(list "upload") (upload)]
    [(list "run") (run-watch)]
    [_ (displayln "did nothing.")]))

(main (vector->list (current-command-line-arguments)))
