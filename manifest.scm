(use-modules (guix packages)
             (gnu packages base)
             (gnu packages compression)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages gnupg))

(packages->manifest
 (list guile-next
       guile-hoot
       gnu-make
       zip
       guile-goblins
       guile-json-4
       guile-fibers
       guile-gcrypt
       guile-readline))