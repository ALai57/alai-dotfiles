;; .dir-locals.el file for Website to allow for testing in Emacs
;;
((nil . ((phpunit-executable . "docker")
         (phpunit-args . ("exec" "-it" "app" "vendor/bin/phpunit"))))
 (php-mode . ((phpunit-executable . "docker")
              (phpunit-args . ("exec" "-it" "app" "vendor/bin/phpunit")))))
