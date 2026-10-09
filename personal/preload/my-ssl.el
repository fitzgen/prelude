;;; -*- lexical-binding: t; -*-
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "/opt/homebrew/etc/ca-certificates/cert.pem")
  (add-to-list 'gnutls-trustfiles "/opt/homebrew/etc/gnutls/cert.pem")
  (add-to-list 'gnutls-trustfiles "/opt/homebrew/etc/libressl/cert.pem")
  (add-to-list 'gnutls-trustfiles "/private/etc/ssl/cert.pem")
)

(provide 'my-ssl)
