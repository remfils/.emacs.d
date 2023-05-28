(require 'package)

(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(provide 'setup-use-package)
