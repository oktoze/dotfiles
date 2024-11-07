(require 'docker)

(defun kz/docker-container-entries(&rest filters)
  (if filters
      (aio-wait-for (docker-container-entries (mapconcat (lambda (f) (format "-f %s" f)) filters " ")))
                                (aio-wait-for (docker-container-entries "--all"))))

(defun kz/docker-project-container-entries (&rest filters)
  (kz/docker-container-entries (format "name=%s" (projectile-project-name))))

(completing-read "project-containers" (kz/docker-project-container-entries "status=running"))

(buffer-file-name)
(delete-file "~/khedmat")

(defun kz/docker-run-command (command container)
  (wait-for ((docker-run-docker-async command container))))

(defun kz/docker-restart-container ()
  (interactive)
  (let ((container (completing-read "Restart container: " (kz/docker-container-entries))))
    (kz/docker-run-command "restart" container)))

(defun kz/docker-restart-project-container ()
  (interactive)
  (let ((container (completing-read "Restart project container: " (kz/docker-container-entries (format "name=%s" (projectile-project-name)))))
    (kz/docker-run-command "restart" container))))


(defun kz/docker-restart ()
  (interactive)
  )
