;;; ssp-player-mpv.el --- MPV JSON IPC  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Authors: Mike Kazantsev <mk.fraggod@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;; 最初是从emms-player-mpv.el里抄来的

;;; Commentary:

;;; Code:
(require 'ssp)

(defcustom ssp-player-mpv-command '("mpv")
  ""
  :type '(list string)
  :group 'ssp)

(defconst ssp-player-mpv-arguments
  '("--no-config" "--pause" "--idle=yes" "--msg-level=ipc=v" "--no-msg-color"
    "--osd-level=0" "--no-osc" "--cursor-autohide=no" "--no-input-cursor"
    "--no-input-default-bindings" "--input-vo-keyboard=no"
    "--keep-open=always" "--autoload-files=no"))

(defun ssp-player-mpv--add-event-callback (proc callback)
  (let ((list (process-get proc 'event-callbacks)))
    (push callback list)
    (process-put proc 'event-callbacks list)))

(defun ssp-player-mpv--remove-event-callback (proc callback)
  (let ((list (process-get proc 'event-callbacks)))
    (process-put proc 'event-callbacks (remove callback list))))

(defun ssp-player-mpv--client-filter (proc string)
  (when (process-live-p proc)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string)
      (goto-char (point-min))
      (let ((requests (process-get proc 'requests)))
        (while-let
            ((json
              (condition-case nil
                  (json-parse-buffer
                   :object-type 'alist
                   :array-type 'list
                   :null-object nil
                   :false-object nil)
                (json-end-of-file nil))))
          (if-let ((request-id (assoc-default 'request_id json)))
              (let* ((e (assoc-default 'error json)))
                (when-let ((callback (gethash request-id requests)))
                  (remhash request-id requests)
                  (if (equal e "success")
                      (funcall callback (assoc-default 'data json))
                    (funcall callback nil e))))
            (dolist (callback (process-get proc 'event-callbacks))
              (funcall callback json)))
          (delete-region (point-min) (point)))))))

(defun ssp-player-mpv--client-sentinel (proc _status)
  (unless (process-live-p proc)
    (let* ((winprops (process-get proc 'winprops))
           (server (ssp--get winprops :mpv-server)))
      (when (process-live-p server)
        (kill-process server))
      (ssp-player--exited winprops))))

(defun ssp-player-mpv--start-client (winprops server callback)
  (let* ((frame (ssp-player--frame winprops))
         (window-id (frame-parameter frame 'window-id))
         (buffer (generate-new-buffer (generate-new-buffer-name
                                       (format " *ssp-mplayer-mpv-%s*" window-id))))
         (proc
          (make-network-process
           :name (concat "ssp-player-mpv-client-" window-id)
           :noquery t
           :buffer buffer
           :service (process-get server 'path)
           :family 'local
           :filter #'ssp-player-mpv--client-filter
           :sentinel #'ssp-player-mpv--client-sentinel)))
    (process-put proc 'winprops winprops)
    (process-put proc 'next-request-id 0)
    (process-put proc 'requests (make-hash-table))
    (process-put proc 'event-callbacks nil)
    (ssp--put winprops :pause t)
    (ssp--put winprops :mpv-client proc)
    (funcall callback)))

(defun ssp-player-mpv--server-sentinel (proc _status)
  (unless (process-live-p proc)
    (let* ((winprops (process-get proc 'winprops))
           (client (ssp--get winprops :mpv-client)))
      (when (process-live-p client)
        (delete-process client)))))

(defun ssp-player-mpv--server-filter (proc string)
  (dolist (line (string-split string "\n"))
    (message "%s: %s" (process-name proc) line)))

(defun ssp-player-mpv--start (winprops callback)
  (let* ((frame (ssp-player--frame winprops t))
         (filter
          (lambda (proc string)
            (set-process-filter proc #'ssp-player-mpv--server-filter)
            (ssp-player-mpv--server-filter proc string)
            (ssp-player-mpv--start-client winprops proc callback)))
         (window-id (frame-parameter frame 'window-id))
         (path (concat "/tmp/ssp-player-mpv-" window-id))
         (proc
          (make-process
           :name (concat "ssp-player-mpv-server-" window-id)
           :noquery t
           :command
           (append ssp-player-mpv-command
                   ssp-player-mpv-arguments
                   (list (concat "--input-ipc-server=" path)
                         (concat "--wid=" window-id)
                         (concat "--background="
                                 (with-selected-frame frame
                                   (background-color-at-point)))))
           :filter filter
           :sentinel #'ssp-player-mpv--server-sentinel)))
    (process-put proc 'path path)
    (process-put proc 'winprops winprops)
    (ssp--put winprops :mpv-server proc)))

(defun ssp-player-mpv--request-id (proc)
  (let ((request-id (process-get proc 'next-request-id)))
    (process-put proc 'next-request-id (1+ request-id))
    request-id))

(defun ssp-player-mpv--send (proc command callback)
  (if (process-live-p proc)
      (let* ((request-id (ssp-player-mpv--request-id proc)))
        (puthash request-id callback (process-get proc 'requests))
        (process-send-string proc
                             (concat
                              (json-encode `((command . ,command)
                                             (request_id . ,request-id)))
                              "\n")))
    (funcall callback nil "mpv quitted")))

;;;###autoload
(defun ssp-player-mpv:load (winprops buffer callback)
  (if-let ((proc (ssp--get winprops :mpv-client)))
      (if (ssp--get winprops :pause)
          (let (start-file-callback end-file-callback)
            (setq end-file-callback
                  (lambda (event)
                    (pcase (assoc-default 'event event)
                      ("end-file"
                       (ssp-player-mpv--remove-event-callback proc end-file-callback)
                       (funcall callback nil (assoc-default 'reason event)))
                      ("playback-restart"
                       (ssp-player-mpv--remove-event-callback proc end-file-callback)
                       (funcall callback)))))
            (setq start-file-callback
                  (lambda (event)
                    (when (equal (assoc-default 'event event) "start-file")
                      (ssp-player-mpv--remove-event-callback proc start-file-callback)
                      (ssp-player-mpv--add-event-callback proc end-file-callback))))
            (ssp-player-mpv--send
             proc (list "loadfile" (buffer-file-name buffer))
             (lambda (_data)
               (ssp-player-mpv--add-event-callback proc start-file-callback))))
        (ssp-player-mpv--send
         proc '("set_property" "pause" t)
         (lambda (&optional result error)
           (if error
               (funcall callback result error)
           (ssp--put winprops :pause t)
           (ssp-player-mpv:load winprops buffer callback)))))
    (ssp-player-mpv--start
     winprops
     (lambda ()
       (ssp-player-mpv:load winprops buffer callback)))))

;;;###autoload
(defun ssp-player-mpv:get-duration (winprops callback)
  (if-let ((proc (ssp--get winprops :mpv-client)))
      (ssp-player-mpv--send proc '("get_property" "duration/full") callback)
    (funcall callback nil "mpv not started")))

;;;###autoload
(defun ssp-player-mpv:ended-p (winprops callback)
  (if-let ((proc (ssp--get winprops :mpv-client)))
      (ssp-player-mpv--send proc '("get_property" "eof-reached") callback)
    (funcall callback nil "mpv not started")))

;;;###autoload
(defun ssp-player-mpv:pause (winprops callback)
  (if-let ((proc (ssp--get winprops :mpv-client)))
      (ssp-player-mpv--send
       proc '("set_property" "pause" t)
       (lambda (&optional result error)
         (unless error
           (ssp--put winprops :pause t))
         (funcall callback result error)))
    (funcall callback nil "mpv not started")))

;;;###autoload
(defun ssp-player-mpv:start (winprops callback)
  (if-let ((proc (ssp--get winprops :mpv-client)))
      (ssp-player-mpv--send
       proc '("set_property" "pause" :json-false)
       (lambda (&optional result error)
         (unless error
           (ssp--put winprops :pause nil))
         (funcall callback result error)))
    (funcall callback nil "mpv not started")))

;;;###autoload
(defun ssp-player-mpv:seek-to (winprops position callback)
  (if-let ((proc (ssp--get winprops :mpv-client)))
      (let (seek-callback end-file-callback)
        (setq end-file-callback
              (lambda (event)
                (pcase (assoc-default 'event event)
                  ("end-file"
                   (ssp-player-mpv--remove-event-callback proc end-file-callback)
                   (funcall callback nil (assoc-default 'reason event)))
                  ("playback-restart"
                   (ssp-player-mpv--remove-event-callback proc end-file-callback)
                   (funcall callback)))))
        (setq seek-callback
              (lambda (event)
                (pcase (assoc-default 'event event)
                  ("seek"
                   (ssp-player-mpv--remove-event-callback proc seek-callback)
                   (ssp-player-mpv--add-event-callback proc end-file-callback))
                  ("end-file"
                   (ssp-player-mpv--remove-event-callback proc seek-callback)
                   (funcall callback (assoc-default 'reason event))))))
        (ssp-player-mpv--send
         proc (list "seek" position "absolute")
         (lambda (&optional result error)
           (if error
               (funcall callback result error)
             (ssp-player-mpv--add-event-callback proc seek-callback)))))
      (funcall callback nil "mpv not started")))

;;;###autoload
(defun ssp-player-mpv:quit (_winprops))

(provide 'ssp-player-mpv)
;;; ssp-player-mpv.el ends here
