;;; ssp-player-vlc.el --- VLC  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2008-2021  Free Software Foundation, Inc.

;; Authors: Yoni Rabkin <yrk@gnu.org>

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

;; 最初是从emms-player-vlc.el里抄来的

;;; Commentary:

;;; Code:
(require 'ssp)
(require 'tq)

(defcustom ssp-player-vlc-command '("vlc")
  ""
  :type '(list string)
  :group 'ssp)

(defconst ssp-player-vlc-arguments
  '("--intf=rc" "--no-random" "--no-loop" "--no-repeat"
    "--no-media-library" "--start-paused" "--play-and-pause"
    "-V" "xcb_xv"))

(defun ssp-player-vlc--started (callback _answer)
  (funcall callback))

(defun ssp-player-vlc--sentinel (proc _status)
  (unless (process-live-p proc)
    (let ((winprops (process-get proc 'winprops)))
      (ssp-player--exited winprops))))

(defun ssp-player-vlc--stderr-filter (proc string)
  (dolist (line (string-split string "\n"))
    (message "%s: %s" (process-name proc) line)))

(defun ssp-player-vlc--start (winprops callback)
  (let* ((frame (ssp-player--frame winprops t))
         (window-id (frame-parameter frame 'window-id))
         (proc
          (make-process
           :name (concat "ssp-player-vlc-" window-id)
           :noquery t
           :connection-type 'pty
           :stderr
           (make-pipe-process
            :name (concat "ssp-player-vlc-" window-id "-stderr")
            :noquery t
            :filter #'ssp-player-vlc--stderr-filter)
           :sentinel #'ssp-player-vlc--sentinel
           :command
           (append
            ssp-player-vlc-command
            ssp-player-vlc-arguments
            (list "--drawable-xid" window-id)))))
    (process-put proc 'winprops winprops)
    (let ((tq (tq-create proc)))
      (ssp--put winprops :vlc-tq tq)
      (ssp--put winprops :vlc proc)
      (ssp--put winprops :pause t)
      (tq-enqueue tq "" ".*> " callback #'ssp-player-vlc--started t))))

(defun ssp-player-vlc--callback (callback answer)
  (funcall callback answer))

(defun ssp-player-vlc--send (tq command callback)
  (if (process-live-p (tq-process tq))
      (tq-enqueue tq (concat command "\n") ".*> " callback
                  #'ssp-player-vlc--callback t)
    (funcall callback nil "vlc quitted")))

;;;###autoload
(defun ssp-player-vlc:load (winprops buffer callback)
  (if-let ((tq (ssp--get winprops :vlc-tq)))
      (ssp-player-vlc--send
       tq "clear"
       (lambda (&optional result error)
         (if error
             (funcall callback result error)
           (ssp-player-vlc--send
            tq (format "add %s" (buffer-file-name buffer))
            callback))))
    (ssp-player-vlc--start
     winprops
     (lambda ()
       (ssp-player-vlc:load winprops buffer callback)))))

(defun ssp-player-vlc--get-length (tq callback)
  (ssp-player-vlc--send
   tq "get_length"
   (lambda (&optional result error)
     (if error
         (funcall callback result error)
       (if (length= (string-trim (substring result 0 -2)) 0)
           (ssp-player-vlc--get-length tq callback)
         (let ((length (string-to-number result)))
           (if (> length 0)
               (funcall callback length)
             (ssp-player-vlc--get-length tq callback))))))))

;;;###autoload
(defun ssp-player-vlc:get-duration (winprops callback)
  (if-let ((tq (ssp--get winprops :vlc-tq)))
      (ssp-player-vlc--get-length tq callback)
    (funcall callback nil "vlc not started")))

;;;###autoload
(defun ssp-player-vlc:ended-p (winprops callback)
  (if-let ((tq (ssp--get winprops :vlc-tq)))
      (if (ssp--get winprops :pause)
          (funcall callback)
        (ssp-player-vlc--send
         tq "status"
         (lambda (&optional result error)
           (if error
               (funcall callback result error)
             (if (string-search "( state paused )" result)
                 (funcall callback t)
               (funcall callback))))))
      (funcall callback nil "vlc not started")))

;;;###autoload
(defun ssp-player-vlc:pause (winprops callback)
  (if-let ((tq (ssp--get winprops :vlc-tq)))
      (if (ssp--get winprops :pause)
          (funcall callback)
        (ssp--put winprops :pause t)
        (ssp-player-vlc--send tq "pause" callback))
    (funcall callback nil "vlc not started")))

;;;###autoload
(defun ssp-player-vlc:start (winprops callback)
  (if-let ((tq (ssp--get winprops :vlc-tq)))
      (if (not (ssp--get winprops :pause))
          (funcall callback)
        (ssp--put winprops :pause nil)
        (ssp-player-vlc--send tq "play" callback))
    (funcall callback nil "vlc not started")))

;;;###autoload
(defun ssp-player-vlc:seek-to (winprops position callback)
  (if-let ((tq (ssp--get winprops :vlc-tq)))
      (ssp-player-vlc--send tq (format "seek %d" position) callback)
    (funcall callback nil "vlc not started")))

;;;###autoload
(defun ssp-player-vlc:quit (winprops)
  (when-let ((tq (ssp--get winprops :vlc-tq)))
    (ssp-player-vlc--send tq "shutdown" #'ignore)))

(provide 'ssp-player-vlc)
;;; ssp-player-vlc.el ends here
