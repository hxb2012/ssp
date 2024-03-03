;;; ssp.el --- SSP  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2007-2023 Free Software Foundation, Inc.

;; Authors: Tassilo Horn <tsdh@gnu.org>, Mike Kazantsev <mk.fraggod@gmail.com>, Yoni Rabkin <yrk@gnu.org>
;; Maintainer: "洪筱冰" <hxb@localhost.localdomain>
;; Package-Requires: ((emacs "29.0"))
;; Version: 0.0.1

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

;; 最初是从doc-view.el里抄来的

;;; Commentary:

;;; Code:

(defgroup ssp nil
  "Play media files"
  :group 'multimedia)

(defcustom ssp-player nil
  ""
  :type 'symbol
  :group 'ssp)

(defvar-local ssp-mode-winprops-alist nil
  "Alist of windows to window properties.
Each element has the form (WINDOW . ALIST).
See `ssp-mode-winprops'.")

(defvar-local ssp-mode--duration nil)
(defvar-local ssp-mode--last-position nil)

(defun ssp--get (winprops property)
  (plist-get (cdr winprops) property))

(defun ssp--put (winprops property value)
  (setcdr winprops (plist-put (cdr winprops) property value)))

(defun ssp-player-winprops (&optional window frame)
  (let* ((window (or window (selected-window)))
         (frame (or frame (window-frame window)))
         (ssp-player-winprops-alist
          (frame-parameter frame 'ssp-player-winprops-alist)))
    (if-let ((winprops (assq window ssp-player-winprops-alist)))
        winprops
      (let ((winprops (cons window nil)))
        (set-frame-parameter frame 'ssp-player-winprops-alist
                             (cons winprops ssp-player-winprops-alist))
        winprops))))

(defun ssp-player--frame (winprops &optional create)
  (or (ssp--get winprops :frame)
      (when create
        (let* ((window (car winprops))
               (parent (window-frame window))
               (frame (make-frame
                       (cons (cons 'parent-frame parent)
                             '((undecorated . t)
                               (vertical-scroll-bars . nil)
                               (left-fringe . 0)
                               (right-fringe . 0)
                               (fullscreen)
                               (minibuffer . only)
                               (visibility . nil)
                               (no-accept-focus . t))))))
          (ssp--put winprops :frame frame)
          frame))))

(defun ssp-player--loaded (winprops buffer _result error)
  (let ((window (car winprops))
        (expected (ssp--get winprops :buffer)))
    (when (window-live-p window)
      (if (eq buffer expected)
          (when (buffer-live-p buffer)
            (ssp--put winprops :loading nil)
            (if error
                (message "Failed to load %s: %s" (buffer-file-name buffer) error)
              (if (with-current-buffer buffer ssp-mode--duration)
                  (let* ((modeprops (ssp-mode-winprops window buffer))
                         (position
                          (or (ssp--get modeprops :start-position)
                              ssp-mode--last-position
                              0)))
                    (ssp--put modeprops :start-position position)
                    (ssp-player--seek-to winprops position)
                    (when (eq (window-buffer window) buffer)
                      (make-frame-visible (ssp--get winprops :frame)))
                    (message "Loaded %s" (buffer-file-name buffer)))
                (ssp--put winprops :loading 'duration)
                (ssp-player--get-duration winprops buffer))))
        (ssp-player--load winprops buffer)))))

(defun ssp-player--exited (winprops)
  (let ((window (car winprops))
        (buffer (ssp--get winprops :buffer)))
    (when (and (window-live-p window)
               (eq (window-buffer window) buffer))
      (ssp-mode--paused (ssp-mode-winprops window)))))

(defun ssp-player--call (func &rest args)
  (apply (intern (format "%s:%s" ssp-player func)) args))

(defun ssp-player--load (winprops buffer)
  (ssp-player--call 'load winprops buffer
                    (lambda (&optional result error)
                      (ssp-player--loaded winprops buffer result error))))

(defun ssp-player--get-duration (winprops buffer)
  (ssp-player--call 'get-duration winprops
                    (lambda (&optional result error)
                      (when (and (buffer-live-p buffer)
                                 (eq (ssp--get winprops :buffer) buffer))
                        (unless error
                          (with-current-buffer buffer
                            (setq ssp-mode--duration result)))
                        (ssp-player--loaded winprops buffer result error)))))

(defun ssp-player--seek-to (winprops position &optional callback)
  (let ((buffer (ssp--get winprops :buffer)))
    (ssp-player--call 'seek-to winprops position
                      (lambda (&optional _result error)
                        (when (and (buffer-live-p buffer)
                                   (eq (ssp--get winprops :buffer) buffer))
                          (if error
                              (message "Failed to seek %s to %s"
                                       (buffer-file-name buffer) position)
                            (when callback
                              (funcall callback))))))))

(defun ssp-player--pause (winprops &optional callback)
  (let ((buffer (ssp--get winprops :buffer)))
    (ssp-player--call 'pause winprops
                      (lambda (&optional _result error)
                        (when (and (buffer-live-p buffer)
                                   (eq (ssp--get winprops :buffer) buffer))
                          (if error
                              (message "Failed to pause %s" (buffer-file-name buffer))
                            (when callback
                              (funcall callback))))))))

(defun ssp-player--start (winprops &optional callback)
  (let ((buffer (ssp--get winprops :buffer)))
    (ssp-player--call 'start winprops
                      (lambda (&optional _result error)
                        (when (and (buffer-live-p buffer)
                                   (eq (ssp--get winprops :buffer) buffer))
                          (if error
                              (message "Failed to start %s" (buffer-file-name buffer))
                            (when callback
                              (funcall callback))))))))

(defun ssp-player--check-ended (winprops &optional callback)
  (let ((buffer (ssp--get winprops :buffer)))
    (ssp-player--call 'ended-p winprops
                      (lambda (&optional result error)
                        (when (and (buffer-live-p buffer)
                                   (eq (ssp--get winprops :buffer) buffer))
                          (if error
                              (message "Failed to check %s" (buffer-file-name buffer))
                            (when (and result callback)
                              (funcall callback))))))))

(defun ssp-player--switch-to (winprops buffer)
  (let* ((window (car winprops))
         (old-buffer (ssp--get winprops :buffer))
         (same-buffer-p (eq buffer old-buffer)))
    (if same-buffer-p
        (unless (ssp--get winprops :loading)
          (make-frame-visible (ssp--get winprops :frame)))
      (when (buffer-live-p old-buffer)
        (with-current-buffer old-buffer
          (let ((winprops (ssp-mode-winprops window old-buffer)))
            (when-let ((start-time (ssp--get winprops :start-time)))
              (when-let ((timer (ssp--get winprops :timer)))
                (cancel-timer timer)
                (ssp--put winprops :timer nil)))
            (let ((position (or (ssp--get winprops :position)
                                (ssp--get winprops :start-position))))
              (ssp--put winprops :start-position position)
              (ssp--put winprops :start-time nil)
              (setq ssp-mode--last-position position)))))
      (ssp--put winprops :buffer buffer)
      (unless (ssp--get winprops :loading)
        (ssp--put winprops :loading t)
        (ssp-player--load winprops buffer)))
    (not same-buffer-p)))

(defun ssp-player--quit (winprops)
  (when-let ((frame (ssp--get winprops :frame)))
    (delete-frame frame)
    (ssp-player--call 'quit winprops)))

(defun ssp-player--update-frame-size (winprops)
  (let* ((window (car winprops))
         (frame (ssp-player--frame winprops t))
         (fringes (window-fringes window))
         (left (window-pixel-left window))
         (top (window-pixel-top window))
         (width (window-body-width window t))
         (height (window-body-height window t)))
    (set-frame-position frame (+ left (car fringes)) top)
    (set-frame-size frame width (- height
                                   (frame-char-height (window-frame window)))
                    t)))

(defun ssp-mode-winprops (&optional window buffer)
  (let ((window (or window (selected-window))))
    (with-current-buffer (or buffer (window-buffer window))
      (setq ssp-mode-winprops-alist
            (seq-filter (lambda (winprops) (window-live-p (car winprops)))
                        ssp-mode-winprops-alist))
      (if-let ((winprops (assq window ssp-mode-winprops-alist)))
          winprops
        (let ((winprops (cons window nil)))
          (setq ssp-mode-winprops-alist (cons winprops ssp-mode-winprops-alist))
          winprops)))))

(defun ssp-mode--get-current-position (winprops)
  (if-let ((start-time (ssp--get winprops :start-time)))
    (let ((time (time-convert (time-since start-time) t)))
      (+ (ssp--get winprops :start-position)
         (/ (float (car time)) (cdr time))))
    (ssp--get winprops :start-position)))

(defun ssp-mode--redisplay (winprops)
  (let ((window (car winprops)))
    (when (window-live-p window)
      (ssp-player--check-ended
       (ssp-player-winprops window)
       (lambda ()
         (when (window-live-p window)
           (ssp-mode--paused winprops))))
      (force-window-update window))))

(defun ssp-mode--started (winprops)
  (unless (ssp--get winprops :start-time)
    (ssp--put winprops :start-time (current-time))
    (ssp--put winprops
              :timer (run-at-time t 1 #'ssp-mode--redisplay winprops))))

(defun ssp-mode--paused (winprops)
  (when-let ((start-time (ssp--get winprops :start-time)))
    (when-let ((timer (ssp--get winprops :timer)))
      (cancel-timer timer)
      (ssp--put winprops :timer nil))
    (let* ((time (time-convert (time-since start-time) t))
           (position (+ (ssp--get winprops :start-position)
                        (/ (float (car time)) (cdr time)))))
      (ssp--put winprops :start-position position)
      (ssp--put winprops :start-time nil))))

(defun ssp-mode--seeked (winprops position)
  (ssp--put winprops :start-position position)
  (when (ssp--get winprops :start-time)
    (ssp--put winprops :start-time (current-time))
    (unless (ssp--get winprops :timer)
      (ssp--put winprops
                :timer (run-at-time t 1 #'ssp-mode--redisplay winprops)))))

(defun ssp-mode-toggle-play ()
  (interactive)
  (let* ((winprops (ssp-player-winprops))
         (buffer (ssp--get winprops :buffer)))
    (when (and (eq (current-buffer) buffer)
               (not (ssp--get winprops :loading)))
      (let ((modeprops (ssp-mode-winprops)))
        (if (ssp--get modeprops :start-time)
            (ssp-player--pause winprops (lambda () (ssp-mode--paused modeprops)))
          (when (< (ssp--get modeprops :start-position) ssp-mode--duration)
            (ssp-player--start winprops
                               (lambda () (ssp-mode--started modeprops)))))))))

(defun ssp-mode--seek-to (position)
  (let* ((winprops (ssp-player-winprops))
         (buffer (ssp--get winprops :buffer)))
    (when (and (eq (current-buffer) buffer)
               (not (ssp--get winprops :loading)))
      (let ((modeprops (ssp-mode-winprops)))
        (when-let ((timer (ssp--get modeprops :timer)))
          (cancel-timer timer)
          (ssp--put modeprops :timer nil))
        (let* ((A (or (ssp--get modeprops :A) 0))
               (B (or (ssp--get modeprops :B) ssp-mode--duration))
               (pos (min B (max A position))))
          (ssp-player--seek-to winprops pos
                               (lambda () (ssp-mode--seeked modeprops pos))))))))

(defun ssp-mode-seek (offset)
  (when ssp-mode--duration
    (when-let ((position (ssp-mode--get-current-position (ssp-mode-winprops))))
      (let ((start-position (+ position offset)))
        (ssp-mode--seek-to start-position)))))

(defun ssp-mode--mouse-position ()
  (when ssp-mode--duration
    (pcase-let ((`(,pos ,x . ,_y) (mouse-pixel-position)))
      (when (eq pos (window-frame))
        (let* ((fringes (window-fringes))
               (left (+ 1 (window-pixel-left) (car fringes)))
               (width (window-body-width nil t)))
          (* (/ (float (max 0 (- x left))) (- width 2)) ssp-mode--duration))))))

(defun ssp-mode-mouse-seek-to ()
  (interactive)
  (when-let ((position (ssp-mode--mouse-position)))
    (ssp-mode--seek-to position)))

(defun ssp-mode-seek-start ()
  (interactive)
  (ssp-mode--seek-to 0))

(defun ssp-mode-seek-end ()
  (interactive)
  (when ssp-mode--duration
    (ssp-mode--seek-to ssp-mode--duration)))

(defun ssp-mode-backward-10s ()
  (interactive)
  (ssp-mode-seek -10))

(defun ssp-mode-forward-10s ()
  (interactive)
  (ssp-mode-seek 10))

(defun ssp-mode-backward-1m ()
  (interactive)
  (ssp-mode-seek -60))

(defun ssp-mode-forward-1m ()
  (interactive)
  (ssp-mode-seek 60))

(defun ssp-mode-backward-10m ()
  (interactive)
  (ssp-mode-seek -600))

(defun ssp-mode-forward-10m ()
  (interactive)
  (ssp-mode-seek 600))

(defun ssp-mode-set-a ()
  (interactive)
  (when ssp-mode--duration
    (let ((winprops (ssp-mode-winprops)))
      (when-let ((position (ssp-mode--get-current-position winprops)))
        (ssp--put winprops :A position)
        (force-mode-line-update t)))))

(defun ssp-mode-set-b ()
  (interactive)
  (when ssp-mode--duration
    (let ((winprops (ssp-mode-winprops)))
      (when (ssp--get winprops :A)
        (when-let ((position (ssp-mode--get-current-position winprops)))
          (ssp--put winprops :B position)
          (force-mode-line-update t))))))

(defun ssp-mode-unset-a-b ()
  (interactive)
  (when ssp-mode--duration
    (let ((winprops (ssp-mode-winprops)))
      (ssp--put winprops :A nil)
      (ssp--put winprops :B nil)
      (force-mode-line-update t))))

(defvar ssp-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "SPC" #'ssp-mode-toggle-play)
    (keymap-set map "<mouse-1>" #'ssp-mode-mouse-seek-to)
    (keymap-set map "<remap> <beginning-of-buffer>" #'ssp-mode-seek-start)
    (keymap-set map "<remap> <end-of-buffer>" #'ssp-mode-seek-end)
    (keymap-set map "<remap> <backward-char>" #'ssp-mode-backward-10s)
    (keymap-set map "<remap> <forward-char>" #'ssp-mode-forward-10s)
    (keymap-set map "<remap> <backward-word>" #'ssp-mode-backward-1m)
    (keymap-set map "<remap> <forward-word>" #'ssp-mode-forward-1m)
    (keymap-set map "<remap> <backward-sentence>" #'ssp-mode-backward-10m)
    (keymap-set map "<remap> <forward-sentence>" #'ssp-mode-forward-10m)
    (keymap-set map "<remap> <set-mark-command>" #'ssp-mode-set-a)
    (keymap-set map "<remap> <narrow-to-region>" #'ssp-mode-set-b)
    (keymap-set map "<remap> <widen>" #'ssp-mode-unset-a-b)
    map)
  "keymap for ssp-mode")

(defun ssp-mode--svg (width height color)
  (list 'image
        :type 'svg
        :data (format
   "<svg width=\"1\" height=\"%s\">
<rect width=\"1\" height=\"2\" fill=\"#000000AA\"/>
<rect width=\"1\" height=\"%s\" y=\"2\" fill=\"%s\"/>
<rect width=\"1\" height=\"2\" y=\"%s\" fill=\"#000000AA\"/>
</svg>"
   height (- height 2) color (- height 2))
        :ascent 'center
        :width width
        :height height))

(defun ssp-mode--overlay-help (window _object _pos)
  (with-selected-window window
    (when-let ((position (ssp-mode--mouse-position)))
      (ssp-format-position position))))

(defun ssp-mode--ensure-overlay (winprops)
  (let ((window (car winprops)))
    (unless (ssp--get winprops :overlay)
      (with-current-buffer (window-buffer window)
        (let* ((height (frame-char-height (window-frame window)))
               (width (window-body-width window t))
               (overlay (make-overlay (point-min) (point-max)))
               (placeholder
                (list 'image
                      :type 'svg
                      :data "<svg width=\"1\" height=\"1\"/>"
                      :ascent 'center))
               (progress (ssp-mode--svg 1 height "#FFFFFFF0"))
               (remain (ssp-mode--svg (- width 1) height "#88888888")))
          (ssp--put winprops :overlay overlay)
          (ssp--put winprops :placeholder placeholder)
          (ssp--put winprops :progress progress)
          (ssp--put winprops :remain remain)
          (ssp--put winprops :position 0.0)
          (ssp--put winprops :ratio 0.0)
          (overlay-put overlay 'window window)
          (overlay-put overlay 'before-string
                       (concat (propertize " " 'display placeholder) "\n"))
          (overlay-put overlay 'display "")
          (overlay-put overlay 'after-string
                       (concat
                        (propertize " "
                                    'display progress
                                    'pointer 'arrow
                                    'help-echo #'ssp-mode--overlay-help)
                        (propertize " "
                                    'display remain
                                    'pointer 'arrow
                                    'help-echo #'ssp-mode--overlay-help))))))))

(defun ssp-mode--update-progress (winprops)
  (when-let ((duration (with-current-buffer (window-buffer (car winprops))
                         ssp-mode--duration))
             (position (ssp-mode--get-current-position winprops)))
    (let ((ratio (min 1.0 (max 0.0 (/ position duration)))))
      (ssp--put winprops :position position)
      (ssp--put winprops :ratio ratio)
      (when-let ((progress (ssp--get winprops :progress))
                 (remain (ssp--get winprops :remain)))
        (let* ((full-width (+ (ssp--get progress :width) (ssp--get remain :width)))
               (width (+ 1 (round (* ratio (- full-width 2))))))
          (ssp--put progress :width width)
          (ssp--put remain :width (- full-width width))))
      (when-let ((B (ssp--get winprops :B)))
        (when (>= position B)
          (ssp-player--pause
           (ssp-player-winprops (car winprops))
           (lambda () (ssp-mode--paused winprops))))))))

(defun ssp-mode--update-overlay (winprops)
  (let* ((window (car winprops))
         (placeholder (ssp--get winprops :placeholder))
         (progress (ssp--get winprops :progress))
         (remain (ssp--get winprops :remain))
         (ratio (ssp--get winprops :ratio))
         (full-width (window-body-width window t))
         (width (+ 1 (round (* ratio (- full-width 2)))))
         (height (- (window-body-height window t)
                    (frame-char-height (window-frame window)))))
    (ssp--put placeholder :width full-width)
    (ssp--put placeholder :height height)
    (ssp--put progress :width width)
    (ssp--put remain :width (- full-width width))))

(defun ssp-mode--pre-redisplay-h (window)
  (ssp-mode--update-progress (ssp-mode-winprops window))
  (force-mode-line-update t))

(defun ssp-mode--size-change-h (window)
  (ssp-mode--update-overlay (ssp-mode-winprops window))
  (ssp-player--update-frame-size (ssp-player-winprops window)))

(defun ssp-mode--split-window (window size side)
  (let* ((ignore-window-parameters t)
         (new-window (split-window window size side)))
    (with-current-buffer (window-buffer window)
     (when (derived-mode-p 'ssp-mode)
       (let ((new-props (ssp-mode-winprops new-window))
             (props (ssp-mode-winprops window)))
         (dolist (prop '(:start-position :A :B))
           (ssp--put new-props prop (ssp--get props prop))))))
    new-window))

(defun ssp-mode--buffer-change-h (window)
  (set-window-parameter window 'split-window #'ssp-mode--split-window)
  (when (ssp-player--switch-to (ssp-player-winprops window) (window-buffer window))
    (ssp-mode--ensure-overlay (ssp-mode-winprops window))))

;;;###autoload
(define-derived-mode ssp-mode special-mode "SSP"
  "Major mode to play media files"
  :group 'ssp
  :abbrev-table nil
  :syntax-table nil
  (setq-local buffer-auto-save-file-name nil)
  (setq-local cursor-type nil)
  (add-hook 'pre-redisplay-functions #'ssp-mode--pre-redisplay-h nil t)
  (add-hook 'window-size-change-functions #'ssp-mode--size-change-h nil t)
  (add-hook 'window-buffer-change-functions #'ssp-mode--buffer-change-h nil t)
  (when (eq (window-buffer) (current-buffer))
    (ssp-mode--buffer-change-h (selected-window)))
  (ssp-position-mode t)
  (ssp-region-mode t))

(defun ssp-mode--window-configuration-change-h ()
  (let ((ssp-player-winprops-alist
         (seq-filter
          (lambda (winprops)
            (or (window-live-p (car winprops))
                (and (ssp-player--quit winprops) nil)))
          (frame-parameter nil 'ssp-player-winprops-alist))))
    (dolist (winprops ssp-player-winprops-alist)
      (when-let ((buffer (ssp--get winprops :buffer))
                 (frame (ssp--get winprops :frame)))
        (unless (eq buffer (window-buffer (car winprops)))
          (when (window-parameter (car winprops) 'split-window)
            (set-window-parameter (car winprops) 'split-window nil)
            (make-frame-invisible frame)
            (ssp-player--pause winprops)
            (when (buffer-live-p buffer)
              (let ((modeprops (ssp-mode-winprops (car winprops) buffer)))
                (ssp-mode--paused modeprops)
                (when-let ((position (ssp--get modeprops :start-position)))
                  (with-current-buffer buffer
                    (setq ssp-mode--last-position position)))))))))
    (set-frame-parameter nil 'ssp-player-winprops-alist ssp-player-winprops-alist)))

(defun ssp-mode--delete-frame-h (frame)
  (dolist (winprops (frame-parameter frame 'ssp-player-winprops-alist))
    (ssp-player--quit winprops)))

(add-hook 'window-configuration-change-hook #'ssp-mode--window-configuration-change-h)
(add-hook 'delete-frame-functions #'ssp-mode--delete-frame-h)

(defun ssp-format-position (d)
  (let* ((seconds (floor d))
         (minutes (floor seconds 60))
         (hours (floor minutes 60)))
    (format "%02d:%02d:%02d" hours (% minutes 60) (% seconds 60))))

(defun ssp-position-mode-line ()
  (when ssp-mode--duration
    (when-let ((position (ssp--get (ssp-mode-winprops) :position)))
      (format " %s/%s"
              (ssp-format-position position)
              (ssp-format-position ssp-mode--duration)))))

(define-minor-mode ssp-position-mode
  "position mode line"
  :lighter (:eval (ssp-position-mode-line)))

(defun ssp-region-mode-line ()
  (when ssp-mode--duration
    (let* ((winprops (ssp-mode-winprops))
           (A (ssp--get winprops :A))
           (B (ssp--get winprops :B)))
      (when (or A B)
        (format " A:%s B:%s"
                (if A (ssp-format-position A) "")
                (if B (ssp-format-position B) ""))))))

(define-minor-mode ssp-region-mode
  "narrow to A-B region"
  :lighter (:eval (ssp-region-mode-line)))

(provide 'ssp)
;;; ssp.el ends here
