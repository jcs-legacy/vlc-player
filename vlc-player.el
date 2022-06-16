;;; vlc-player.el --- Play video using VLC  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-20 13:28:20

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/vlc-player
;; Version: 0.0.8
;; Package-Requires: ((emacs "24.3") (s "1.12.0"))
;; Keywords: video vlc buffering images

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Play video using VLC.
;;

;;; Code:

(require 's)


(defgroup vlc-player nil
  "Play video using VLC."
  :prefix "vlc-player-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/vlc-player"))


(defcustom vlc-player-images-directory (format "%s%s"
                                             user-emacs-directory
                                             "vlc-play/images/")
  "Directory that stores video images."
  :type 'string
  :group 'vlc-player)

(defcustom vlc-player-buffer-name "*vlc-play* : %s"
  "Buffer name of the video player."
  :type 'string
  :group 'vlc-player)

(defcustom vlc-player-image-prefix "snap"
  "Prefix when output images."
  :type 'string
  :group 'vlc-player)

(defcustom vlc-player-image-extension "jpg"
  "Image extension when output from VLC."
  :type 'string
  :group 'vlc-player)

(defcustom vlc-player-prefer-fps 30
  "Prefer FPS that will play the video.
Current doesn't have ideal solution for this; I will have to ask user to enter
the FPS manually.  Todo will have to find out a way to get the FPS information
from the input video file."
  :type 'string
  :group 'vlc-player)

(defconst vlc-player--command-video-to-images
  "vlc \"%s\" %s --scene-path=\"%s\" vlc://quit"
  "Command that convert video to image source.")

(defconst vlc-player--command-video-to-audio
  ""
  "Command that convert video to audio source.")

(defconst vlc-player--command-kill-process
  "vlc vlc://quit"
  "Command that kill VLC process.")


(defvar vlc-player--frame-regexp nil
  "Frame regular expression for matching length.")

(defvar vlc-player--current-fps 0.0 "Current FPS that are being used.")
(defvar vlc-player--first-frame-time 0.2 "Time to check if the first frame exists.")
(defvar vlc-player--buffer-time 0.0 "Time to update buffer frame, calculate with FPS.")

(defvar vlc-player--start-time 0.0 "Record video start time.")
(defvar vlc-player--video-timer 0.0 "Time to record delta time.")

(defvar vlc-player--frame-index 0 "Current frame index/counter.")

(defvar vlc-player--first-frame-timer nil "Timer that find out the first frame.")

(defvar vlc-player--buffer nil "Buffer that displays video.")
(defvar vlc-player--buffer-timer nil "Timer that will update the image buffer.")


;;; Command

(defun vlc-player--form-command-list (lst)
  "Form the command by LST."
  (let ((output ""))
    (dolist (cmd lst)
      (setq output (concat output cmd " ")))
    output))

(defun vlc-player--form-command (path source)
  "From the command by needed parameters.
PATH is the input video file.  SOURCE is the output image directory."
  (format vlc-player--command-video-to-images
          path
          (vlc-player--form-command-list
           (list "--sub-track=0"              ; subtitle track
                 "--rate=1"                   ; playback speed
                 "--video-filter=scene"       ; post processing
                 ;;"-Idummy"                    ; Don't show GUI.
                 "--vout=dummy"               ; video output
                 (format "--scene-format=%s"  ; extension
                         vlc-player-image-extension)
                 "--scene-ratio=1"            ; FPS thumbnails
                 ;;"--scene-replace"            ; Always write to the same file
                 ;;"--no-audio"                 ; no audio
                 (format "--scene-prefix=%s"  ;
                         vlc-player-image-prefix)))
          source))

(defun vlc-player--kill-vlc ()
  "Kill VLC by command."
  (shell-command vlc-player--command-kill-process))

;;; Util

(defun vlc-player--safe-path (path)
  "Check if safe PATH."
  (unless (file-exists-p path) (setq path (expand-file-name path)))
  (if (file-exists-p path) path nil))

(defun vlc-player--clean-video-images ()
  "Clean up all video images."
  (delete-directory (expand-file-name vlc-player-images-directory) t))

(defun vlc-player--ensure-video-directory-exists ()
  "Ensure the video directory exists so we can put our image files."
  (unless (file-directory-p (expand-file-name vlc-player-images-directory))
    (make-directory (expand-file-name vlc-player-images-directory) t)))

;;; Buffer

(defun vlc-player--buffer-name (path)
  "Return current vlc play buffer name by PATH."
  (format vlc-player-buffer-name (f-filename path)))

(defun vlc-player--create-video-buffer (path)
  "Create a new video buffer with PATH."
  (let* ((name (vlc-player--buffer-name path))
         (buf (if (get-buffer name) (get-buffer name) (generate-new-buffer name))))
    (setq vlc-player--buffer buf)
    (with-current-buffer buf (vlc-player-mode))
    (vlc-player--update-frame-by-string "[Nothing to display yet...]")
    buf))

(defun vlc-player--buffer-alive-p ()
  "Check if the video buffer alive."
  (buffer-name (get-buffer vlc-player--buffer)))

;;; First frame

(defun vlc-player--set-first-frame-timer ()
  "Set the first frame timer task."
  (vlc-player--kill-first-frame-timer)
  (setq vlc-player--first-frame-timer
        (run-with-timer vlc-player--first-frame-time nil 'vlc-player--check-first-frame)))

(defun vlc-player--kill-first-frame-timer ()
  "Kill the first frame timer.
Information about first frame timer please see variable `vlc-player--first-frame-timer'."
  (when (timerp vlc-player--first-frame-timer)
    (cancel-timer vlc-player--first-frame-timer)
    (setq vlc-player--first-frame-timer nil)))

(defun vlc-player--form-file-extension-regexp ()
  "Form regular expression for search image file."
  (format "\\.%s$" vlc-player-image-extension))

(defun vlc-player--check-first-frame ()
  "Core function to check first frame image is ready."
  (let ((images (directory-files (expand-file-name vlc-player-images-directory) nil (vlc-player--form-file-extension-regexp)))
        (first-frame nil))
    (if (not images)
        (vlc-player--set-first-frame-timer)
      (setq first-frame (nth 0 images))
      (setq first-frame (s-replace vlc-player-image-prefix "" first-frame))
      (setq first-frame (s-replace-regexp (vlc-player--form-file-extension-regexp) "" first-frame))
      (setq vlc-player--frame-regexp (format "%s%sd" "%0" (length first-frame)))
      (setq vlc-player--start-time (float-time))
      (vlc-player--update-frame))))

;;; Frame

(defun vlc-player--set-buffer-timer ()
  "Set the buffer timer task."
  (vlc-player--kill-buffer-timer)
  (setq vlc-player--buffer-timer
        (run-with-timer vlc-player--buffer-time nil 'vlc-player--update-frame)))

(defun vlc-player--kill-buffer-timer ()
  "Kill the buffer timer."
  (when (timerp vlc-player--buffer-timer)
    (cancel-timer vlc-player--buffer-timer)
    (setq vlc-player--buffer-timer nil)))

(defun vlc-player--form-frame-filename ()
  "Form the frame filename."
  (format "%s%s.%s"
          vlc-player-image-prefix
          (format vlc-player--frame-regexp vlc-player--frame-index)
          vlc-player-image-extension))

(defun vlc-player--update-frame-by-image-path (path)
  "Update the frame by image PATH."
  (if (not (vlc-player--buffer-alive-p))
      (vlc-player--clean-up)
    (with-current-buffer vlc-player--buffer
      (erase-buffer)
      (insert-image-file path))))

(defun vlc-player--update-frame-by-string (str)
  "Update the frame by STR."
  (if (not (vlc-player--buffer-alive-p))
      (vlc-player--clean-up)
    (with-current-buffer vlc-player--buffer
      (erase-buffer)
      (insert str))))

(defun vlc-player--update-frame ()
  "Core logic to update frame."
  (if (not (vlc-player--buffer-alive-p))
      (user-error "[WARNING] Display buffer no longer alived")
    ;; Calculate the time passed.
    (setq vlc-player--video-timer (- (float-time) vlc-player--start-time))
    ;; Calculate the frame index.
    (setq vlc-player--frame-index (ceiling (* vlc-player--current-fps vlc-player--video-timer)))
    (message "frame index: %s" vlc-player--frame-index)
    (let ((frame-file (concat vlc-player-images-directory (vlc-player--form-frame-filename))))
      (if (file-exists-p frame-file)
          (progn
            (vlc-player--update-frame-by-image-path frame-file)
            (vlc-player--set-buffer-timer))
        (vlc-player--update-frame-by-string "[Done display...]")))))

;;; Core

(defun vlc-player--ask-fps ()
  "Ask use of FPS."
  (setq vlc-player--current-fps (read-number "Video FPS: " vlc-player-prefer-fps))
  (setq vlc-player--buffer-time (/ 1.0 vlc-player--current-fps)))

(defun vlc-player--clean-up ()
  "Reset/Clean up some variable before we play a new video."
  (vlc-player--kill-first-frame-timer)
  (vlc-player--kill-buffer-timer)
  (setq vlc-player--buffer nil)
  (setq vlc-player--start-time 0.0)
  (setq vlc-player--video-timer 0.0)
  (setq vlc-player--current-fps 0.0)
  (setq vlc-player--frame-index 0)
  (setq vlc-player--frame-regexp nil))

(defun vlc-player--video (path)
  "Play the video with PATH."
  (setq path (vlc-player--safe-path path))
  (if (not path)
      (user-error "[ERROR] Input video file doesn't exists: %s" path)
    (vlc-player--clean-video-images)
    (vlc-player--ensure-video-directory-exists)
    (vlc-player--clean-up)
    (vlc-player--ask-fps)
    (let ((converting (shell-command (vlc-player--form-command path vlc-player-images-directory))))
      (if (not (= converting 0))
          (user-error "[ERROR] Failed to convert to images: %s" converting)
        (vlc-player--create-video-buffer path)
        (switch-to-buffer-other-window vlc-player--buffer))
      (vlc-player--check-first-frame)
      )
    ))


(define-derived-mode vlc-player-mode fundamental-mode "vlc-player"
  "Major mode for play vlc video."
  :group 'ffmpeg-player
  (buffer-disable-undo)
  )


(setq vlc-player-image-extension "jpg")
(vlc-player--clean-up)
(vlc-player--ask-fps)
(setq vlc-player--buffer (vlc-player--create-video-buffer "test-clip"))
(switch-to-buffer-other-window vlc-player--buffer)
(vlc-player--check-first-frame)
;;(vlc-player--video (expand-file-name "./test/3.mp4"))


(provide 'vlc-player)
;;; vlc-player.el ends here
