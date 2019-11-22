;;; vlc-play.el --- Play video using VLC.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-20 13:28:20

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Play video using VLC.
;; Keyword: video vlc buffering images
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (s "1.12.0"))
;; URL: https://github.com/jcs090218/vlc-play

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


(defgroup vlc-play nil
  "Play video using VLC."
  :prefix "vlc-play-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/vlc-play"))


(defcustom vlc-play-images-directory (format "%s%s"
                                             user-emacs-directory
                                             "vlc-play/images/")
  "Directory that stores video images."
  :type 'string
  :group 'vlc-play)

(defcustom vlc-play-buffer-name "*vlc-play* : %s"
  "Buffer name of the video player."
  :type 'string
  :group 'vlc-play)

(defcustom vlc-play-image-prefix "snap"
  "Prefix when output images."
  :type 'string
  :group 'vlc-play)

(defcustom vlc-play-image-extension "png"
  "Image extension when output from VLC."
  :type 'string
  :group 'vlc-play)

(defcustom vlc-play-prefer-fps 30
  "Prefer FPS that will play the video.
Current doesn't have ideal solution for this; I will have to ask user to enter
the FPS manually.  Todo will have to find out a way to get the FPS information
from the input video file."
  :type 'string
  :group 'vlc-play)

(defconst vlc-play--command-video-to-images
  "vlc \"%s\" %s --scene-path=\"%s\" vlc://quit"
  "Command that convert video to image source.")

(defconst vlc-play--command-video-to-audio
  ""
  "Command that convert video to audio source.")

(defconst vlc-play--command-kill-process
  "vlc vlc://quit"
  "Command that kill VLC process.")


(defvar vlc-play--frame-regexp nil
  "Frame regular expression for matching length.")

(defvar vlc-play--current-fps 0 "Current FPS that are being used.")
(defvar vlc-play--first-frame-time 0.2 "Time to check if the first frame exists.")
(defvar vlc-play--buffer-time 0 "Time to update buffer frame, calculate with FPS.")

(defvar vlc-play--frame-index 0 "Current frame index/counter.")

(defvar vlc-play--first-frame-timer nil "Timer that find out the first frame.")
(defvar vlc-play--first-frame nil "Flag to check if the first frame are ready.")

(defvar vlc-play--buffer nil "Buffer that displays video.")
(defvar vlc-play--buffer-timer nil "Timer that will update the image buffer.")


;;; Command

(defun vlc-play--form-command-list (lst)
  "Form the command by LST."
  (let ((output ""))
    (dolist (cmd lst)
      (setq output (concat output cmd " ")))
    output))

(defun vlc-play--form-command (path source)
  "From the command by needed parameters.
PATH is the input video file.  SOURCE is the output image directory."
  (format vlc-play--command-video-to-images
          path
          (vlc-play--form-command-list
           (list "--sub-track=0"              ; subtitle track
                 "--rate=1"                   ; playback speed
                 "--video-filter=scene"       ; post processing
                 ;;"-Idummy"                    ; Don't show GUI.
                 "--vout=dummy"               ; video output
                 "--scene-format=png"         ; extension
                 "--scene-ratio=1"            ; FPS thumbnails
                 ;;"--no-audio"                 ; no audio
                 (format "--scene-prefix=%s"  ;
                         vlc-play-image-prefix)))
          source))

;;; Util

(defun vlc-play--safe-path (path)
  "Check if safe PATH."
  (unless (file-exists-p path) (setq path (expand-file-name path)))
  (if (file-exists-p path) path nil))

(defun vlc-play--clean-video-images ()
  "Clean up all video images."
  (delete-directory (expand-file-name vlc-play-images-directory) t))

(defun vlc-play--ensure-video-directory-exists ()
  "Ensure the video directory exists so we can put our image files."
  (unless (file-directory-p (expand-file-name vlc-play-images-directory))
    (make-directory (expand-file-name vlc-play-images-directory) t)))

;;; Buffer

(defun vlc-play--buffer-name (path)
  "Return current vlc play buffer name by PATH."
  (format vlc-play-buffer-name (f-filename path)))

(defun vlc-play--create-video-buffer (path)
  "Create a new video buffer with PATH."
  (let* ((name (vlc-play--buffer-name path))
         (buf (if (get-buffer name) (get-buffer name) (generate-new-buffer name))))
    (setq vlc-play--buffer buf)
    (with-current-buffer buf (buffer-disable-undo))
    (vlc-play--update-frame-by-string "[Nothing to display yet...]")
    (pop-to-buffer buf)
    buf))

;;; First frame

(defun vlc-play--set-first-frame-timer ()
  "Set the first frame timer task."
  (vlc-play--kill-first-frame-timer)
  (setq vlc-play--first-frame-timer
        (run-with-timer vlc-play--first-frame-time nil 'vlc-play--check-first-frame)))

(defun vlc-play--kill-first-frame-timer ()
  "Kill the first frame timer.
Information about first frame timer please see variable `vlc-play--first-frame-timer'."
  (when (timerp vlc-play--first-frame-timer)
    (cancel-timer vlc-play--first-frame-timer)
    (setq vlc-play--first-frame-timer nil)))

(defun vlc-play--form-file-extension-regexp ()
  "Form regular expression for search image file."
  (format "\\.%s$" vlc-play-image-extension))

(defun vlc-play--check-first-frame ()
  "Core function to check first frame image is ready."
  (let ((images (directory-files (expand-file-name vlc-play-images-directory) nil (vlc-play--form-file-extension-regexp)))
        (first-frame nil))
    (if (not images)
        (vlc-play--set-first-frame-timer)
      (setq first-frame (nth 0 images))
      (setq first-frame (s-replace vlc-play-image-prefix "" first-frame))
      (setq first-frame (s-replace-regexp (vlc-play--form-file-extension-regexp) "" first-frame))
      (setq vlc-play--frame-regexp (format "%s%sd" "%0" (length first-frame)))
      ;; TODO: Something goes wrong here...
      (setq vlc-play--current-fps (read-number "Video FPS: " vlc-play-prefer-fps))
      (setq vlc-play--buffer-time (/ 1.0 vlc-play--current-fps))
      (vlc-play--update-frame))))

;;; Frame

(defun vlc-play--set-buffer-timer ()
  "Set the buffer timer task."
  (vlc-play--kill-buffer-timer)
  (setq vlc-play--buffer-timer
        (run-with-timer vlc-play--buffer-time nil 'vlc-play--update-frame)))

(defun vlc-play--kill-buffer-timer ()
  "Kill the buffer timer."
  (when (timerp vlc-play--buffer-timer)
    (cancel-timer vlc-play--buffer-timer)
    (setq vlc-play--buffer-timer nil)))

(defun vlc-play--form-frame-filename ()
  "Form the frame filename."
  (format "%s%s.%s"
          vlc-play-image-prefix
          (format vlc-play--frame-regexp vlc-play--frame-index)
          vlc-play-image-extension))

(defun vlc-play--update-frame-by-image-path (path)
  "Update the frame by image PATH."
  (with-current-buffer vlc-play--buffer
    (erase-buffer)
    (insert-image-file path)))

(defun vlc-play--update-frame-by-string (str)
  "Update the frame by STR."
  (with-current-buffer vlc-play--buffer
    (erase-buffer)
    (insert str)))

(defun vlc-play--update-frame ()
  "Core logic to update frame."
  (setq vlc-play--frame-index (1+ vlc-play--frame-index))  ; increment frame
  (let ((frame-file (concat vlc-play-images-directory (vlc-play--form-frame-filename))))
    (if (file-exists-p frame-file)
        (progn
          (vlc-play--update-frame-by-image-path frame-file)
          (vlc-play--set-buffer-timer))
      (vlc-play--update-frame-by-string "[Done display...]"))))

;;; Core

(defun vlc-play--reset ()
  "Reset some variable before we play a new video."
  (vlc-play--kill-first-frame-timer)
  (vlc-play--kill-buffer-timer)
  (setq vlc-play--current-fps 0)
  (setq vlc-play--frame-index 0)
  (setq vlc-play--frame-regexp nil)
  (setq vlc-play--first-frame nil))

(defun vlc-play--video (path)
  "Play the video with PATH."
  (setq path (vlc-play--safe-path path))
  (if (not path)
      (user-error "[ERROR] Input video file doesn't exists: %s" path)
    (vlc-play--clean-video-images)
    (vlc-play--ensure-video-directory-exists)
    (vlc-play--reset)
    (let ((converting (shell-command (vlc-play--form-command path vlc-play-images-directory))))
      (if (not (= converting 0))
          (user-error "[ERROR] Failed to convert to images: %s" converting)
        (vlc-play--create-video-buffer path))
      (vlc-play--check-first-frame)
      )
    ))

(setq vlc-play--buffer (vlc-play--create-video-buffer "test-clip"))
(vlc-play--reset)
(vlc-play--check-first-frame)
;;(vlc-play--video (expand-file-name "./test/3.mp4"))


(provide 'vlc-play)
;;; vlc-play.el ends here
