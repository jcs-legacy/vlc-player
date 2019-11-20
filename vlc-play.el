;;; vlc-play.el --- Play video using VLC.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-20 13:28:20

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Play video using VLC.
;; Keyword: video vlc buffering images
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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


(defgroup vlc-play nil
  "Play video using VLC."
  :prefix "vlc-play-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/vlc-play"))


(defcustom vlc-play-images-directory "~/.emacs.d/vlc-play/images/"
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

(defconst vlc-play--command-video-to-images
  "vlc \"%s\" --sub-track=0 --rate=1 --video-filter=scene --vout=dummy --scene-format=png --scene-ratio=24 --scene-prefix=%s --scene-path=\"%s\" vlc://quit"
  "Command that convert video to images.")

(defvar vlc-play--buffer nil
  "Buffer that displays video.")

(defvar vlc-play--buffer-timer nil
  "Timer that will update the image buffer.")


(defun vlc-play--form-command (path source)
  "From the command by needed parameters.
PATH is the input video file.  SOURCE is the output image directory."
  (format vlc-play--command-video-to-images
          path vlc-play-image-prefix source))

(defun vlc-play--safe-path (path)
  "Check if safe PATH."
  (unless (file-exists-p path) (setq path (expand-file-name path)))
  (if (file-exists-p path) path nil))

(defun vlc-play--buffer-name (path)
  "Return current vlc play buffer name by PATH."
  (format vlc-play-buffer-name (f-filename path)))

(defun vlc-play--create-video-buffer (path)
  "Create a new video buffer with PATH."
  (let* ((name (vlc-play--buffer-name path))
         (buf (if (get-buffer name) (get-buffer name) (generate-new-buffer name))))
    (with-current-buffer buf
      (erase-buffer)
      (insert "[Nothing to play yet...]"))
    buf))

(defun vlc-play--video (path)
  "Play the video with PATH."
  (setq path (vlc-play--safe-path path))
  (if (not path)
      (user-error "[ERROR] Input video file doesn't exists: %s" path)
    (unless (file-directory-p (expand-file-name vlc-play-images-directory))
      (make-directory (expand-file-name vlc-play-images-directory) t))
    (let ((converting ((shell-command (vlc-play--form-command path vlc-play-images-directory)))))
      (if (not (= converting 0))
          (user-error "[ERROR] Failed to convert to images: %s" converting)
        (setq vlc-play--buffer (vlc-play--create-video-buffer path))
        (pop-to-buffer vlc-play--buffer))
      )
    ))

(vlc-play--video (expand-file-name "./test/1.avi"))


(provide 'vlc-play)
;;; vlc-play.el ends here
