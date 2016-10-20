;;; podcaster.el --- Podcast client -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by lujun9972

;; Author: DarkSun <lujun9972@gmail.com>
;; URL: https://github.com/lujun9972/podcaster
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; podcaster.el is an podcast client which is derived from syohex's emacs-rebuildfm
;;
;; podcaster.el provides showing podscasts list.
;; Its actions are
;;   - Play podcast mp3(requires `avplay' or `ffplay' or `itunes')
;;

;;; Code:

(require 'xml)
(require 'url)
(require 'cl-lib)
(require 'nnrss)

(declare-function do-applescript "nsfns.m")

(defgroup podcaster nil
  "podcaster client"
  :group 'applications)

(defcustom podcaster-mp3-player (or (when (eq system-type 'darwin)
                                      "itunes")
                                    (executable-find "avplay")
                                    (executable-find "ffplay"))
  "MP3 player for playing podcast.
The player should be able to open an mp3 URL."
  :type 'string
  :group 'podcaster)

(defcustom podcaster-mp3-player-extra-params nil
  "Extra params for MP3 player.
It should be a list of params which make the player playing mp3 without gui and quit automatically
If the player is avplay or ffplay or itunes, you don't have to set the params"
  :type '(repeat string)
  :group 'podcaster)


(defcustom podcaster-play-podcast-hook nil
  "Hook that gets run after podcast is played."
  :type 'hook
  :group 'podcaster)

(defcustom podcaster-feeds-urls
  '("https://ipn.li/kernelpanic/feed"
    "http://sachachua.com/blog/tag/emacs-chat/podcast" )
  "The RSS Feed URL list."
  :type 'list
  :group 'podcaster)

(defsubst podcaster--extract-tag-value (tag tree)
  (cadr (assoc-default tag tree)))

(defsubst podcaster--extract-tag-attribute (tag attribute tree)
  (assoc-default attribute (car (assoc-default tag tree))))

(defsubst podcaster--construct-item (item &optional channel-title)
  (let ((tree (cl-remove-if-not (lambda (e)
                                  (and e (listp e))) item))
        (channel-title (or channel-title "")))
    (let ((title (concat channel-title " - " (podcaster--extract-tag-value 'title tree)))
          (link  (podcaster--extract-tag-value 'link tree))
          (pubdate (podcaster--extract-tag-value 'pubDate tree))
          (summary (podcaster--extract-tag-value 'summary tree))
          (mp3-url (podcaster--extract-tag-attribute 'enclosure 'url tree)))
      (cons title
            (list :title title :link link :summary summary
                  :pubdate pubdate :mp3-url mp3-url)))))

(defun podcaster--get-feeds-from-channel (channel)
  (let* ((items (nnrss-find-el 'item channel))
         (channel-title (podcaster--extract-tag-value 'title channel)))
    (mapcar (lambda (item) (podcaster--construct-item item channel-title))  items )))

(defun podcaster--get-feeds-from-url (url)
  (let* ((feed (nnrss-fetch url))
         (channels (nnrss-find-el 'channel feed))
         (new-feed-url (nth 2 (car (nnrss-find-el 'itunes:new-feed-url feed))))
         (channel-title ()))
    (if (and new-feed-url
             (not (equal url new-feed-url))) ;some rss feed have the same value
        (podcaster--get-feeds-from-url new-feed-url)
      (cl-mapcan #'podcaster--get-feeds-from-channel channels))))

(defun podcaster--get-feeds (urls)
  (let ((feeds (cl-mapcan #'podcaster--get-feeds-from-url urls)))
    (sort feeds (lambda (item1 item2)
                  (let ((pubdate1 (date-to-time (plist-get (cdr item1) :pubdate)))
                        (pubdate2 (date-to-time (plist-get (cdr item2) :pubdate))))
                    (time-less-p pubdate2 pubdate1 ))))))

;; (podcaster--get-feeds '("https://ipn.li/kernelpanic/feed" "http://sachachua.com/blog/tag/emacs-chat/podcast"))

(defun podcaster--collect-podcasts ()
  (podcaster--get-feeds podcaster-feeds-urls))

(defun podcaster--mp3-player-command (cmd url)
  (let ((extra-params podcaster-mp3-player-extra-params))
    (when (member (file-name-nondirectory cmd) '("avplay" "ffplay"))
      (cl-pushnew "-autoexit" extra-params)
      (cl-pushnew "-nodisp" extra-params))
    `(,cmd ,@extra-params ,url)))

(defsubst podcaster--use-itunes-p ()
  (string= "itunes" podcaster-mp3-player))

(defun podcaster--play-itunes (url)
  (do-applescript
   (format "
tell application \"iTunes\"
  open location \"%s\"
  play
end tell" url)))

(defun podcaster--play-podcast (item)
  (let ((mp3-url (plist-get item :mp3-url))
        (buf (get-buffer-create "*podcaster mp3*")))
    (if (podcaster--use-itunes-p)
        (podcaster--play-itunes mp3-url)
      (apply 'start-file-process
             "podcaster-mp3" buf
             (podcaster--mp3-player-command podcaster-mp3-player mp3-url))
      (run-hook-with-args 'podcaster-play-podcast-hook item))))


;;;###autoload
(defun podcaster ()
  "Play podcasts."
  (interactive)
  (let* ((items (podcaster--collect-podcasts))
         (titles (mapcar #'car items))
         (title (completing-read "Podcasts: " titles))
         (item (cdr (assoc-string title items))))
    (when (ignore-errors (podcaster--player-process))
      (podcaster-stop))
    (podcaster--play-podcast item)))

(defun podcaster--stop-itunes ()
  (do-applescript
   "tell application \"iTunes\"
      stop
    end tell"))

(defun podcaster--player-process ()
  (let ((buf (get-buffer "*podcaster mp3*")))
    (if (not buf)
        (error "process buffer is already deleted")
      (let ((proc (get-buffer-process buf)))
        (unless proc
          (error "mp3 player process is already dead"))
        proc))))

;;;###autoload
(defun podcaster-stop ()
  "Stop the currently-playing podcast."
  (interactive)
  (when (yes-or-no-p "Stop MP3 Player? ")
    (if (podcaster--use-itunes-p)
        (podcaster--stop-itunes)
      (let ((proc (podcaster--player-process)))
        (kill-process proc)))))

(defun podcaster--playpause-itunes ()
  (do-applescript
   "tell application \"iTunes\"
      playpause
    end tell"))

;;;###autoload
(defun podcaster-pause ()
  (interactive)
  (if (podcaster--use-itunes-p)
      (podcaster--playpause-itunes)
    (let ((proc (podcaster--player-process)))
      (signal-process proc 'SIGSTOP))))

;;;###autoload
(defun podcaster-resume ()
  "Resume the currently-playing podcast."
  (interactive)
  (if (podcaster--use-itunes-p)
      (podcaster--playpause-itunes)
    (let ((proc (podcaster--player-process)))
      (signal-process proc 'SIGCONT))))

(provide 'podcaster)

;;; podcaster.el ends here
