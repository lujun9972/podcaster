;;; podcaster.el --- Emacs podcast client -*- lexical-binding: t; -*-

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

;; podcaster.el is a emacs podcast client
;;
;; podcaster.el provides showing podscasts list.
;; Its actions are
;;   - Play podcast mp3(requires `avplay' or `ffplay' or `itunes')
;;   - Browse podcast page
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

(defcustom podcaster-mp3-player (or (and (executable-find "avplay") "avplay")
                                    (and (executable-find "ffplay") "ffplay"))
  "MP3 player for playing podcast. The player should support
to open mp3 URL."
  :type 'string
  :group 'podcaster)

(defcustom podcaster-play-podcast-hook nil
  "Hook that gets run after podcast is played"
  :type 'hook
  :group 'podcaster)

(defcustom podcaster--feeds-url  "https://ipn.li/kernelpanic/feed"
  "The RSS Feed URL"
  :type 'string
  :group 'podcaster)

;; (nnrss-fetch  "http://feeds.rebuild.fm/podcaster")
;; (nnrss-fetch "https://ipn.li/kernelpanic/feed")

(defsubst podcaster--extract-tag-value (tag tree)
  (cadr (assoc-default tag tree)))

(defsubst podcaster--extract-tag-attribute (tag attribute tree)
  (assoc-default attribute (car (assoc-default tag tree))))

(defsubst podcaster--construct-item (item)
  (let ((tree (cl-remove-if-not (lambda (e)
                                  (and e (listp e))) item)))
    (let ((title (podcaster--extract-tag-value 'title tree))
          (link  (podcaster--extract-tag-value 'link tree))
          (pubdate (podcaster--extract-tag-value 'pubDate tree))
          (summary (podcaster--extract-tag-value 'summary tree))
          (mp3-url (podcaster--extract-tag-attribute 'enclosure 'url tree)))
      (cons title
            (list :title title :link link :summary summary
                  :pubdate pubdate :mp3-url mp3-url)))))

(defun podcaster--get-feeds (url)
  (let* ((feed (nnrss-fetch url))
         (items (nnrss-find-el 'item feed))
         (new-feed-url (nth 2 (car (nnrss-find-el 'itunes:new-feed-url feed)))))
    (if (and new-feed-url
             (not (equal url new-feed-url))) ;some rss feed have the same value
        (podcaster--get-feeds new-feed-url)
      (mapcar #'podcaster--construct-item items))))

;; (podcaster--get-feeds   "https://ipn.li/kernelpanic/feed")

(defun podcaster--collect-podcasts ()
  (podcaster--get-feeds podcaster--feeds-url))

(defun podcaster--mp3-player-command (cmd url)
  (cond ((member cmd '("avplay" "ffplay"))
         (list cmd "-autoexit" "-nodisp" url))
        (t
         (error "'%s' is not supported!!" cmd))))

(defsubst podcaster--macosx-p ()
  (eq system-type 'darwin))

(defun podcaster--play-itunes (url)
  (interactive "")
  (do-applescript
   (format "
tell application \"iTunes\"
  open location \"%s\"
  play
end tell" url)))

(defun podcaster--play-podcast (item)
  (let ((mp3-url (plist-get item :mp3-url))
        (buf (get-buffer-create "*podcaster mp3*")))
    (if (podcaster--macosx-p)
        (podcaster--play-itunes mp3-url)
      (apply 'start-file-process
             "podcaster-mp3" buf
             (podcaster--mp3-player-command podcaster-mp3-player mp3-url))
      (run-hook-with-args 'podcaster-play-podcast-hook item))))


;;;###autoload
(defun podcaster ()
  (interactive)
  (let* ((items (podcaster--collect-podcasts))
         (titles (mapcar #'car items))
         (title (completing-read "Podcasts" titles))
         (item (cdr (assoc-string title items))))
    (ignore-errors (podcaster-stop))
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
  (interactive)
  (when (yes-or-no-p "Stop MP3 Player? ")
    (if (podcaster--macosx-p)
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
  (if (podcaster--macosx-p)
      (podcaster--playpause-itunes)
    (let ((proc (podcaster--player-process)))
      (signal-process proc 'SIGSTOP))))

;;;###autoload
(defun podcaster-resume ()
  (interactive)
  (if (podcaster--macosx-p)
      (podcaster--playpause-itunes)
    (let ((proc (podcaster--player-process)))
      (signal-process proc 'SIGCONT))))

(provide 'podcaster)

;;; podcaster.el ends here
