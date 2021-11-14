;;; generate.el --- generate vulpea notes for testing purpose -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted>
;; Maintainer: Boris Buliga <d12frosted@d12frosted>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vulpea) (vino))
;;
;; Created: 14 Nov 2021
;;
;; URL: https://github.com/d12frosted/vulpea-test-notes
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; See README.org for more information.
;;
;;; Code:

(require 'vulpea)
(require 'vino)



(defun init-in (dir)
  "Initialize testing environment in DIR."
  (setq org-roam-directory dir
        org-roam-db-location (expand-file-name "org-roam.db" dir)
        vino-db-gc-threshold most-positive-fixnum)
  (when (file-exists-p org-roam-db-location)
    (let ((db (emacsql-sqlite org-roam-db-location)))
      (message "Count of notes: %s"
               (caar (emacsql db "select count(*) from nodes")))
      (when-let* ((res (emacsql db [:select file :from files]))
                  (some-file (caar res)))
        (unless (string-prefix-p "/" some-file)
          (emacsql db [:pragma (= foreign_keys 0)])
          (emacsql db (format "update nodes set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                              (file-name-as-directory org-roam-directory)))
          (emacsql db (format "update files set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                              (file-name-as-directory org-roam-directory)))
          (emacsql db (format "update notes set path = '\"' || '%s' || replace(path, '\"', '') || '\"'"
                              (file-name-as-directory org-roam-directory)))))))
  (vulpea-db-autosync-enable)
  (org-roam-db-autosync-enable))

(defun relativize-file-paths (db-file dir)
  "Convert file path in DB-FILE into relative to DIR."
  (let ((db (emacsql-sqlite db-file)))
    (emacsql db [:pragma (= foreign_keys 0)])
    (emacsql db (format "update nodes set file = replace(file, '%s', '')"
                        (file-name-as-directory dir)))
    (emacsql db (format "update files set file = replace(file, '%s', '')"
                        (file-name-as-directory dir)))
    (emacsql db (format "update notes set path = replace(path, '%s', '')"
                        (file-name-as-directory dir)))))



(defvar random-valid-chars
  (split-string "abcdefghijklmnopqrstuvwxyz" "" t))

(defvar random-valid-chars-length
  (length random-valid-chars))

(defun random-string (length)
  "Return random string of LENGTH."
  (let* (s)
    (while (< (length s) length)
      (setq s (concat s (nth (random random-valid-chars-length)
                             random-valid-chars))))
    s))

(defun random-name ()
  "Generate random name."
  (concat
   (capitalize (random-string (+ 3 (random 6))))
   (when (= (random 2) 1)
     (concat
      " "
      (capitalize (random-string (+ 3 (random 6))))))))



(defun swap (list el1 el2)
  "In LIST swap indices EL1 and EL2 in place."
  (let ((tmp (elt list el1)))
    (setf (elt list el1) (elt list el2))
    (setf (elt list el2) tmp)))

(defun shuffle (list)
  "Shuffle the elements in LIST.

Shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length list))))
           do (let ((j (random (+ i 1))))
                (swap list i j)))
  list)



(defun sync-db (dir)
  "Synchronise `org-roam-db' in DIR."
  (let ((notes-dir (expand-file-name "notes" dir)))
    (init-in notes-dir)
    (relativize-file-paths
     (expand-file-name "org-roam.db" notes-dir) notes-dir)))

(defun generate-data (dir &optional verbose)
  "Generate test data in DIR.

When VERBOSE is non-nil, lots of garbage will be printed.

DIR will be populated like:

.
├── data
│   ├── appellations
│   ├── cellar
│   ├── grapes
│   ├── producers
│   ├── ratings
│   └── regions
└── notes
    └── wine
        ├── appellation
        │   │   20210313121752-anm.org
        │   └── ...
        ├── cellar
        │   │   58324762-802c-11eb-a46e-93bf426c7c38.org
        │   └── ...
        ├── grape
        │   │   20210313121752-caroelit.org
        │   └── ...
        ├── producer
        │   │   20210313121752-ddhfe.org
        │   └── ...
        ├── rating
        │   │   554f9c34-802c-11eb-8904-6fb6e370bbb9.org
        │   └── ...
        └── region
            │   20210313121752-etwjis_btqdxlny.org
            └── ...

DIR/notes can be used as `org-roam-directory'."
  (let ((notes-dir (expand-file-name "notes" dir))
        (producers-file (expand-file-name "data/producers" dir))
        (grapes-file (expand-file-name "data/grapes" dir))
        (regions-file (expand-file-name "data/regions" dir))
        (appellations-file (expand-file-name "data/appellations" dir))
        (cellar-file (expand-file-name "data/cellar" dir))
        (ratings-file (expand-file-name "data/ratings" dir)))
    (make-directory dir 'force)
    (make-directory (expand-file-name "notes" dir) 'force)
    (make-directory (expand-file-name "notes/wine/appellation" dir) 'force)
    (make-directory (expand-file-name "notes/wine/cellar" dir) 'force)
    (make-directory (expand-file-name "notes/wine/grape" dir) 'force)
    (make-directory (expand-file-name "notes/wine/producer" dir) 'force)
    (make-directory (expand-file-name "notes/wine/rating" dir) 'force)
    (make-directory (expand-file-name "notes/wine/region" dir) 'force)
    (make-directory (expand-file-name "data" dir) 'force)
    (setq org-id-uuid-program
          "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
    (setq vino-rating-props
          '((1 . (("score" 20)))
            (4 . (("property_1" 3)
                  ("property_2" 4)
                  ("property_3" 2)
                  ("property_4" 5)
                  ("property_5" 6)))))
    (init-in notes-dir)
    (vino-setup)
    (vino-db-sync)
    (let* ((producers (generate-producers 10 producers-file))
           (grapes (generate-grapes 20 grapes-file))
           (regions (generate-regions 10 regions-file))
           (appellations (generate-appellations 10 appellations-file))
           (resources
            (list (vulpea-note-id (nth (random (length producers)) producers))
                  (vulpea-note-id (nth (random (length producers)) grapes))
                  (vulpea-note-id (nth (random (length producers)) regions))
                  (vulpea-note-id (nth (random (length producers)) appellations))
                  "http://www.agricolaocchipinti.it/it/vinicontrada"
                  "wikipedia"
                  "duckduckgo")))
      (generate-cellar
       cellar-file
       ratings-file
       producers
       grapes
       regions
       appellations
       resources
       verbose))
    (message "Generated %s notes" (caar (org-roam-db-query "select count(*) from nodes")))
    (message "Almost done! Converting paths in org-roam.db into relative...")
    (relativize-file-paths org-roam-db-location
                           org-roam-directory)))

(defun generate-producers (n filename)
  "Generate N producers and store info in FILENAME."
  (let ((producers
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-producer-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'relativize-note
                    producers))
    (save-some-buffers t)
    producers))

(defun generate-grapes (n filename)
  "Generate N grapes and store info in FILENAME."
  (let ((grapes
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-grape-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'relativize-note
                    grapes))
    (save-some-buffers t)
    grapes))

(defun generate-regions (n filename)
  "Generate N regions and store info in FILENAME."
  (let ((regions
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-region-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'relativize-note
                    regions))
    (save-some-buffers t)
    regions))

(defun generate-appellations (n filename)
  "Generate N appellations and store info in FILENAME."
  (let ((appellations
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-appellation-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'relativize-note
                    appellations))
    (save-some-buffers t)
    appellations))

(defun generate-cellar (cellar-file
                        ratings-file
                        producers
                        grapes
                        regions
                        appellations
                        resources
                        verbose)
  "Generate cellar entries and ratings for them.

Cellar entries are stored in CELLAR-FILE and ratings are stored
in RATINGS-FILE.

PRODUCERS, GRAPES, REGIONS, APPELLATIONS and RESOURCES are used
for generation.

When VERBOSE is non-nil, lots of garbage will be printed."
  (let* ((entries-with-ratings
          (seq-map
           (lambda (spec)
             (pcase-let ((`(,carbonation
                            ,sweetness
                            ,colour
                            ,vintage?
                            ,rora?
                            ,grapes-count
                            ,sugar?
                            ,resources-count
                            ,prices-count
                            ,ratings-count)
                          spec))
               (generate-entry
                :producers producers
                :grapes grapes
                :regions regions
                :appellations appellations
                :resources resources
                :verbose verbose
                :carbonation carbonation
                :sweetness sweetness
                :colour colour
                :vintage? vintage?
                :rora? rora?
                :grapes-count grapes-count
                :sugar? sugar?
                :resources-count resources-count
                :prices-count prices-count
                :ratings-count ratings-count)))
           ;; Generate a Cartesian product of all required values.
           (-table-flat
            #'-snoc
            (cl-loop
             for carbonation in vino-carbonation-types
             nconc
             (cl-loop
              for sweetness in (plist-get vino-sweetness-levels carbonation)
              collect
              (list carbonation sweetness)))
            vino-colour-types
            '(nv random)
            '(appellation region)
            '(0 1 2)
            '(na random)
            '(0 1)
            '(0 1)
            '(1 0))))
         (entries (seq-map #'car entries-with-ratings))
         (ratings (seq-mapcat #'cdr entries-with-ratings)))
    (print-to-file
     cellar-file
     (seq-map #'relativize-entry entries))
    (print-to-file
     ratings-file
     (seq-map #'relativize-rating ratings))
    entries))

(cl-defun generate-entry (&key
                          producers
                          grapes
                          regions
                          appellations
                          resources
                          verbose
                          carbonation
                          sweetness
                          colour
                          vintage?
                          rora?
                          grapes-count
                          sugar?
                          resources-count
                          prices-count
                          ratings-count)
  "Generate a cellar entry and associated ratings.

Return a list of (`vino-entry' . (`vino-rating')).

Generation is based on CARBONATION, SWEETNESS, COLOUR, VINTAGE?,
RORA?, GRAPES-COUNT, SUGAR?, RESOURCES-COUNT, PRICES-COUNT,
RATINGS-COUNT.

Producer is randomly selected from PRODUCERS list.

Grapes are selected randomly from GRAPES list based on
GRAPES-COUNT value.

Region or appellation is selected based on RORA? value from
either REGIONS list or APPELLATIONS list.

Resources are randomly selected from RESOURCES list based on
RESOURCES-COUNT value.

When VERBOSE is non-nil, lots of garbage will be printed."
  (let ((t1 (current-time)))
    (when verbose
      (message (string-join
                '("creating cellar for:"
                  "    carbonation:     %s"
                  "    sweetness:       %s"
                  "    colour:          %s"
                  "    vintage:         %s"
                  "    rora:            %s"
                  "    grapes-count:    %s"
                  "    sugar:           %s"
                  "    resources-count: %s"
                  "    prices-count:    %s"
                  "    ratings-count:   %s")
                "\n")
               carbonation
               sweetness
               colour
               vintage?
               rora?
               grapes-count
               sugar?
               resources-count
               prices-count
               ratings-count))
    (let* ((acquired (random 10))
           (consumed (random (+ 1 acquired)))
           (vino-availability-fn (lambda (_) (cons acquired consumed)))
           (entry (make-vino-entry
                   :carbonation carbonation
                   :colour colour
                   :sweetness sweetness
                   :producer (nth (random (length producers)) producers)
                   :name (random-name)
                   :vintage (pcase vintage?
                              (`random (+ 1900 (random 121))))
                   :appellation (pcase rora?
                                  (`appellation (nth (random (length appellations)) appellations)))
                   :region (pcase rora?
                             (`region (nth (random (length regions)) regions)))
                   :grapes (seq-take (shuffle grapes) grapes-count)
                   :alcohol (+ 9.0 (/ (random 500) 100.0))
                   :sugar (pcase sugar?
                            (`random (/ (random 2400) 100.0)))
                   :resources (seq-take (shuffle resources) resources-count)
                   :price (cl-loop
                           repeat prices-count
                           collect (format
                                    "%.2f EUR"
                                    (+ 3.0 (/ (random 10000) 100.0))))
                   :acquired acquired
                   :consumed consumed
                   :rating nil
                   :ratings nil))
           (_ (when verbose (message "  ... entry = %s" entry)))
           (entry-note (vino-entry--create entry))
           (_ (when verbose (message "  ... entry id = %s" (vulpea-note-id entry-note))))
           (ratings (cl-loop
                     repeat ratings-count
                     collect
                     (make-vino-rating
                      :wine entry-note
                      :date (format "%s-%s-%s"
                                    (+ 1990 (random 35))
                                    (+ 1 (random 12))
                                    (+ 1 (random 28)))
                      :version 1
                      :values (list
                               (list "property_1" (random 4) 3)
                               (list "property_2" (random 5) 4)
                               (list "property_3" (random 3) 2)
                               (list "property_4" (random 6) 5)
                               (list "property_5" (random 7) 6)))))
           (ratings-notes (seq-map #'vino-rating--create ratings)))
      (when (> ratings-count 0)
        (setf (vino-entry-rating entry)
              (/ (seq-reduce (lambda (r x) (+ r (vino-rating-total x))) ratings 0)
                 (float (length ratings))))
        (setf (vino-entry-ratings entry)
              ratings-notes))
      (when verbose (message "  ... saving and killing remaining buffers"))
      (save-some-buffers 'ignore)
      (seq-each
       #'kill-buffer
       (seq-filter (lambda (b)
                     (and (buffer-file-name b)
                          (string-suffix-p ".org" (buffer-file-name b))))
                   (buffer-list)))
      (message "  ... entry generated in %s ms"
               (car (time-convert
                     (time-subtract (current-time) t1)
                     1000)))
      (cons entry ratings))))

(defun relativize-note (n0)
  "Make `vulpea-note' N0 relative to `org-roam-directory'."
  (when n0
    (let ((n (copy-vulpea-note n0)))
      (setf (vulpea-note-path n)
            (string-remove-prefix org-roam-directory
                                  (vulpea-note-path n)))
      n)))

(defun relativize-rating (r0)
  "Make `vino-rating' R0 relative to `org-roam-directory'."
  (let ((r (copy-vino-rating r0)))
    (setf (vino-rating-wine r)
          (relativize-note (vino-rating-wine r)))
    r))

(defun relativize-entry (e0)
  "Make `vino-entry' E0 relative to `org-roam-directory'."
  (let ((e (copy-vino-entry e0)))
    (setf (vino-entry-producer e)
          (relativize-note (vino-entry-producer e)))
    (setf (vino-entry-appellation e)
          (relativize-note (vino-entry-appellation e)))
    (setf (vino-entry-region e)
          (relativize-note (vino-entry-region e)))
    (setf (vino-entry-grapes e)
          (seq-map #'relativize-note (vino-entry-grapes e)))
    (setf (vino-entry-ratings e)
          (seq-map #'relativize-note (vino-entry-ratings e)))
    e))



(defun print-to-file (filename data)
  "Print DATA to FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun read-from-file (filename)
  "Read data from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))



(provide 'generate)
;;; generate.el ends here
