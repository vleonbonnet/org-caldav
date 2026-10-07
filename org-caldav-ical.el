;;; org-caldav-ical.el --- iCalendar compatibility boundary -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the importer's raw property representation while parsing with the
;; Emacs 31 syntax tree.  Raw values matter for RRULE, EXDATE and escaped text.
;; Older Emacs versions retain their parser behind this boundary.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'calendar)
(require 'icalendar)
(require 'icalendar-parser nil t)
(require 'icalendar-utils nil t)
(require 'icalendar-recur nil t)

(defvar icalendar-param-types)
(declare-function icalendar-parse "icalendar-parser" (&optional buffer))
(declare-function icalendar-component-node-p "icalendar-ast" (node))
(declare-function icalendar-ast-node-type "icalendar-ast" (node))
(declare-function icalendar-ast-node-value "icalendar-ast" (node))
(declare-function icalendar-ast-node-children "icalendar-ast" (node))
(declare-function icalendar-ast-node-meta-get "icalendar-ast"
                  (property node &optional dflt force-undefer))
(declare-function icalendar-read-date-time "icalendar-parser" (s))
(declare-function icalendar-recur-tz-set-zone "icalendar-recur"
                  (dt vtimezone &optional nonexistent))
(declare-function icalendar-unfolded-buffer-from-buffer "icalendar-parser" (buffer))

(defun org-caldav-ical--raw-value (node)
  "Return NODE's original property value, before text unescaping."
  (let ((buffer (icalendar-ast-node-meta-get :buffer node))
        (begin (icalendar-ast-node-meta-get :value-begin node))
        (end (icalendar-ast-node-meta-get :value-end node)))
    (unless (and (buffer-live-p buffer) begin end)
      (error "iCalendar property has no source text"))
    (with-current-buffer buffer
      (string-trim-right (buffer-substring-no-properties begin end) "\r?\n"))))

(defun org-caldav-ical--component (node)
  "Adapt syntax NODE to the importer's raw component representation.
The fifth slot retains the syntax tree for VTIMEZONE interpretation."
  (let (properties children)
    (dolist (child (icalendar-ast-node-children node))
      (if (icalendar-component-node-p child)
          (push (org-caldav-ical--component child) children)
        (let (params)
          (dolist (param (icalendar-ast-node-children child))
            (let ((name (or (car (rassq (icalendar-ast-node-type param)
                                        icalendar-param-types))
                            (icalendar-ast-node-meta-get :original-name param))))
              (let ((value (icalendar-ast-node-value param)))
                (when (equal name "VALUE")
                  (setq value (upcase (string-remove-prefix
                                       "icalendar-"
                                       (symbol-name (icalendar-ast-node-value value))))))
                (setq params
                      (append params (list (intern (upcase name)) value))))))
          (push (list (intern (upcase (string-remove-prefix
                                       "icalendar-" (symbol-name
                                                     (icalendar-ast-node-type child)))))
                      params (org-caldav-ical--raw-value child)) properties))))
    (list (intern (upcase (string-remove-prefix
                           "icalendar-" (symbol-name (icalendar-ast-node-type node)))))
          nil (nreverse properties) (nreverse children) node)))

(defun org-caldav-ical-read ()
  "Parse the current buffer, retaining raw values for Org conversion."
  (if (fboundp 'icalendar-parse)
      (let ((node (icalendar-parse)))
        (unless node (error "Cannot parse iCalendar data"))
        (list (org-caldav-ical--component node)))
    ;; Compatibility only: this API is not deprecated before Emacs 31.
    (with-no-warnings (icalendar--read-element nil nil))))

(defun org-caldav-ical-property (event property)
  "Return the first raw PROPERTY value of EVENT."
  (nth 2 (assq property (nth 2 event))))

(defun org-caldav-ical-attributes (event property)
  "Return the parameter plist of EVENT's first PROPERTY."
  (nth 1 (assq property (nth 2 event))))

(defun org-caldav-ical-children (component type)
  "Return COMPONENT's children of TYPE."
  (seq-filter (lambda (child) (eq (car child) type)) (nth 3 component)))

(defun org-caldav-ical-events (calendars)
  "Return all VEVENT components in CALENDARS."
  (mapcan (lambda (cal) (org-caldav-ical-children cal 'VEVENT)) calendars))

(defun org-caldav-ical-timezones (calendars)
  "Return a TZID alist for CALENDARS, using syntax trees on Emacs 31."
  (if (fboundp 'icalendar-parse)
      (mapcar (lambda (zone)
                (cons (org-caldav-ical-property zone 'TZID) (nth 4 zone)))
              (org-caldav-ical-children (car calendars) 'VTIMEZONE))
    (with-no-warnings (icalendar--convert-all-timezones calendars))))

(defun org-caldav-ical-zone (params zones)
  "Resolve PARAMS' TZID in ZONES, falling back to a system zone name."
  (let ((id (plist-get params 'TZID)))
    (and id (or (cdr (assoc id zones)) id))))

(defun org-caldav-ical-decode (text &optional day-shift source-zone result-zone)
  "Decode TEXT, with DAY-SHIFT in SOURCE-ZONE, into RESULT-ZONE.
UTC date-times override SOURCE-ZONE.  DATE values retain their calendar day."
  (when text
    (if (not (fboundp 'icalendar-read-date-time))
        (with-no-warnings
          (icalendar--decode-isodatetime text day-shift source-zone result-zone))
      (let* ((date-only (= (length text) 8))
           (dt (if (fboundp 'icalendar-read-date-time)
                   (icalendar-read-date-time
                    (if date-only (concat text "T000000") text))
                 (with-no-warnings
                   (icalendar--decode-isodatetime text nil source-zone source-zone)))))
      (when day-shift
        (setq dt (decoded-time-add dt (make-decoded-time :day day-shift))))
      (unless (string-suffix-p "Z" text)
        (if (and (consp source-zone) (eq (car source-zone) 'icalendar-vtimezone))
            (icalendar-recur-tz-set-zone dt source-zone)
          (setf (decoded-time-zone dt) source-zone)))
      (if date-only dt (decode-time (encode-time dt) result-zone))))))

(defun org-caldav-ical-date (datetime)
  "Format decoded DATETIME for the importer's space-separated diary dates."
  (when datetime
    (let ((d (decoded-time-day datetime)) (m (decoded-time-month datetime))
          (y (decoded-time-year datetime)))
      (pcase calendar-date-style
        ('european (format "%d %d %d" d m y))
        ('iso (format "%d %d %d" y m d))
        (_ (format "%d %d %d" m d y))))))

(defun org-caldav-ical-time (datetime)
  "Format decoded DATETIME as an HH:MM time."
  (when datetime
    (format "%02d:%02d" (decoded-time-hour datetime) (decoded-time-minute datetime))))

(defun org-caldav-ical-duration (text &optional correction)
  "Parse RFC 5545 duration TEXT to a decoded-time delta.
CORRECTION subtracts one day from a day-only duration for inclusive Org ends."
  (when text
    (unless (string-match
             "\\`\\([+-]?\\)P\\(?:\\([0-9]+\\)W\\|\\([0-9]+\\)D\\)?\\(?:T\\(?:\\([0-9]+\\)H\\)?\\(?:\\([0-9]+\\)M\\)?\\(?:\\([0-9]+\\)S\\)?\\)?\\'"
             text)
      (error "Invalid iCalendar duration: %s" text))
    (let* ((sign (if (equal (match-string 1 text) "-") -1 1))
           (weeks (match-string 2 text)) (days (match-string 3 text))
           (hours (match-string 4 text)) (minutes (match-string 5 text))
           (seconds (match-string 6 text)))
      (unless (or weeks days hours minutes seconds)
        (error "Empty iCalendar duration: %s" text))
      (when (and weeks (or hours minutes seconds))
        (error "Week duration cannot contain time: %s" text))
      (make-decoded-time
       :second (* sign (string-to-number (or seconds "0")))
       :minute (* sign (string-to-number (or minutes "0")))
       :hour (* sign (string-to-number (or hours "0")))
       :day (- (* sign (+ (* 7 (string-to-number (or weeks "0")))
                          (string-to-number (or days "0"))))
               (if (and correction (not (string-match-p "T" text))) 1 0))))))

(defun org-caldav-ical-add (time delta)
  "Add decoded DELTA to TIME and normalize the result in local time."
  (decode-time (encode-time (decoded-time-add time delta))))

(defun org-caldav-ical-split-rule (text)
  "Split raw recurrence TEXT into the importer's symbol/list representation."
  (mapcar (lambda (part)
            (let ((pieces (split-string part "=")))
              (cons (intern (car pieces)) (split-string (or (cadr pieces) "") ","))))
          (and text (split-string text ";" t))))

(defun org-caldav-ical-unfolded-buffer (buffer)
  "Return an unfolded copy of BUFFER using the available parser API."
  (if (fboundp 'icalendar-unfolded-buffer-from-buffer)
      (icalendar-unfolded-buffer-from-buffer buffer)
    (with-no-warnings (icalendar--get-unfolded-buffer buffer))))

(provide 'org-caldav-ical)
;;; org-caldav-ical.el ends here
