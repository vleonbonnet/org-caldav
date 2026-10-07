;;; org-caldav-ical-test.el --- Offline import compatibility tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-caldav)

(defun org-caldav-ical-test--calendar (body &optional timezone)
  (concat "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//org-caldav tests//EN\n"
          timezone body "END:VCALENDAR\n"))

(defun org-caldav-ical-test--convert (body &optional todo timezone)
  (with-temp-buffer
    (insert (org-caldav-ical-test--calendar body timezone))
    (org-caldav-convert-event-or-todo--from-buffer todo)))

(ert-deftest org-caldav-ical-raw-properties-and-attendees ()
  (let* ((org-caldav-my-email "jane@example.com")
         (result (org-caldav-ical-test--convert
                  (concat "BEGIN:VEVENT\nUID:test\nDTSTAMP:20261007T090000Z\n"
                          "DTSTART:20261007T090000Z\nSUMMARY:Hello\\, world\n"
                          "DESCRIPTION:First\\nSecond\nORGANIZER:mailto:boss@example.com\n"
                          "ATTENDEE;CN=\"Doe, Jane\";ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED:mailto:jane@example.com\n"
                          "RRULE:FREQ=DAILY;COUNT=2\nEXDATE:20261008T090000Z\nEND:VEVENT\n")))
         (master (plist-get result :master))
         (attendees (alist-get 'attendee-data master)))
    (should (equal "Hello, world" (alist-get 'summary master)))
    (should (equal "First\n Second" (alist-get 'description master)))
    (should (equal "FREQ=DAILY;COUNT=2" (alist-get 'rrule master)))
    (should (equal '("20261008T090000Z") (plist-get result :exdates)))
    (should (equal "boss@example.com" (plist-get attendees :organizer)))
    (should (equal "ACCEPTED" (plist-get attendees :my-partstat)))))

(ert-deftest org-caldav-ical-all-day-exclusive-end-and-duration ()
  (dolist (ending '("DTEND;VALUE=DATE:20261009\n" "DURATION:P2D\n"))
    (let ((master (plist-get
                   (org-caldav-ical-test--convert
                    (concat "BEGIN:VEVENT\nUID:test\nDTSTAMP:20261007T090000Z\n"
                            "DTSTART;VALUE=DATE:20261007\nSUMMARY:Trip\n"
                            ending "END:VEVENT\n")) :master)))
      (should (equal "7 10 2026" (alist-get 'start-d master)))
      (should (equal "8 10 2026" (alist-get 'end-d master)))
      (should-not (alist-get 'start-t master))
      (should-not (alist-get 'end-t master)))))

(ert-deftest org-caldav-ical-duration-and-local-time ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "TZ" "UTC")
    (let ((master (plist-get
                   (org-caldav-ical-test--convert
                    (concat "BEGIN:VEVENT\nUID:test\nDTSTAMP:20261007T090000Z\n"
                            "DTSTART;TZID=Europe/Paris:20261007T090000\n"
                            "DURATION:PT1H30M\nSUMMARY:Call\nEND:VEVENT\n")) :master)))
      (should (equal "07:00" (alist-get 'start-t master)))
      (should (equal "08:30" (alist-get 'end-t master)))))
  (should (= 14 (decoded-time-day (org-caldav-ical-duration "P2W"))))
  (should-error (org-caldav-ical-duration "P")))

(ert-deftest org-caldav-ical-vtimezone-and-exceptions ()
  (let* ((process-environment (copy-sequence process-environment))
         (timezone (concat "BEGIN:VTIMEZONE\nTZID:Custom/Fixed\nBEGIN:STANDARD\n"
                           "DTSTART:19700101T000000\nTZOFFSETFROM:+0230\n"
                           "TZOFFSETTO:+0230\nEND:STANDARD\nEND:VTIMEZONE\n")))
    (setenv "TZ" "UTC")
    (let* ((result (org-caldav-ical-test--convert
                    (concat "BEGIN:VEVENT\nUID:test\nDTSTAMP:20261007T090000Z\n"
                            "DTSTART;TZID=Custom/Fixed:20261007T090000\nSUMMARY:Call\n"
                            "RRULE:FREQ=DAILY;COUNT=2\nEND:VEVENT\n"
                            "BEGIN:VEVENT\nUID:test\nDTSTAMP:20261007T090000Z\n"
                            "RECURRENCE-ID:20261008T063000Z\nDTSTART:20261008T080000Z\n"
                            "SUMMARY:Moved\nEND:VEVENT\n") nil timezone))
           (master (plist-get result :master)))
      (should (equal "06:30" (alist-get 'start-t master)))
      (should (= 1 (length (plist-get result :exceptions)))))))

(ert-deftest org-caldav-ical-todo-without-start ()
  (let ((master (plist-get
                 (org-caldav-ical-test--convert
                  (concat "BEGIN:VTODO\nUID:todo\nDTSTAMP:20261007T090000Z\n"
                          "DUE;VALUE=DATE:20261009\nSUMMARY:Task\nSTATUS:COMPLETED\n"
                          "PERCENT-COMPLETE:100\nEND:VTODO\n") t) :master)))
    (should (eq 'todo (alist-get 'component-type master)))
    (should (equal "9 10 2026" (alist-get 'due-d master)))
    (should (equal "100" (alist-get 'percent-complete master)))))

(provide 'org-caldav-ical-test)

(ert-deftest org-caldav-ical-legacy-parser-compatibility ()
  (let ((text (concat "BEGIN:VEVENT\nUID:legacy\nDTSTAMP:20261007T090000Z\n"
                      "DTSTART;VALUE=DATE:20261007\nDTEND;VALUE=DATE:20261009\n"
                      "SUMMARY:Trip\nEND:VEVENT\n")))
    (let ((modern (org-caldav-ical-test--convert text))
          (original-fboundp (symbol-function 'fboundp)))
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (symbol)
                   (and (not (memq symbol '(icalendar-parse icalendar-read-date-time)))
                        (funcall original-fboundp symbol)))))
        (let ((legacy (org-caldav-ical-test--convert text)))
          (dolist (key '(start-d end-d start-t end-t summary rrule))
            (should (equal (alist-get key (plist-get modern :master))
                           (alist-get key (plist-get legacy :master))))))))))

(ert-deftest org-caldav-ical-folded-utf8-text ()
  (let ((result (org-caldav-ical-test--convert
                 (concat "BEGIN:VEVENT\nUID:folded\nDTSTAMP:20261007T090000Z\n"
                         "DTSTART:20261007T090000Z\nSUMMARY:Rendez-vous é\n té\n"
                         "END:VEVENT\n"))))
    (should (equal "Rendez-vous été" (alist-get 'summary (plist-get result :master))))))
