(ql:quickload '(:cue-parser
                :cl-ppcre
                :lparallel))
(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defun get-file-of-type (dir type)
  "Returns the file in a directory that has the given type. Errors if there are more than one of the given type."
  (let ((searched (remove-if #'(lambda (f) (not (string-equal
                                                 (pathname-type f)
                                                 type)))
                             (uiop:directory-files dir))))
    (if (equal (length searched) 1)
        (first searched)
        (error (format nil "get-file-of-type failed on ~d with ~d!" dir type)))))

(defun get-album-cover (dir)
  "Returns the best found album cover in a directory."
  (let ((images (remove-if #'(lambda (f) (not (member (pathname-type f)
                                                      '("jpg" "jpeg" "png")
                                                      :test #'string-equal)))
                           (uiop:directory-files dir))))
    (or (when (equal (length images) 1) (first images))
        (find-if #'(lambda (f) (string-equal (pathname-name f) "front")) images)
        (find-if #'(lambda (f) (string-equal (pathname-name f) "cover")) images))))

(defun get-album-filename (cue)
  (let ((cue-string (uiop:read-file-string cue)))
    (format nil "~d - ~d/"
            ;; chances are first 4-digit number is the date
            (first
             (ppcre:all-matches-as-strings "[0-9]{4}" cue-string))
            (let ((title-scan (first (ppcre:all-matches-as-strings "TITLE \".+\"" cue-string))))
              (subseq
               title-scan
               7 (1- (length title-scan)))))))

(defun parse-album (album-dir new-base &optional (audio-file-type "flac"))
  "Given an album directory (one cue, one flac, and a cover), parse into a new directory with flacs for each song."
  (let* ((cue (get-file-of-type album-dir "cue"))
         (audio-file (get-file-of-type album-dir audio-file-type))
         (image (get-album-cover album-dir))
         (new-dir (merge-pathnames (get-album-filename cue)
                                   new-base)))
    (ensure-directories-exist new-dir)
    (uiop:run-program (format nil "shnsplit -f \"~d\" -o flac -t \"%n - %t\" -d \"~d\" \"~d\"" cue new-dir audio-file))
    ;; cuetag is finnicky so we use a format directive to grab each outputted file individually, e.g. "01 - song" "02 - other song"
    (uiop:run-program (format nil "cuetag \"~d\" ~{\"~a\"~^ ~}" cue (uiop:directory-files new-dir)))
    (when image (uiop:run-program (format nil "metaflac --import-picture-from=\"~d\" \"~d\"" image (merge-pathnames "*" new-dir))))))

(defun parse-albums (base new &optional (audio-file-type "flac"))
  "Given a directory containing albums (ie Band/Album/Music.flac), converts .cue and .flac albums into .flac files for each song."
  (lparallel:pmapc (lambda (f) (parse-album f new audio-file-type))
                   (uiop:subdirectories base)))
