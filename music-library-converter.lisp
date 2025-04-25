(ql:quickload '(:lparallel))
(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defun relative-dir-func (base new)
  "Returns a function that relativizes a dir off of a base, and returns its position on a new dir"
  (let ((bl (length (pathname-directory base))))
    (lambda (dir)
      (merge-pathnames
       (make-pathname :directory (cons :relative (nthcdr bl (pathname-directory dir))))
       new))))

(defun relative-file-func (base new)
  "Returns a function that relativizes a file off of a base, and returns its position on a new dir"
  (let ((bl (length (pathname-directory base))))
    (lambda (file)
      (merge-pathnames
       (make-pathname :directory (cons :relative (nthcdr bl (pathname-directory file))) :defaults file)
       new))))

(defun convertable-p (file)
  "Checks if a file is convertable."
  (member (pathname-type file) '("flac" "mp3" "m4a") :test #'string-equal))


(defun transcode-file-func (base new)
  "Returns a function that converts a directory, if suitable, to a set of files for transcoding."
  (let ((relativizer (relative-file-func base new)))
    (lambda (dir) (when (every #'convertable-p
                               (uiop:directory-files dir))
                    (mapcar (lambda (f)
                              ;; TODO: switch to .ogg once possible
                              (list f (make-pathname :type "mp3" :defaults (funcall relativizer f))))
                            (uiop:directory-files dir))))))

(defun collect-transcoding-files (base new)
  "Returns a list of all files and their destinations, for transcoding"
  (let ((col nil)
        (func (transcode-file-func base new)))
    (uiop:collect-sub*directories base (constantly t) (constantly t) (lambda (f) (push (funcall func f) col)))
    (apply #'nconc col)))

(defun transcodable-files-count (base)
  (let ((col nil))
    (uiop:collect-sub*directories base (constantly t) (constantly t) (lambda (dir) (when (every #'convertable-p
                                                                                                (uiop:directory-files dir))
                                                                                     (push (uiop:directory-files dir) col))))
    (setf col (apply #'nconc col))
    (length col)))

(defun transcode-dir (base new)
  "The actual tool. Use this function to convert all the files in one directory to another."
  (let ((files (collect-transcoding-files base new)))
    (mapc (lambda (fileset)
            (ensure-directories-exist (second fileset)))
          files)
    (lparallel:pmapc (lambda (fileset)
                       (handler-case (uiop:run-program (format nil "ffmpeg -i \"~d\" -ab 320k \"~d\"" (first fileset) (second fileset)))
                         (error (c)
                           (format t "Failed on ~s!~&" (first fileset))
                           c)))
                     files))
  (format t "Success rate: ~d out of ~d~&" (transcodable-files-count new) (transcodable-files-count base)))

;; run (transcode-dir #P"/source/dir/here/" #P"/end/dir/here/")
