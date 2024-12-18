(defun split-string (string separator)
  "Splits a string into a list of substrings based on the given separator."
  (loop with result = nil
        with length = (length string)
        for start = 0 then (1+ end)
        for end = (position separator string :start start)
        while (and end (< start length))
        do (push (subseq string start end) result)
        finally (push (subseq string start) result)
        (return (nreverse result))))

(defun read-csv-file (file-path)
  "Reads a CSV file, parses it, and returns the data as a list of association lists."
  (with-open-file (stream file-path :direction :input)
    (let* ((lines (loop for line = (read-line stream nil nil)
                        while line collect line))
           (headers (split-string (first lines) #\,)) ; Parse headers
           (data-lines (rest lines))) ; Remaining lines as data
      (mapcar (lambda (line)
                (let ((values (split-string line #\,))) ; Parse each line's values
                  (loop for header in headers
                        for value in values
                        collect (cons (intern (string-upcase (string-trim '(#\Space #\Newline #\") header)) :keyword) 
                                      (string-trim '(#\Space #\") value)))))
              data-lines))))

(defun select (file-path &rest filters)
  "Selects data from a CSV file based on optional filters and returns a lambda function for further access."
  (let ((data (read-csv-file file-path)))
    (if (null filters) ; If no filters are provided, return all data
        (lambda () data)
        (let ((filter-pairs
               (loop for (key value) on filters by #'cddr
                     collect (cons key value)))) ; Convert filters into pairs
          (lambda () 
            (remove-if-not (lambda (row)
                             (every (lambda (filter)
                                      (equal (cdr filter) (cdr (assoc (car filter) row))))
                                    filter-pairs)) ; Apply filters to each row
                           data))))))

(defun write-alist-to-csv (file-path alists &optional write-headers)
  "Writes an association list to a CSV file, optionally including headers."
  (with-open-file (stream file-path :direction :output :if-exists :append :if-does-not-exist :create)
    (when write-headers ; If headers are to be written, generate and write them
          (let ((headers (mapcar #'car (first alists)))) 
            (write-line
              (reduce (lambda (a b) (concatenate 'string a "," b))
                      (mapcar #'symbol-name headers)) ; Join headers as CSV line
              stream)))
    (dolist (alist alists) ; Write each alist as a CSV line
      (let ((line
             (reduce (lambda (a b) (concatenate 'string a "," b))
                     (mapcar (lambda (header)
                               (or (cdr (assoc header alist)) "")) ; Handle missing values
                             (mapcar #'car (first alists)))))) 
        (write-line line stream)))))

(defun alist-to-hash-table (alist)
  "Converts an association list to a hash table."
  (let ((hash (make-hash-table :test 'equal))) ; Create an empty hash table
    (dolist (pair alist) ; Iterate over each pair in the alist
      (setf (gethash (car pair) hash) (cdr pair))) ; Add to the hash table
    hash))

(defun print-alist (alist)
  "Prints each key-value pair in the alist in a human-readable format."
  (if (null alist)
      (format t "The alist is empty.~%")
      (dolist (pair alist)
        (format t "~A: ~A~%" (car pair) (cdr pair)))))

(defun hash-table-keys (hash-table)
  "Returns a list of all keys in the hash table."
  (loop for key being the hash-keys of hash-table collect key))

(defun test-filter-data ()
  (let ((article-with-id-1 (select "scientific-articles.csv" :ID "1"))
        (authors-with-speciality-123 (select "scientific-articles.csv" :CODE "123")))
    (format t "Article with id 1:~%")
    (let ((article (funcall article-with-id-1)))
      (assert (not (null article)) () "No article with ID 1 found")
      (print-alist article))
    (format t "~%Authors with speciality 123:~%")
    (let ((authors (funcall authors-with-speciality-123)))
      (assert (not (null authors)) () "No authors with speciality 123 found")
      (print-alist authors))))

(defun test-select-data ()
  (let ((all-articles (select "scientific-articles.csv"))
        (all-specialities (select "specialities.csv")))
    (format t "All articles:~%")
    (let ((articles (funcall all-articles)))
      (assert (not (null articles)) () "No articles found")
      (print-alist articles))
    (format t "~%All specialities:~%")
    (let ((specialities (funcall all-specialities)))
      (assert (not (null specialities)) () "No specialities found")
      (print-alist specialities))))

(defun test-alist-to-hash ()
  (let* ((all-specialities (select "specialities.csv"))
         (speciality (first (funcall all-specialities)))
         (hash (alist-to-hash-table speciality)))
    (format t "Speciality as hash table:~%")
    (assert hash () "Failed to convert alist to hash table")
    (maphash (lambda (key value)
               (format t "~a => ~a~%" key value))
             hash)
    (format t "~%Keys in hash table:~%")
    (let ((keys (hash-table-keys hash)))
      (assert (not (null keys)) () "No keys found in hash table")
      (dolist (key keys)
        (format t "~a~%" key)))))

(defun test-add-to-file ()
  (let ((data '(((:ID . "5") (:CODE . "113") (:NAME . "Прикладна математика")))))
    (write-alist-to-csv "specialities.csv" data))
  (with-open-file (stream "specialities.csv" :direction :input)
    (format t "Updated structure of specialities.csv~%")
    (let ((lines (loop for line = (read-line stream nil)
                       while line collect line)))
      (assert (some (lambda (line) (search "Прикладна математика" line)) lines)
              () "Failed to add new entry to file")
      (dolist (line lines)
        (format t "~a~%" line)))))

(defun run-all-tests ()
  (test-select-data)
  (test-filter-data)
  (test-alist-to-hash)
  (test-add-to-file))

(run-all-tests)
