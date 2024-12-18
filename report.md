<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Рибалка Денис Віталійович КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.

5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці,
геш-таблиці у асоціативні списки,
асоціативні списки у геш-таблиці.

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
## Варіант 9
| База даних             | Тип записів          |
|------------------------|----------------------|
| Наукові статті         | Асоціативний список  |
## Лістинг реалізації завдання
```lisp
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
                                      (string-trim '(#\Space #\") value))))
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
        (authors-with-speciality-123 (select "specialities.csv" :Code "123")))
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
```

### Вміст тестових файлів
scientific-articles.csv
```csv
ID, Article_Name,Author,Code
1,"Аналіз алгоритмів",Іваненко І.,2022,122
2,"Проектування систем",Петренко П.;Сидоренко С.,2023,123
3,"Моделювання процесів",Ковальчук К.,2021,124
```

specialities.csv
```csv
ID,Code,Name
1,121,Інженерія програмного забезпечення
2,122,Комп'ютерні науки
3,123,Компʼютерна інженерія
4,124,Системний аналіз5,113,Прикладна математика
```

### Тестові набори та утиліти
```lisp
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
        (authors-with-speciality-123 (select "specialities.csv" :Code "123")))
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
```
### Тестування
```
All articles:
(ID . 1): ((ARTICLE_NAME . Аналіз алгоритмів) (AUTHOR . Іваненко І.)
           (CODE . 2022))
(ID . 2): ((ARTICLE_NAME . Проектування систем)
           (AUTHOR . Петренко П.;Сидоренко С.) (CODE . 2023))
(ID . 3): ((ARTICLE_NAME . Моделювання процесів) (AUTHOR . Ковальчук К.)
           (CODE . 2021))

All specialities:
(ID . 1): ((CODE . 121) (NAME . Інженерія програмного забезпечення))
(ID . 2): ((CODE . 122) (NAME . Комп'ютерні науки))
(ID . 3): ((CODE . 123) (NAME . Компʼютерна інженерія))
(ID . 4): ((CODE . 124) (NAME . Системний аналіз))
Article with id 1:
(ID . 1): ((ARTICLE_NAME . Аналіз алгоритмів) (AUTHOR . Іваненко І.)
           (CODE . 2022))

Authors with speciality 123:
(ID . 3): ((CODE . 123) (NAME . Компʼютерна інженерія))
Speciality as hash table:
ID => 1
CODE => 121
NAME => Інженерія програмного забезпечення

Keys in hash table:
ID
CODE
NAME

Updated structure of specialities.csv
ID,Code,Name
1,121,Інженерія програмного забезпечення
2,122,Комп'ютерні науки
3,123,Компʼютерна інженерія
4,124,Системний аналіз5,113,Прикладна математика
```