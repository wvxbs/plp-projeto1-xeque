;; Gabriel Ferreira - RA: 10442043

(defun parseRow (row)
  (cond
    ((stringp row) (coerce row 'list))
    ((integerp row) (make-list row :initial-element #\.))
    (t (error "Formato de linha inválido"))))

(defun parseBoard (dados)
  (mapcar #'parseRow dados))

(defun getPiece (board r c)
  (and (>= r 0) (< r 8) (>= c 0) (< c 8)
       (nth c (nth r board))))

(defun validPos (r c)
  (and (>= r 0) (< r 8) (>= c 0) (< c 8)))

(defun allCoords ()
  (mapcan (lambda (r)
            (mapcar (lambda (c) (list r c)) '(0 1 2 3 4 5 6 7)))
          '(0 1 2 3 4 5 6 7)))

(defun findWhiteKing (board)
  (find-if (lambda (pos)
             (char= (getPiece board (first pos) (second pos)) #\R))
           (allCoords)))

(defun checkRay (board r c dr dc targets)
  (let ((nextR (+ r dr))
        (nextC (+ c dc)))
    (and (validPos nextR nextC)
         (let ((piece (getPiece board nextR nextC)))
           (cond
             ((char= piece #\.) (checkRay board nextR nextC dr dc targets))
             ((find piece targets :test #'char=) t)
             (t nil))))))

(defun chess (dados)
  (let* ((board (parseBoard dados))
         (kingPos (findWhiteKing board)))
    (if (not kingPos)
        (error "Rei branco não encontrado!")
        (let ((kr (first kingPos))
              (kc (second kingPos)))
          (or
           ;; 1. Ataques de cavalo
           (some (lambda (offset)
                   (let ((r (+ kr (first offset)))
                         (c (+ kc (second offset))))
                     (and (validPos r c) (char= (getPiece board r c) #\c))))
                 '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))

           ;; 2. Ataques de peão
           (some (lambda (c)
                   (let ((r (- kr 1)))
                     (and (validPos r c) (char= (getPiece board r c) #\p))))
                 (list (- kc 1) (+ kc 1)))

           ;; 3. Ataques de rei adjacente
           (some (lambda (offset)
                   (let ((r (+ kr (first offset)))
                         (c (+ kc (second offset))))
                     (and (validPos r c) (char= (getPiece board r c) #\r))))
                 '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))

           ;; 4. Ataques ortogonais (torre e dama)
           (some (lambda (dir)
                   (checkRay board kr kc (first dir) (second dir) '(#\t #\d)))
                 '((-1 0) (1 0) (0 -1) (0 1)))

           ;; 5. Ataques diagonais (bispo e dama)
           (some (lambda (dir)
                   (checkRay board kr kc (first dir) (second dir) '(#\b #\d)))
                 '((-1 -1) (-1 1) (1 -1) (1 1))))))))

;; Exemplo:
;; (chess '("tcbdrbct" "pppppppp" 8 8 8 8 "PPPPPPPP" "TCBDRBCT"))
;; sbcl --load index.lisp --eval "(print (chess '(\"tcbdrbct\" \"pppppppp\" 8 8 8 8 \"PPPPPPPP\" \"TCBDRBCT\")))" --quit