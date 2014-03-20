;; Relational Turing Machine code
;;
;; Translated to miniKanren, with no optimizations or significant
;; changes, from Igor Wiedler's core.logic code:
;;
;; https://gist.github.com/igorw/9654549
;;
;; Links to Igor:
;;
;; https://igor.io/
;; https://github.com/igorw
;; https://twitter.com/igorwhiletrue

(load "mk.scm")
(load "test-check.scm")

(define emptyo
  (lambda (x)
    (== '() x)))

(define conso
  (lambda (a d p)
    (== `(,a . ,d) p)))

(define firsto
  (lambda (p a)
    (fresh (d)
      (== `(,a . ,d) p))))

(define resto
  (lambda (p d)
    (fresh (a)
      (== `(,a . ,d) p))))


(define membero
  (lambda (x ls)
    (fresh (head tail)
      (== `(,head . ,tail) ls)
      (conde
        ((== head x))
        ((=/= head x)
         (membero x tail))))))

(define not-membero
  (lambda (x ls)
    (conde
      ((== '() ls))
      ((fresh (head tail)
         (== `(,head . ,tail) ls)
         (=/= head x)
         (not-membero x tail))))))

(define turing-ruleo
  (lambda (rules state-in tape-in-val rule-out)
    (fresh (rule rules-rest write-val dir state-new)
      (conso rule rules-rest rules)
      (conde
        ((== `(,state-in ,tape-in-val ,write-val ,dir ,state-new) rule)
         (== `(,write-val ,dir ,state-new) rule-out))
        ((turing-ruleo rules-rest state-in tape-in-val rule-out))))))

(define turing-directiono
  (lambda (dir tape-in tape-out)
    (fresh (tape-in-l tape-in-val tape-in-r)
      (== `(,tape-in-l ,tape-in-val ,tape-in-r) tape-in)
      (conde
        ((== 'r dir)
         (fresh (tape-new-l tape-new-val tape-new-r)
           (conde
             ((== '_ tape-in-val)
              (emptyo tape-in-l)
              (emptyo tape-new-l))
             ((conso tape-in-val tape-in-l tape-new-l)))
           (conde
             ((firsto tape-in-r tape-new-val)
              (resto tape-in-r tape-new-r))
             ((== '_ tape-new-val)
              (emptyo tape-in-r)
              (emptyo tape-new-r)))
           (== `(,tape-new-l ,tape-new-val ,tape-new-r) tape-out)))
        ((== 'l dir)
         (fresh (tape-new-l tape-new-val tape-new-r)
           (conde
             ((== '_ tape-in-val)
              (emptyo tape-in-r)
              (emptyo tape-new-r))
             ((conso tape-in-val tape-in-r tape-new-r)))
           (conde
             ((firsto tape-in-l tape-new-val)
              (resto tape-in-l tape-new-l))
             ((== '_ tape-new-val)
              (emptyo tape-in-l)
              (emptyo tape-new-l)))
           (== `(,tape-new-l ,tape-new-val ,tape-new-r) tape-out)))
        ((== 'n dir)
         (== tape-out tape-in))))))

;; Comment adapted from Igor's code:

;; note: tape is a 3-tuple ((tape-left...) tape-val (tape-right...))
;; tape-left and tape-right are lists
;; tape-left is inverted, so a value of ((0 1) 0 (1 1)) actually
;; represents the tape (1 0 0 1 1)

(define turing-machineo
  (lambda (rules accept-states state-in tape-in state-out tape-out)
    (fresh (tape-in-l tape-in-val tape-in-r
            write-val dir state-new
            tape-new)
      (conde
        ((== state-out state-in)
         (== tape-out tape-in)
         (membero state-in accept-states))
        ((== `(,tape-in-l ,tape-in-val ,tape-in-r) tape-in)
         (not-membero state-in accept-states)
         (turing-ruleo rules state-in tape-in-val `(,write-val ,dir ,state-new))
         (turing-directiono dir `(,tape-in-l ,write-val ,tape-in-r) tape-new)
         (turing-machineo rules accept-states state-new tape-new state-out tape-out))))))

(define hello-rules
  '((1 _ o l 2)
    (2 _ l l 3)
    (3 _ l l 42)))

;; Times using Vicare Scheme on my Macbook Pro (2.8 GHz Intel Core i7,
;; 16 GB RAM)

;; 0 ms elapsed cpu time, including 0 ms collecting
;; 0 ms elapsed real time, including 0 ms collecting
;; 49 008 bytes allocated
(test "tm-1"
  (run 1 (q)
    (turing-machineo hello-rules '(42) 1 '(() _ ()) 42 '(() _ (l l o))))
  '(_.0))

;; 63 collections
;; 2709 ms elapsed cpu time, including 843 ms collecting
;; 2708 ms elapsed real time, including 844 ms collecting
;; 532 927 280 bytes allocated
(test "tm-2"
  (run 1 (q)
    (turing-machineo q '(42) 1 '(() _ ()) 42 '(() _ (l l o))))
  '((((1 _ o l _.0)
      (_.0 _ l l _.1)
      (_.1 _ l l 42)
      . _.2)
     (=/= ((_.0 42)) ((_.1 42))))))

;; Let's get a couple more answers
;;
;; 196 collections
;; 10 666 ms elapsed cpu time, including 3884 ms collecting
;; 10 668 ms elapsed real time, including 3885 ms collecting
;; 1 639 446 432 bytes allocated
(test "tm-2b"
  (run 3 (q)
    (turing-machineo q '(42) 1 '(() _ ()) 42 '(() _ (l l o))))
  '((((1 _ o l _.0)
      (_.0 _ l l _.1)
      (_.1 _ l l 42)
      . _.2)
     (=/= ((_.0 42)) ((_.1 42))))
    (((1 _ o l _.0)
      (_.0 _ l l _.0)
      (_.0 _ _ n 42)
      . _.1)
     (=/= ((_.0 42))))
    (((1 _ o l _.0)
      (_.0 _ l l _.0)
      _.1
      (_.0 _ _ n 42)
      . _.2)
     (=/= ((_.0 42))))))
