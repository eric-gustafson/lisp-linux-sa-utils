(in-package #:lsatest)

(defvar tseq (list 1 2 3 3 2 3 3 1 2 3 3 4 4 2 3 3 1 2))

(defvar tseq-result
  `((1 (2 (3 3))
       (2 (3 3)))
    (1 (2 (3 3 (4 4)))
       (2 (3 3)))
    (1 (2))
    )
  )

(fiasco:deftest seq2tree ()
  (let ((tree-from-seq (seq->tree tseq)))
    (fiasco:is
     (equalp tree-from-seq tseq-result))
    )
  )
