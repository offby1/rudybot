#lang racket

(provide (struct-out utterance))
(struct utterance (timestamp speaker target text) #:prefab)
