SELECT distinct(log.text)
FROM log_word_map lwm1, log_word_map lwm2
JOIN log ON log.rowid = lwm1.log_id
WHERE lwm1.log_id = lwm2.log_id
    AND lwm1.word = 'skeleton'
    AND lwm2.word = 'lobster';
