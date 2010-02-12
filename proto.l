(macro (def_res name *body)
       `(define (,name input dic mood) ,@(a2l body)))

(macro (new_responder_list list *init_lis)
       `(= ,list (newarray ,@(map (lambda (ran_lis)
				`(new_responder ,@ran_lis)) (a2l init_lis)))))

(macro (dictionary_load path *block)
       `(open ,path
	      (block (f)
		(= array [])
		(f.each (block ,@(a2l block)))
		array)))

(def_res what_response (+ input "ってなに?"))
			  
(def (select_random ary)
     (aref ary (rand ary.size)))

(def_res random_response
	 (select_random dic.random))

(def_res pattern_response
	 (dic.pattern.each (block (ptn_item)
			     (when (= m (ptn_item.match input))
			       (= resp (ptn_item.choice mood))
			       (if resp.nil? next)
			       (return (resp.gsub /%match%/ m.to_s)))))
	 (select_random dic.random))

(def (new_responder name response)
     (list name response))

(class Unmo
	  (def (initialize name dics responder)
	       (sets @name name @dics dics @responder responder
		     @emotion (Emotion.new @dics)))

	  

	  (def (dialogue input)
	       (define (chois)
		 (case (rand 100)
		       (0..59 (return 2)) 
		       (60..89 (return 0))
		       (else (return 1))))

	       (@emotion.update input)

	       (sets respons (aref @responder (chois.call))
		     @respon_name (car respons))

	       (mcall call (cadr respons)  input @dics @emotion.mood))

	  (def (mood) @emotion.mood)

	  (attr_reader :name :respon_name))
(class PatternItem
  (asm "SEPARATOR = /^((-?\d+)##)?(.*)$/")

  (def (initialize pattern phrases)
    SEPARATOR=~pattern
    (asm "@modify, @pattern = $2.to_i, $3")

    @phrases=[]
    (= spl (phrases.split "|"))                       
    (spl.each 
     (block (phrase)
	    SEPARATOR=~phrase
	    (@phrases.push (new_hash ("need" $2.to_i) ("phrase" $3))))))

  

  (def (match str)
    (return (str.match @pattern)))


  (def (choice mood)
       choices=[]
       (@phrases.each 
	(block (p)
	       (if (suitable? (aref p "need") mood)
		   (choices.push (aref p "phrase")))))
       (return (if (choices.empty?) nil (select_random choices))))
  

  (def (suitable? need mood)
       (if (== need 0) (return true))
       (if (> need 0)
	   (return mood>need)
	 (return mood<need)))

  (attr_reader :modify :pattern :phrases))
(class Dics
	  (def (initialize)
	       (sets @random  [] @pattern [])

	       (dictionary_load "dics/random.txt"
				(line)
				(line.chomp!)
				(if (line.empty?)
				    next)
				(@random.push line))

	       (dictionary_load "dics/pattern.txt"
				(line)
				  (asm "pattern, phrases = line.chomp.split('\t')")
				  (if (or pattern.nil? phrases.nil?)
				      next)
				(@pattern.push (PatternItem.new pattern phrases))))
	  (attr_reader :random :pattern))

(class Emotion
     (sets  MOOD_MIN -15 MOOD_MAX  15 MOOD_RECOVERY 0.5)

     (def (initialize dic)
	  (sets @dictionary dic @mood 0))

     (def (update input)
	  (@dictionary.pattern.each 
	   (block (ptn_item)
		  (when (ptn_item.match input)
		      (adjust_mood ptn_item.modify)
		      break)))
	  (cond ((< @mood 0)
		 (= @mood (+ @mood MOOD_RECOVERY)))
		((> @mood 0)
		 (= @mood (- @mood MOOD_RECOVERY)))))

     (def (adjust_mood val)
	  (= @mood (+ @mood val))
	  (cond ((> @mood MOOD_MAX)
		 (= @mood MOOD_MAX))
		((< @mood MOOD_MIN)
		 (= @mood MOOD_MIN))))
     (attr_reader :mood))
     

(= dics Dics.new)
(new_responder_list responder_list
		    ("random" random_response)
		    ("what" what_response)
		    ("pattern" pattern_response))


		     
(def (prompt unmo)
     (+ unmo.name ":" unmo.respon_name ">"))

(puts "Unmo System prototype : proto")
(= proto (Unmo.new "proto" dics responder_list))

(loop (block ()
	(print "> ")
	(= input STDIN.gets)
	(input.chomp!)
	(if (== input "")
	    break)
	(= res (proto.dialogue input))
	(puts (+ (prompt proto) res))))
